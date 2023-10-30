; Copied from https://github.com/igrishaev/teleward


(ns tg-bot-api.telegram
  "
  Telegram HTTP API.
  "
  (:require

    [cheshire.core :as json]

    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.spec.alpha :as spec]

    [org.httpkit.client :as http]


    ))


(defn filter-params
  "
  Filter out nil values from a map.
  "
  [params]
  (persistent!
   (reduce-kv
    (fn [result k v]
      (if (some? v)
        (assoc! result k v)
        result))
    (transient {})
    params)))


(defn encode-params
  "
  JSON-encode complex values of a map.
  "
  [params]
  (persistent!
   (reduce-kv
    (fn [result k v]
      (if (coll? v)
        (assoc! result k (json/generate-string v))
        (assoc! result k v)))
    (transient {})
    params)))


(defn file->name
  [file]
  (clojure.string/replace (str file) #"/" ""))


(defn ->multipart
  [params]
  (let [params (map filter-params (map second params))
                    ;params (filter-params params)
                    ]

                (into
                  (for [[key value] (first params)]
                      (try
                        {:name (name key)
                         :content (json/generate-string value)}
                        (catch Exception e
                          {:name (name key)
                           :content value
                           :filename (file->name value)
                           })))
                  (for [[key value] (second params)]
                      {:name (name key)
                       :content (str value)}))))


(defn api-request
  [{:keys [token
           user-agent
           timeout
           keepalive
           test-server
           local-server]}

   api-method http-method params]

  (let [params
        (filter-params params)



        url
        (if test-server
          (format "https://api.telegram.org/bot%s/test/%s"
                  token (name api-method))
          (format "https://api.telegram.org/bot%s/%s"
                  token (name api-method)))

        url
        (if local-server
          (format (str local-server "/bot%s/%s") token (name api-method))
          url)


        request
        {:url url

         :method http-method


         :as :stream}

        request
        (cond-> request

          user-agent
          (assoc :user-agent user-agent)

          timeout
          (assoc :timeout timeout)

          keepalive
          (assoc :keepalive keepalive))


        request
        (cond-> request

          ;; for GET, complex values must be JSON-encoded
          (= :get http-method)
          (assoc :query-params (encode-params params))

          (= :post http-method)
          (->
           (assoc-in [:headers "content-type"] "application/json")
           (assoc :body (json/generate-string params)))

          (= :post-multipart http-method)
          (->
            (assoc-in [:headers "content-type"]   "multipart/form-data"
                                                #_"application/json")
            (assoc :method :post)
            (assoc :multipart
              (->multipart params))))

        {:keys [error status body headers]}
        @(http/request request)]

    (if error
      (throw (ex-info (format "Telegram HTTP error: %s" (ex-message error))
                      {:api-method api-method
                       :api-params params}
                      error))

      (let [{:keys [content-type]}
            headers

            json?
            (some-> content-type
                    (str/starts-with? "application/json"))

            ;; parse JSON manually as Http Kit cannot
            body-json
            (if json?
              (-> body io/reader (json/decode-stream keyword))
              (throw (ex-info (format "Telegram response was not JSON: %s" content-type)
                              {:http-status status
                               :http-method http-method
                               :http-headers headers
                               :api-method api-method
                               :api-params params})))

            {:keys [ok
                    result
                    error_code
                    description]}
            body-json]

        (if ok
          result
          (throw (ex-info (format "Telegram API error: %s %s %s"
                                  error_code api-method description)
                          {:http-status status
                           :http-method http-method
                           :api-method api-method
                           :api-params params
                           :error-code error_code
                           :error description})))))))


(defn get-me
  "https://core.telegram.org/bots/api#getme"
  [config]
  (api-request config :getMe :get nil))


(defn get-updates
  "https://core.telegram.org/bots/api#getupdates"

  ([config]
   (get-updates config nil))

  ([config {:keys [limit
                   offset
                   timeout
                   allowed-updates]}]

   (api-request config
                :getUpdates
                :get
                {:limit limit
                 :offset offset
                 :timeout timeout
                 :allowed_updates allowed-updates})))


(defn ban-user
  "https://core.telegram.org/bots/api#banchatmember"

  ([config chat-id user-id]
   (ban-user config chat-id user-id nil))

  ([config chat-id user-id {:keys [until-date
                                   revoke-messages]}]

   (api-request config
                :banChatMember
                :post
                {:chat_id chat-id
                 :user_id user-id
                 :until_date until-date
                 :revoke_messages revoke-messages})))

(defn send-message
  "https://core.telegram.org/bots/api#sendmessage"

  ([config chat-id text]
   (send-message config chat-id text nil))

  ([config chat-id text {:keys [parse-mode
                                entities
                                disable-web-page-preview
                                disable-notification
                                protect-content
                                reply-to-message-id
                                allow-sending-without-reply
                                reply-markup]}]

   (api-request config
                :sendMessage
                :post
                {:chat_id chat-id
                 :text text
                 :parse_mode parse-mode
                 :entities entities
                 :disable_web_page_preview disable-web-page-preview
                 :disable_notification disable-notification
                 :protect_content protect-content
                 :reply_to_message_id reply-to-message-id
                 :allow_sending_without_reply allow-sending-without-reply
                 :reply_markup reply-markup})))

(defn send-photo
  "https://core.telegram.org/bots/api#sendphoto"
  ([config chat-id photo]
   (send-photo config chat-id photo nil))

  ([config chat-id photo {:keys [message-thread-id
                                 caption
                                 parse-mode
                                 caption-entities
                                 has-spoiler
                                 disable-notification
                                 protect-content
                                 reply-to-message-id
                                 allow-sending-without-reply
                                 reply-markup]}]
   (api-request
     config
     :sendPhoto
     :post-multipart
     [{:chat_id chat-id
       :photo photo}
      {:message_thread_id message-thread-id
       :caption caption
       :parse_mode parse-mode
       :caption_entities caption-entities
       :has_spoiler has-spoiler
       :disable_notification disable-notification
       :protect_content protect-content
       :reply_to_message_id reply-to-message-id
       :allow_sending_without_reply allow-sending-without-reply
       :reply_markup reply-markup}])))


(defn send-document
  "https://core.telegram.org/bots/api#senddocument"
  ([config chat-id document]
   (send-document config chat-id document nil))

  ([config chat-id document {:keys [message-thread-id
                                    thumbnail
                                    caption
                                    parse-mode
                                    caption-entities
                                    disable-content-type-detection
                                    disable-notification
                                    protect-content
                                    reply-to-message-id
                                    allow-sending-without-reply
                                    reply-markup]}]
   (api-request
     config
     :sendDocument
     :post-multipart
     [{:chat_id chat-id
       :document document
       ;:thumbnail thumbnail
       }
      {:message_thread_id message-thread-id
       :caption caption
       :parse_mode parse-mode
       :caption_entities caption-entities
       :disable_content_type_detection disable-content-type-detection
       :disable_notification disable-notification
       :protect_content protect-content
       :reply_to_message_id reply-to-message-id
       :allow_sending_without_reply allow-sending-without-reply
       :reply_markup reply-markup}])))


(defn input-stream?
  [x]
  (= (type x) java.io.BufferedInputStream))
(defn file?
  [x]
  (= (type x) java.io.File))


(spec/def :media/type #{"photo" "video" "audio" "document"})

(spec/def ::url
  (spec/and
       string?
       (partial re-matches #"(?i)^http(s?)://.*")))

(spec/def ::file
  (spec/or :f file? :is input-stream?))

(spec/def :media/media
  (spec/or

    :url ::url

    :file ::file))

(spec/def ::media
  (spec/coll-of
    (spec/keys
      :req-un
      [:media/type
       :media/media]
      :opt-un
      [::caption
       ::parse_mode
       ::caption_entities
       ::has_spoiler])
    :min 2
    :max 10))


(defn media->multipart-ready
  [media]

  (let [files
        (filter (fn [x]
                  (or
                    (= (type (:media x)) java.io.BufferedInputStream)
                    (= (type (:media x)) java.io.File)))
          media)

        urls
        (filter (fn [x]
                  (spec/valid? ::url (:media x)))
          media)


        files'
        (map
          (fn [x] (assoc x :media
                    (format "attach://%s" (file->name (:media x)))))
          files)

        files''
        (map
          (fn [x]
            {(keyword (str (:media x))) (:media x)})
          files)]

    (into
      {:media (vec (concat files' urls))}
      files'')))


(comment
  (do
    (media->multipart-ready
    [{:type "photo"
      :media (io/file "https://www.tema.ru/i/home=page.jpg")}
     {:type "photo"
      :media (io/input-stream "https://www.tema.ru/i/home=page.jpg")}
     {:type "photo"
      :media "https://www.tema.ru/i/home=page.jpg"}])))


(defn send-media-group
  "https://core.telegram.org/bots/api#sendmediagroup"

  ([config chat-id media]
   {:pre [(spec/valid? ::media media)]}
   (send-media-group config chat-id media nil))
  ([config chat-id media {:keys [message-thread-id
                                 disable-notification
                                 protect-content
                                 reply-to-message-id
                                 allow-sending-without-reply]}]

   (api-request
     config
     :sendMediaGroup
     :post-multipart
     [(into
        {:chat_id chat-id}
        (media->multipart-ready media))
      {:message_thread_id message-thread-id
       :disable_notification disable-notification
       :protect_content protect-content
       :reply_to_message_id reply-to-message-id
       :allow_sending_without_reply allow-sending-without-reply}])))


(spec/fdef send-media-group
  :args
  (spec/cat :media ::media))


(defn edit-message-text
  "https://core.telegram.org/bots/api#editmessagetext"
  ([config chat-id message-id text]
   (edit-message-text config chat-id message-id text nil))
  ([config chat-id message-id text {:keys [parse-mode
                                           entities
                                           disable-web-page-preview
                                           reply-markup]}]
   (api-request
     config
     :editMessageText
     :post
     {:chat_id chat-id
      :message_id message-id
      :text text
      :parse_mode parse-mode
      :entities entities
      :disable_web_page_preview disable-web-page-preview
      :reply_markup reply-markup})))


(defn edit-message-caption
  "https://core.telegram.org/bots/api#editmessagecaption"
  ([config chat-id message-id caption]
   (edit-message-caption config chat-id message-id caption nil))

  ([config chat-id message-id caption {:keys [parse-mode
                                              caption-entities
                                              reply-markup]}]
   (api-request
     config
     :editMessageCaption
     :post
     {:chat_id chat-id
      :message_id message-id
      :caption caption
      :parse_mode parse-mode
      :caption_entities caption-entities
      :reply_markup reply-markup})
   )
  )


(defn edit-message-photo
  "https://core.telegram.org/bots/api#editmessagemedia"
  ([config chat-id message-id photo]
   (edit-message-photo config chat-id message-id photo nil))
  ([config chat-id message-id photo {:keys [reply-markup]}]
     (api-request
       config
       :editMessageMedia
       :post-multipart
       [{:photo photo
         :media
         {:type :photo
          :media "attach://photo"}}
        {:chat_id chat-id
         :message_id message-id
         :reply_markup reply-markup}])))


(defn delete-message
  "https://core.telegram.org/bots/api#deletemessage"
  [config chat-id message-id]
  (api-request config
                :deleteMessage
                :post
                {:chat_id chat-id
                 :message_id message-id}))


(def chat-permission-types
  #{:can_send_messages
    :can_send_media_messages
    :can_send_polls
    :can_send_other_messages
    :can_add_web_page_previews
    :can_change_info
    :can_invite_users
    :can_pin_messages})

(def chat-permissions-on
  (zipmap chat-permission-types (repeat true)))

(def chat-permissions-off
  (zipmap chat-permission-types (repeat false)))


(defn restrict-user

  ([config chat-id user-id permissions]
   (restrict-user config chat-id user-id permissions nil))

  ([config chat-id user-id permissions {:keys [until_date]}]
   (api-request config
                :restrictChatMember
                :post
                {:chat_id chat-id
                 :user_id user-id
                 :permissions permissions
                 :until_date until_date})))


(defn answer-callback-query
  "https://core.telegram.org/bots/api#answercallbackquery"

  ([config callback-query-id]
   (answer-callback-query config callback-query-id nil))

  ([config callback-query-id {:keys [url
                                     text
                                     show-alert?
                                     cache-time]}]
   (api-request config
                :answerCallbackQuery
                :post
                {:callback_query_id callback-query-id
                 :text text
                 :show_alert show-alert?
                 :url url
                 :cache_time cache-time})))


(defn copy-message
  "https://core.telegram.org/bots/api#copymessage"

  ([config chat-id from-chat-id message-id]
   (copy-message config chat-id from-chat-id message-id nil))

  ([config chat-id from-chat-id message-id {:keys [message-thread-id
                                                   caption
                                                   parse-mode
                                                   caption-entities
                                                   disable-notification
                                                   protect-content
                                                   reply-to-message-id
                                                   allow-sending-without-reply
                                                   reply-markup]}]
   (api-request
     config
     :copyMessage
     :post
     {:chat_id chat-id
      :message_thread_id message-thread-id
      :from_chat_id from-chat-id
      :message_id message-id
      :caption caption
      :parse_mode parse-mode
      :caption_entities caption-entities
      :disable_notification disable-notification
      :protect_content protect-content
      :reply_to_message_id reply-to-message-id
      :allow_sending_without_reply allow-sending-without-reply
      :reply_markup reply-markup})))


(defn get-chat
  "https://core.telegram.org/bots/api#getchat"
  [config chat-id]
  (api-request
    config
    :getChat
    :get
    {:chat_id chat-id}))


(defn get-file
  [config file-id]
  (let [response (api-request config
                              :getFile
                              :get
                              {:file_id file-id})]

  (conj response
        {:url (str "https://api.telegram.org/file/bot" (:token config) "/" (:file_path response))})))


(defn create-chat-invite-link
  "https://core.telegram.org/bots/api#createchatinvitelink"
  ([config chat-id]
   (create-chat-invite-link config chat-id nil))

  ([config chat-id {:keys [link-name
                           expire-date
                           member-limit
                           creates-join-request?]}]

   (api-request
     config
     :createChatInviteLink
     :post
     {:chat_id chat-id
      :name link-name
      :expire_date expire-date
      :member_limit member-limit
      :creates_join_request creates-join-request?})))


;;
;; Dev
;;


(comment

 (def telegram
   {:token (slurp "token")
    :user-agent "Clojure 1.10.3"
    :timeout 300000
    :keepalive 300000
    :local-server nil})

  (edit-message-photo
    telegram
    163440129
    106
    (clojure.java.io/file "target/full.png"))

  (get-me telegram)




 (get-updates telegram {:timeout 30 :offset 75640811})

 (ban-user telegram -721166690 223429441 {:unix-until 0})

 (send-message telegram -721166690 "hello!")

 (send-message telegram -721166690 "hello!"
               {:reply-markup
                {:inline_keyboard
                 [[{:text "a"}
                   {:text "b"}
                   {:text "c"}]
                  [{:text "d"}
                   {:text "e"}
                   {:text "f"}]]}})

 (restrict-user telegram -721166690 223429441 {:can_send_messages false})

 (restrict-user telegram -1001175355067 873472876 {:can_send_messages false})


  (send-media-group
    telegram
    163440129
    [{:type "photo"
      :media (io/file "home=page.jpg")}
     {:type "photo"
      :media (io/input-stream "https://www.tema.ru/i/home=page.jpg")}
     {:type "photo"
      :media "https://www.tema.ru/i/home=page.jpg"}]

    )

 )
