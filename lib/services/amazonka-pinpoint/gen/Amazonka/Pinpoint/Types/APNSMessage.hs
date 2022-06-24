{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.APNSMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.APNSMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a one-time message that\'s sent directly to
-- an endpoint through the APNs (Apple Push Notification service) channel.
--
-- /See:/ 'newAPNSMessage' smart constructor.
data APNSMessage = APNSMessage'
  { -- | An arbitrary identifier that, if assigned to multiple messages, APNs
    -- uses to coalesce the messages into a single push notification instead of
    -- delivering each message individually. This value can\'t exceed 64 bytes.
    --
    -- Amazon Pinpoint specifies this value in the apns-collapse-id request
    -- header when it sends the notification message to APNs.
    collapseId :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that APNs should store and attempt to
    -- deliver the push notification, if the service is unable to deliver the
    -- notification the first time. If this value is 0, APNs treats the
    -- notification as if it expires immediately and the service doesn\'t store
    -- or try to deliver the notification again.
    --
    -- Amazon Pinpoint specifies this value in the apns-expiration request
    -- header when it sends the notification message to APNs.
    timeToLive :: Prelude.Maybe Prelude.Int,
    -- | The key that represents your app-specific identifier for grouping
    -- notifications. If you provide a Notification Content app extension, you
    -- can use this value to group your notifications together.
    threadId :: Prelude.Maybe Prelude.Text,
    -- | The body of the notification message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The URL to open in the recipient\'s default mobile browser, if a
    -- recipient taps the push notification and the value of the Action
    -- property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The default message variables to use in the notification message. You
    -- can override these default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The raw, JSON-formatted string to use as the payload for the
    -- notification message. If specified, this value overrides all other
    -- content for the message.
    --
    -- If you specify the raw content of an APNs push notification, the message
    -- payload has to include the content-available key. The value of the
    -- content-available key has to be an integer, and can only be 0 or 1. If
    -- you\'re sending a standard notification, set the value of
    -- content-available to 0. If you\'re sending a silent (background)
    -- notification, set the value of content-available to 1. Additionally,
    -- silent notification payloads can\'t include the alert, badge, or sound
    -- keys. For more information, see
    -- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification Generating a Remote Notification>
    -- and
    -- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/pushing_background_updates_to_your_app Pushing Background Updates to Your App>
    -- on the Apple Developer website.
    rawContent :: Prelude.Maybe Prelude.Text,
    -- | The key for the sound to play when the recipient receives the push
    -- notification. The value for this key is the name of a sound file in your
    -- app\'s main bundle or the Library\/Sounds folder in your app\'s data
    -- container. If the sound file can\'t be found or you specify default for
    -- the value, the system plays the default alert sound.
    sound :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the notification is a silent push notification. A
    -- silent (or background) push notification isn\'t displayed on
    -- recipients\' devices. You can use silent push notifications to make
    -- small updates to your app, or to display messages in an in-app message
    -- center.
    --
    -- Amazon Pinpoint uses this property to determine the correct value for
    -- the apns-push-type request header when it sends the notification message
    -- to APNs. If you specify a value of true for this property, Amazon
    -- Pinpoint sets the value for the apns-push-type header field to
    -- background.
    --
    -- If you specify the raw content of an APNs push notification, the message
    -- payload has to include the content-available key. For silent
    -- (background) notifications, set the value of content-available to 1.
    -- Additionally, the message payload for a silent notification can\'t
    -- include the alert, badge, or sound keys. For more information, see
    -- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification Generating a Remote Notification>
    -- and
    -- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/pushing_background_updates_to_your_app Pushing Background Updates to Your App>
    -- on the Apple Developer website.
    --
    -- Apple has indicated that they will throttle \"excessive\" background
    -- notifications based on current traffic volumes. To prevent your
    -- notifications being throttled, Apple recommends that you send no more
    -- than 3 silent push notifications to each recipient per hour.
    silentPush :: Prelude.Maybe Prelude.Bool,
    -- | The type of push notification to send. Valid values are:
    --
    -- -   alert - For a standard notification that\'s displayed on
    --     recipients\' devices and prompts a recipient to interact with the
    --     notification.
    --
    -- -   background - For a silent notification that delivers content in the
    --     background and isn\'t displayed on recipients\' devices.
    --
    -- -   complication - For a notification that contains update information
    --     for an app’s complication timeline.
    --
    -- -   fileprovider - For a notification that signals changes to a File
    --     Provider extension.
    --
    -- -   mdm - For a notification that tells managed devices to contact the
    --     MDM server.
    --
    -- -   voip - For a notification that provides information about an
    --     incoming VoIP call.
    --
    -- Amazon Pinpoint specifies this value in the apns-push-type request
    -- header when it sends the notification message to APNs. If you don\'t
    -- specify a value for this property, Amazon Pinpoint sets the value to
    -- alert or background automatically, based on the value that you specify
    -- for the SilentPush or RawContent property of the message.
    --
    -- For more information about the apns-push-type request header, see
    -- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/sending_notification_requests_to_apns Sending Notification Requests to APNs>
    -- on the Apple Developer website.
    aPNSPushType :: Prelude.Maybe Prelude.Text,
    -- | The key that indicates whether and how to modify the badge of your
    -- app\'s icon when the recipient receives the push notification. If this
    -- key isn\'t included in the dictionary, the badge doesn\'t change. To
    -- remove the badge, set this value to 0.
    badge :: Prelude.Maybe Prelude.Int,
    -- | The title to display above the notification message on the recipient\'s
    -- device.
    title :: Prelude.Maybe Prelude.Text,
    -- | para>5 - Low priority, the notification might be delayed, delivered as
    -- part of a group, or throttled.
    --
    -- \/listitem>
    --
    -- 10 - High priority, the notification is sent immediately. This is the
    -- default value. A high priority notification should trigger an alert,
    -- play a sound, or badge your app\'s icon on the recipient\'s device.
    --
    -- \/para>
    --
    -- Amazon Pinpoint specifies this value in the apns-priority request header
    -- when it sends the notification message to APNs.
    --
    -- The equivalent values for Firebase Cloud Messaging (FCM), formerly
    -- Google Cloud Messaging (GCM), are normal, for 5, and high, for 10. If
    -- you specify an FCM value for this property, Amazon Pinpoint accepts and
    -- converts the value to the corresponding APNs value.
    priority :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image or video to display in the push notification.
    mediaUrl :: Prelude.Maybe Prelude.Text,
    -- | The key that indicates the notification type for the push notification.
    -- This key is a value that\'s defined by the identifier property of one of
    -- your app\'s registered categories.
    category :: Prelude.Maybe Prelude.Text,
    -- | The action to occur if the recipient taps the push notification. Valid
    -- values are:
    --
    -- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
    --     sent to the background. This is the default action.
    --
    -- -   DEEP_LINK - Your app opens and displays a designated user interface
    --     in the app. This setting uses the deep-linking features of the iOS
    --     platform.
    --
    -- -   URL - The default mobile browser on the recipient\'s device opens
    --     and loads the web page at a URL that you specify.
    action :: Prelude.Maybe Action,
    -- | The JSON payload to use for a silent push notification. This payload is
    -- added to the data.pinpoint.jsonBody object of the notification.
    data' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The authentication method that you want Amazon Pinpoint to use when
    -- authenticating with APNs, CERTIFICATE or TOKEN.
    preferredAuthenticationMethod :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'APNSMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collapseId', 'aPNSMessage_collapseId' - An arbitrary identifier that, if assigned to multiple messages, APNs
-- uses to coalesce the messages into a single push notification instead of
-- delivering each message individually. This value can\'t exceed 64 bytes.
--
-- Amazon Pinpoint specifies this value in the apns-collapse-id request
-- header when it sends the notification message to APNs.
--
-- 'timeToLive', 'aPNSMessage_timeToLive' - The amount of time, in seconds, that APNs should store and attempt to
-- deliver the push notification, if the service is unable to deliver the
-- notification the first time. If this value is 0, APNs treats the
-- notification as if it expires immediately and the service doesn\'t store
-- or try to deliver the notification again.
--
-- Amazon Pinpoint specifies this value in the apns-expiration request
-- header when it sends the notification message to APNs.
--
-- 'threadId', 'aPNSMessage_threadId' - The key that represents your app-specific identifier for grouping
-- notifications. If you provide a Notification Content app extension, you
-- can use this value to group your notifications together.
--
-- 'body', 'aPNSMessage_body' - The body of the notification message.
--
-- 'url', 'aPNSMessage_url' - The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
--
-- 'substitutions', 'aPNSMessage_substitutions' - The default message variables to use in the notification message. You
-- can override these default variables with individual address variables.
--
-- 'rawContent', 'aPNSMessage_rawContent' - The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
--
-- If you specify the raw content of an APNs push notification, the message
-- payload has to include the content-available key. The value of the
-- content-available key has to be an integer, and can only be 0 or 1. If
-- you\'re sending a standard notification, set the value of
-- content-available to 0. If you\'re sending a silent (background)
-- notification, set the value of content-available to 1. Additionally,
-- silent notification payloads can\'t include the alert, badge, or sound
-- keys. For more information, see
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification Generating a Remote Notification>
-- and
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/pushing_background_updates_to_your_app Pushing Background Updates to Your App>
-- on the Apple Developer website.
--
-- 'sound', 'aPNSMessage_sound' - The key for the sound to play when the recipient receives the push
-- notification. The value for this key is the name of a sound file in your
-- app\'s main bundle or the Library\/Sounds folder in your app\'s data
-- container. If the sound file can\'t be found or you specify default for
-- the value, the system plays the default alert sound.
--
-- 'silentPush', 'aPNSMessage_silentPush' - Specifies whether the notification is a silent push notification. A
-- silent (or background) push notification isn\'t displayed on
-- recipients\' devices. You can use silent push notifications to make
-- small updates to your app, or to display messages in an in-app message
-- center.
--
-- Amazon Pinpoint uses this property to determine the correct value for
-- the apns-push-type request header when it sends the notification message
-- to APNs. If you specify a value of true for this property, Amazon
-- Pinpoint sets the value for the apns-push-type header field to
-- background.
--
-- If you specify the raw content of an APNs push notification, the message
-- payload has to include the content-available key. For silent
-- (background) notifications, set the value of content-available to 1.
-- Additionally, the message payload for a silent notification can\'t
-- include the alert, badge, or sound keys. For more information, see
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification Generating a Remote Notification>
-- and
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/pushing_background_updates_to_your_app Pushing Background Updates to Your App>
-- on the Apple Developer website.
--
-- Apple has indicated that they will throttle \"excessive\" background
-- notifications based on current traffic volumes. To prevent your
-- notifications being throttled, Apple recommends that you send no more
-- than 3 silent push notifications to each recipient per hour.
--
-- 'aPNSPushType', 'aPNSMessage_aPNSPushType' - The type of push notification to send. Valid values are:
--
-- -   alert - For a standard notification that\'s displayed on
--     recipients\' devices and prompts a recipient to interact with the
--     notification.
--
-- -   background - For a silent notification that delivers content in the
--     background and isn\'t displayed on recipients\' devices.
--
-- -   complication - For a notification that contains update information
--     for an app’s complication timeline.
--
-- -   fileprovider - For a notification that signals changes to a File
--     Provider extension.
--
-- -   mdm - For a notification that tells managed devices to contact the
--     MDM server.
--
-- -   voip - For a notification that provides information about an
--     incoming VoIP call.
--
-- Amazon Pinpoint specifies this value in the apns-push-type request
-- header when it sends the notification message to APNs. If you don\'t
-- specify a value for this property, Amazon Pinpoint sets the value to
-- alert or background automatically, based on the value that you specify
-- for the SilentPush or RawContent property of the message.
--
-- For more information about the apns-push-type request header, see
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/sending_notification_requests_to_apns Sending Notification Requests to APNs>
-- on the Apple Developer website.
--
-- 'badge', 'aPNSMessage_badge' - The key that indicates whether and how to modify the badge of your
-- app\'s icon when the recipient receives the push notification. If this
-- key isn\'t included in the dictionary, the badge doesn\'t change. To
-- remove the badge, set this value to 0.
--
-- 'title', 'aPNSMessage_title' - The title to display above the notification message on the recipient\'s
-- device.
--
-- 'priority', 'aPNSMessage_priority' - para>5 - Low priority, the notification might be delayed, delivered as
-- part of a group, or throttled.
--
-- \/listitem>
--
-- 10 - High priority, the notification is sent immediately. This is the
-- default value. A high priority notification should trigger an alert,
-- play a sound, or badge your app\'s icon on the recipient\'s device.
--
-- \/para>
--
-- Amazon Pinpoint specifies this value in the apns-priority request header
-- when it sends the notification message to APNs.
--
-- The equivalent values for Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), are normal, for 5, and high, for 10. If
-- you specify an FCM value for this property, Amazon Pinpoint accepts and
-- converts the value to the corresponding APNs value.
--
-- 'mediaUrl', 'aPNSMessage_mediaUrl' - The URL of an image or video to display in the push notification.
--
-- 'category', 'aPNSMessage_category' - The key that indicates the notification type for the push notification.
-- This key is a value that\'s defined by the identifier property of one of
-- your app\'s registered categories.
--
-- 'action', 'aPNSMessage_action' - The action to occur if the recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of the iOS
--     platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
--
-- 'data'', 'aPNSMessage_data' - The JSON payload to use for a silent push notification. This payload is
-- added to the data.pinpoint.jsonBody object of the notification.
--
-- 'preferredAuthenticationMethod', 'aPNSMessage_preferredAuthenticationMethod' - The authentication method that you want Amazon Pinpoint to use when
-- authenticating with APNs, CERTIFICATE or TOKEN.
newAPNSMessage ::
  APNSMessage
newAPNSMessage =
  APNSMessage'
    { collapseId = Prelude.Nothing,
      timeToLive = Prelude.Nothing,
      threadId = Prelude.Nothing,
      body = Prelude.Nothing,
      url = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      rawContent = Prelude.Nothing,
      sound = Prelude.Nothing,
      silentPush = Prelude.Nothing,
      aPNSPushType = Prelude.Nothing,
      badge = Prelude.Nothing,
      title = Prelude.Nothing,
      priority = Prelude.Nothing,
      mediaUrl = Prelude.Nothing,
      category = Prelude.Nothing,
      action = Prelude.Nothing,
      data' = Prelude.Nothing,
      preferredAuthenticationMethod = Prelude.Nothing
    }

-- | An arbitrary identifier that, if assigned to multiple messages, APNs
-- uses to coalesce the messages into a single push notification instead of
-- delivering each message individually. This value can\'t exceed 64 bytes.
--
-- Amazon Pinpoint specifies this value in the apns-collapse-id request
-- header when it sends the notification message to APNs.
aPNSMessage_collapseId :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_collapseId = Lens.lens (\APNSMessage' {collapseId} -> collapseId) (\s@APNSMessage' {} a -> s {collapseId = a} :: APNSMessage)

-- | The amount of time, in seconds, that APNs should store and attempt to
-- deliver the push notification, if the service is unable to deliver the
-- notification the first time. If this value is 0, APNs treats the
-- notification as if it expires immediately and the service doesn\'t store
-- or try to deliver the notification again.
--
-- Amazon Pinpoint specifies this value in the apns-expiration request
-- header when it sends the notification message to APNs.
aPNSMessage_timeToLive :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Int)
aPNSMessage_timeToLive = Lens.lens (\APNSMessage' {timeToLive} -> timeToLive) (\s@APNSMessage' {} a -> s {timeToLive = a} :: APNSMessage)

-- | The key that represents your app-specific identifier for grouping
-- notifications. If you provide a Notification Content app extension, you
-- can use this value to group your notifications together.
aPNSMessage_threadId :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_threadId = Lens.lens (\APNSMessage' {threadId} -> threadId) (\s@APNSMessage' {} a -> s {threadId = a} :: APNSMessage)

-- | The body of the notification message.
aPNSMessage_body :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_body = Lens.lens (\APNSMessage' {body} -> body) (\s@APNSMessage' {} a -> s {body = a} :: APNSMessage)

-- | The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
aPNSMessage_url :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_url = Lens.lens (\APNSMessage' {url} -> url) (\s@APNSMessage' {} a -> s {url = a} :: APNSMessage)

-- | The default message variables to use in the notification message. You
-- can override these default variables with individual address variables.
aPNSMessage_substitutions :: Lens.Lens' APNSMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
aPNSMessage_substitutions = Lens.lens (\APNSMessage' {substitutions} -> substitutions) (\s@APNSMessage' {} a -> s {substitutions = a} :: APNSMessage) Prelude.. Lens.mapping Lens.coerced

-- | The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
--
-- If you specify the raw content of an APNs push notification, the message
-- payload has to include the content-available key. The value of the
-- content-available key has to be an integer, and can only be 0 or 1. If
-- you\'re sending a standard notification, set the value of
-- content-available to 0. If you\'re sending a silent (background)
-- notification, set the value of content-available to 1. Additionally,
-- silent notification payloads can\'t include the alert, badge, or sound
-- keys. For more information, see
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification Generating a Remote Notification>
-- and
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/pushing_background_updates_to_your_app Pushing Background Updates to Your App>
-- on the Apple Developer website.
aPNSMessage_rawContent :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_rawContent = Lens.lens (\APNSMessage' {rawContent} -> rawContent) (\s@APNSMessage' {} a -> s {rawContent = a} :: APNSMessage)

-- | The key for the sound to play when the recipient receives the push
-- notification. The value for this key is the name of a sound file in your
-- app\'s main bundle or the Library\/Sounds folder in your app\'s data
-- container. If the sound file can\'t be found or you specify default for
-- the value, the system plays the default alert sound.
aPNSMessage_sound :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_sound = Lens.lens (\APNSMessage' {sound} -> sound) (\s@APNSMessage' {} a -> s {sound = a} :: APNSMessage)

-- | Specifies whether the notification is a silent push notification. A
-- silent (or background) push notification isn\'t displayed on
-- recipients\' devices. You can use silent push notifications to make
-- small updates to your app, or to display messages in an in-app message
-- center.
--
-- Amazon Pinpoint uses this property to determine the correct value for
-- the apns-push-type request header when it sends the notification message
-- to APNs. If you specify a value of true for this property, Amazon
-- Pinpoint sets the value for the apns-push-type header field to
-- background.
--
-- If you specify the raw content of an APNs push notification, the message
-- payload has to include the content-available key. For silent
-- (background) notifications, set the value of content-available to 1.
-- Additionally, the message payload for a silent notification can\'t
-- include the alert, badge, or sound keys. For more information, see
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification Generating a Remote Notification>
-- and
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/pushing_background_updates_to_your_app Pushing Background Updates to Your App>
-- on the Apple Developer website.
--
-- Apple has indicated that they will throttle \"excessive\" background
-- notifications based on current traffic volumes. To prevent your
-- notifications being throttled, Apple recommends that you send no more
-- than 3 silent push notifications to each recipient per hour.
aPNSMessage_silentPush :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Bool)
aPNSMessage_silentPush = Lens.lens (\APNSMessage' {silentPush} -> silentPush) (\s@APNSMessage' {} a -> s {silentPush = a} :: APNSMessage)

-- | The type of push notification to send. Valid values are:
--
-- -   alert - For a standard notification that\'s displayed on
--     recipients\' devices and prompts a recipient to interact with the
--     notification.
--
-- -   background - For a silent notification that delivers content in the
--     background and isn\'t displayed on recipients\' devices.
--
-- -   complication - For a notification that contains update information
--     for an app’s complication timeline.
--
-- -   fileprovider - For a notification that signals changes to a File
--     Provider extension.
--
-- -   mdm - For a notification that tells managed devices to contact the
--     MDM server.
--
-- -   voip - For a notification that provides information about an
--     incoming VoIP call.
--
-- Amazon Pinpoint specifies this value in the apns-push-type request
-- header when it sends the notification message to APNs. If you don\'t
-- specify a value for this property, Amazon Pinpoint sets the value to
-- alert or background automatically, based on the value that you specify
-- for the SilentPush or RawContent property of the message.
--
-- For more information about the apns-push-type request header, see
-- <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/sending_notification_requests_to_apns Sending Notification Requests to APNs>
-- on the Apple Developer website.
aPNSMessage_aPNSPushType :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_aPNSPushType = Lens.lens (\APNSMessage' {aPNSPushType} -> aPNSPushType) (\s@APNSMessage' {} a -> s {aPNSPushType = a} :: APNSMessage)

-- | The key that indicates whether and how to modify the badge of your
-- app\'s icon when the recipient receives the push notification. If this
-- key isn\'t included in the dictionary, the badge doesn\'t change. To
-- remove the badge, set this value to 0.
aPNSMessage_badge :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Int)
aPNSMessage_badge = Lens.lens (\APNSMessage' {badge} -> badge) (\s@APNSMessage' {} a -> s {badge = a} :: APNSMessage)

-- | The title to display above the notification message on the recipient\'s
-- device.
aPNSMessage_title :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_title = Lens.lens (\APNSMessage' {title} -> title) (\s@APNSMessage' {} a -> s {title = a} :: APNSMessage)

-- | para>5 - Low priority, the notification might be delayed, delivered as
-- part of a group, or throttled.
--
-- \/listitem>
--
-- 10 - High priority, the notification is sent immediately. This is the
-- default value. A high priority notification should trigger an alert,
-- play a sound, or badge your app\'s icon on the recipient\'s device.
--
-- \/para>
--
-- Amazon Pinpoint specifies this value in the apns-priority request header
-- when it sends the notification message to APNs.
--
-- The equivalent values for Firebase Cloud Messaging (FCM), formerly
-- Google Cloud Messaging (GCM), are normal, for 5, and high, for 10. If
-- you specify an FCM value for this property, Amazon Pinpoint accepts and
-- converts the value to the corresponding APNs value.
aPNSMessage_priority :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_priority = Lens.lens (\APNSMessage' {priority} -> priority) (\s@APNSMessage' {} a -> s {priority = a} :: APNSMessage)

-- | The URL of an image or video to display in the push notification.
aPNSMessage_mediaUrl :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_mediaUrl = Lens.lens (\APNSMessage' {mediaUrl} -> mediaUrl) (\s@APNSMessage' {} a -> s {mediaUrl = a} :: APNSMessage)

-- | The key that indicates the notification type for the push notification.
-- This key is a value that\'s defined by the identifier property of one of
-- your app\'s registered categories.
aPNSMessage_category :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_category = Lens.lens (\APNSMessage' {category} -> category) (\s@APNSMessage' {} a -> s {category = a} :: APNSMessage)

-- | The action to occur if the recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of the iOS
--     platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
aPNSMessage_action :: Lens.Lens' APNSMessage (Prelude.Maybe Action)
aPNSMessage_action = Lens.lens (\APNSMessage' {action} -> action) (\s@APNSMessage' {} a -> s {action = a} :: APNSMessage)

-- | The JSON payload to use for a silent push notification. This payload is
-- added to the data.pinpoint.jsonBody object of the notification.
aPNSMessage_data :: Lens.Lens' APNSMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
aPNSMessage_data = Lens.lens (\APNSMessage' {data'} -> data') (\s@APNSMessage' {} a -> s {data' = a} :: APNSMessage) Prelude.. Lens.mapping Lens.coerced

-- | The authentication method that you want Amazon Pinpoint to use when
-- authenticating with APNs, CERTIFICATE or TOKEN.
aPNSMessage_preferredAuthenticationMethod :: Lens.Lens' APNSMessage (Prelude.Maybe Prelude.Text)
aPNSMessage_preferredAuthenticationMethod = Lens.lens (\APNSMessage' {preferredAuthenticationMethod} -> preferredAuthenticationMethod) (\s@APNSMessage' {} a -> s {preferredAuthenticationMethod = a} :: APNSMessage)

instance Prelude.Hashable APNSMessage where
  hashWithSalt _salt APNSMessage' {..} =
    _salt `Prelude.hashWithSalt` collapseId
      `Prelude.hashWithSalt` timeToLive
      `Prelude.hashWithSalt` threadId
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` substitutions
      `Prelude.hashWithSalt` rawContent
      `Prelude.hashWithSalt` sound
      `Prelude.hashWithSalt` silentPush
      `Prelude.hashWithSalt` aPNSPushType
      `Prelude.hashWithSalt` badge
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` mediaUrl
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` preferredAuthenticationMethod

instance Prelude.NFData APNSMessage where
  rnf APNSMessage' {..} =
    Prelude.rnf collapseId
      `Prelude.seq` Prelude.rnf timeToLive
      `Prelude.seq` Prelude.rnf threadId
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf substitutions
      `Prelude.seq` Prelude.rnf rawContent
      `Prelude.seq` Prelude.rnf sound
      `Prelude.seq` Prelude.rnf silentPush
      `Prelude.seq` Prelude.rnf aPNSPushType
      `Prelude.seq` Prelude.rnf badge
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf mediaUrl
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf
        preferredAuthenticationMethod

instance Core.ToJSON APNSMessage where
  toJSON APNSMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CollapseId" Core..=) Prelude.<$> collapseId,
            ("TimeToLive" Core..=) Prelude.<$> timeToLive,
            ("ThreadId" Core..=) Prelude.<$> threadId,
            ("Body" Core..=) Prelude.<$> body,
            ("Url" Core..=) Prelude.<$> url,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("RawContent" Core..=) Prelude.<$> rawContent,
            ("Sound" Core..=) Prelude.<$> sound,
            ("SilentPush" Core..=) Prelude.<$> silentPush,
            ("APNSPushType" Core..=) Prelude.<$> aPNSPushType,
            ("Badge" Core..=) Prelude.<$> badge,
            ("Title" Core..=) Prelude.<$> title,
            ("Priority" Core..=) Prelude.<$> priority,
            ("MediaUrl" Core..=) Prelude.<$> mediaUrl,
            ("Category" Core..=) Prelude.<$> category,
            ("Action" Core..=) Prelude.<$> action,
            ("Data" Core..=) Prelude.<$> data',
            ("PreferredAuthenticationMethod" Core..=)
              Prelude.<$> preferredAuthenticationMethod
          ]
      )
