-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSMessage
  ( APNSMessage (..),

    -- * Smart constructor
    mkAPNSMessage,

    -- * Lenses
    amSubstitutions,
    amSilentPush,
    amAPNSPushType,
    amPriority,
    amRawContent,
    amData,
    amBody,
    amCategory,
    amTimeToLive,
    amURL,
    amSound,
    amAction,
    amMediaURL,
    amPreferredAuthenticationMethod,
    amBadge,
    amTitle,
    amThreadId,
    amCollapseId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the APNs (Apple Push Notification service) channel.
--
-- /See:/ 'mkAPNSMessage' smart constructor.
data APNSMessage = APNSMessage'
  { substitutions ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    silentPush :: Lude.Maybe Lude.Bool,
    apnsPushType :: Lude.Maybe Lude.Text,
    priority :: Lude.Maybe Lude.Text,
    rawContent :: Lude.Maybe Lude.Text,
    data' :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    body :: Lude.Maybe Lude.Text,
    category :: Lude.Maybe Lude.Text,
    timeToLive :: Lude.Maybe Lude.Int,
    url :: Lude.Maybe Lude.Text,
    sound :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
    mediaURL :: Lude.Maybe Lude.Text,
    preferredAuthenticationMethod :: Lude.Maybe Lude.Text,
    badge :: Lude.Maybe Lude.Int,
    title :: Lude.Maybe Lude.Text,
    threadId :: Lude.Maybe Lude.Text,
    collapseId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APNSMessage' with the minimum fields required to make a request.
--
-- * 'action' - The action to occur if the recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS platform.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
-- * 'apnsPushType' - The type of push notification to send. Valid values are:
--
--
--     * alert - For a standard notification that's displayed on recipients' devices and prompts a recipient to interact with the notification.
--
--
--     * background - For a silent notification that delivers content in the background and isn't displayed on recipients' devices.
--
--
--     * complication - For a notification that contains update information for an app’s complication timeline.
--
--
--     * fileprovider - For a notification that signals changes to a File Provider extension.
--
--
--     * mdm - For a notification that tells managed devices to contact the MDM server.
--
--
--     * voip - For a notification that provides information about an incoming VoIP call.
--
--
-- Amazon Pinpoint specifies this value in the apns-push-type request header when it sends the notification message to APNs. If you don't specify a value for this property, Amazon Pinpoint sets the value to alert or background automatically, based on the value that you specify for the SilentPush or RawContent property of the message.
-- For more information about the apns-push-type request header, see <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/sending_notification_requests_to_apns Sending Notification Requests to APNs> on the Apple Developer website.
-- * 'badge' - The key that indicates whether and how to modify the badge of your app's icon when the recipient receives the push notification. If this key isn't included in the dictionary, the badge doesn't change. To remove the badge, set this value to 0.
-- * 'body' - The body of the notification message.
-- * 'category' - The key that indicates the notification type for the push notification. This key is a value that's defined by the identifier property of one of your app's registered categories.
-- * 'collapseId' - An arbitrary identifier that, if assigned to multiple messages, APNs uses to coalesce the messages into a single push notification instead of delivering each message individually. This value can't exceed 64 bytes.
--
-- Amazon Pinpoint specifies this value in the apns-collapse-id request header when it sends the notification message to APNs.
-- * 'data'' - The JSON payload to use for a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
-- * 'mediaURL' - The URL of an image or video to display in the push notification.
-- * 'preferredAuthenticationMethod' - The authentication method that you want Amazon Pinpoint to use when authenticating with APNs, CERTIFICATE or TOKEN.
-- * 'priority' - para>5 - Low priority, the notification might be delayed, delivered as part of a group, or throttled.
--
-- /listitem>
--     * 10 - High priority, the notification is sent immediately. This is the default value. A high priority notification should trigger an alert, play a sound, or badge your app's icon on the recipient's device.
--
-- /para> Amazon Pinpoint specifies this value in the apns-priority request header when it sends the notification message to APNs.
-- The equivalent values for Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), are normal, for 5, and high, for 10. If you specify an FCM value for this property, Amazon Pinpoint accepts and converts the value to the corresponding APNs value.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
-- * 'silentPush' - Specifies whether the notification is a silent push notification. A silent (or background) push notification isn't displayed on recipients' devices. You can use silent push notifications to make small updates to your app, or to display messages in an in-app message center.
--
-- Amazon Pinpoint uses this property to determine the correct value for the apns-push-type request header when it sends the notification message to APNs. If you specify a value of true for this property, Amazon Pinpoint sets the value for the apns-push-type header field to background.
-- * 'sound' - The key for the sound to play when the recipient receives the push notification. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
-- * 'substitutions' - The default message variables to use in the notification message. You can override these default variables with individual address variables.
-- * 'threadId' - The key that represents your app-specific identifier for grouping notifications. If you provide a Notification Content app extension, you can use this value to group your notifications together.
-- * 'timeToLive' - The amount of time, in seconds, that APNs should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If this value is 0, APNs treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- Amazon Pinpoint specifies this value in the apns-expiration request header when it sends the notification message to APNs.
-- * 'title' - The title to display above the notification message on the recipient's device.
-- * 'url' - The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
mkAPNSMessage ::
  APNSMessage
mkAPNSMessage =
  APNSMessage'
    { substitutions = Lude.Nothing,
      silentPush = Lude.Nothing,
      apnsPushType = Lude.Nothing,
      priority = Lude.Nothing,
      rawContent = Lude.Nothing,
      data' = Lude.Nothing,
      body = Lude.Nothing,
      category = Lude.Nothing,
      timeToLive = Lude.Nothing,
      url = Lude.Nothing,
      sound = Lude.Nothing,
      action = Lude.Nothing,
      mediaURL = Lude.Nothing,
      preferredAuthenticationMethod = Lude.Nothing,
      badge = Lude.Nothing,
      title = Lude.Nothing,
      threadId = Lude.Nothing,
      collapseId = Lude.Nothing
    }

-- | The default message variables to use in the notification message. You can override these default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amSubstitutions :: Lens.Lens' APNSMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
amSubstitutions = Lens.lens (substitutions :: APNSMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: APNSMessage)
{-# DEPRECATED amSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | Specifies whether the notification is a silent push notification. A silent (or background) push notification isn't displayed on recipients' devices. You can use silent push notifications to make small updates to your app, or to display messages in an in-app message center.
--
-- Amazon Pinpoint uses this property to determine the correct value for the apns-push-type request header when it sends the notification message to APNs. If you specify a value of true for this property, Amazon Pinpoint sets the value for the apns-push-type header field to background.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amSilentPush :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Bool)
amSilentPush = Lens.lens (silentPush :: APNSMessage -> Lude.Maybe Lude.Bool) (\s a -> s {silentPush = a} :: APNSMessage)
{-# DEPRECATED amSilentPush "Use generic-lens or generic-optics with 'silentPush' instead." #-}

-- | The type of push notification to send. Valid values are:
--
--
--     * alert - For a standard notification that's displayed on recipients' devices and prompts a recipient to interact with the notification.
--
--
--     * background - For a silent notification that delivers content in the background and isn't displayed on recipients' devices.
--
--
--     * complication - For a notification that contains update information for an app’s complication timeline.
--
--
--     * fileprovider - For a notification that signals changes to a File Provider extension.
--
--
--     * mdm - For a notification that tells managed devices to contact the MDM server.
--
--
--     * voip - For a notification that provides information about an incoming VoIP call.
--
--
-- Amazon Pinpoint specifies this value in the apns-push-type request header when it sends the notification message to APNs. If you don't specify a value for this property, Amazon Pinpoint sets the value to alert or background automatically, based on the value that you specify for the SilentPush or RawContent property of the message.
-- For more information about the apns-push-type request header, see <https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/sending_notification_requests_to_apns Sending Notification Requests to APNs> on the Apple Developer website.
--
-- /Note:/ Consider using 'apnsPushType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amAPNSPushType :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amAPNSPushType = Lens.lens (apnsPushType :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {apnsPushType = a} :: APNSMessage)
{-# DEPRECATED amAPNSPushType "Use generic-lens or generic-optics with 'apnsPushType' instead." #-}

-- | para>5 - Low priority, the notification might be delayed, delivered as part of a group, or throttled.
--
-- /listitem>
--     * 10 - High priority, the notification is sent immediately. This is the default value. A high priority notification should trigger an alert, play a sound, or badge your app's icon on the recipient's device.
--
-- /para> Amazon Pinpoint specifies this value in the apns-priority request header when it sends the notification message to APNs.
-- The equivalent values for Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), are normal, for 5, and high, for 10. If you specify an FCM value for this property, Amazon Pinpoint accepts and converts the value to the corresponding APNs value.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amPriority :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amPriority = Lens.lens (priority :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {priority = a} :: APNSMessage)
{-# DEPRECATED amPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amRawContent :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amRawContent = Lens.lens (rawContent :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: APNSMessage)
{-# DEPRECATED amRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The JSON payload to use for a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amData :: Lens.Lens' APNSMessage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
amData = Lens.lens (data' :: APNSMessage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {data' = a} :: APNSMessage)
{-# DEPRECATED amData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amBody :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amBody = Lens.lens (body :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: APNSMessage)
{-# DEPRECATED amBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The key that indicates the notification type for the push notification. This key is a value that's defined by the identifier property of one of your app's registered categories.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amCategory :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amCategory = Lens.lens (category :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: APNSMessage)
{-# DEPRECATED amCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The amount of time, in seconds, that APNs should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If this value is 0, APNs treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- Amazon Pinpoint specifies this value in the apns-expiration request header when it sends the notification message to APNs.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amTimeToLive :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Int)
amTimeToLive = Lens.lens (timeToLive :: APNSMessage -> Lude.Maybe Lude.Int) (\s a -> s {timeToLive = a} :: APNSMessage)
{-# DEPRECATED amTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amURL :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amURL = Lens.lens (url :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: APNSMessage)
{-# DEPRECATED amURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The key for the sound to play when the recipient receives the push notification. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amSound :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amSound = Lens.lens (sound :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {sound = a} :: APNSMessage)
{-# DEPRECATED amSound "Use generic-lens or generic-optics with 'sound' instead." #-}

-- | The action to occur if the recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS platform.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amAction :: Lens.Lens' APNSMessage (Lude.Maybe Action)
amAction = Lens.lens (action :: APNSMessage -> Lude.Maybe Action) (\s a -> s {action = a} :: APNSMessage)
{-# DEPRECATED amAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The URL of an image or video to display in the push notification.
--
-- /Note:/ Consider using 'mediaURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amMediaURL :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amMediaURL = Lens.lens (mediaURL :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {mediaURL = a} :: APNSMessage)
{-# DEPRECATED amMediaURL "Use generic-lens or generic-optics with 'mediaURL' instead." #-}

-- | The authentication method that you want Amazon Pinpoint to use when authenticating with APNs, CERTIFICATE or TOKEN.
--
-- /Note:/ Consider using 'preferredAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amPreferredAuthenticationMethod :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amPreferredAuthenticationMethod = Lens.lens (preferredAuthenticationMethod :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {preferredAuthenticationMethod = a} :: APNSMessage)
{-# DEPRECATED amPreferredAuthenticationMethod "Use generic-lens or generic-optics with 'preferredAuthenticationMethod' instead." #-}

-- | The key that indicates whether and how to modify the badge of your app's icon when the recipient receives the push notification. If this key isn't included in the dictionary, the badge doesn't change. To remove the badge, set this value to 0.
--
-- /Note:/ Consider using 'badge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amBadge :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Int)
amBadge = Lens.lens (badge :: APNSMessage -> Lude.Maybe Lude.Int) (\s a -> s {badge = a} :: APNSMessage)
{-# DEPRECATED amBadge "Use generic-lens or generic-optics with 'badge' instead." #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amTitle :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amTitle = Lens.lens (title :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: APNSMessage)
{-# DEPRECATED amTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The key that represents your app-specific identifier for grouping notifications. If you provide a Notification Content app extension, you can use this value to group your notifications together.
--
-- /Note:/ Consider using 'threadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amThreadId :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amThreadId = Lens.lens (threadId :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {threadId = a} :: APNSMessage)
{-# DEPRECATED amThreadId "Use generic-lens or generic-optics with 'threadId' instead." #-}

-- | An arbitrary identifier that, if assigned to multiple messages, APNs uses to coalesce the messages into a single push notification instead of delivering each message individually. This value can't exceed 64 bytes.
--
-- Amazon Pinpoint specifies this value in the apns-collapse-id request header when it sends the notification message to APNs.
--
-- /Note:/ Consider using 'collapseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amCollapseId :: Lens.Lens' APNSMessage (Lude.Maybe Lude.Text)
amCollapseId = Lens.lens (collapseId :: APNSMessage -> Lude.Maybe Lude.Text) (\s a -> s {collapseId = a} :: APNSMessage)
{-# DEPRECATED amCollapseId "Use generic-lens or generic-optics with 'collapseId' instead." #-}

instance Lude.ToJSON APNSMessage where
  toJSON APNSMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("SilentPush" Lude..=) Lude.<$> silentPush,
            ("APNSPushType" Lude..=) Lude.<$> apnsPushType,
            ("Priority" Lude..=) Lude.<$> priority,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("Data" Lude..=) Lude.<$> data',
            ("Body" Lude..=) Lude.<$> body,
            ("Category" Lude..=) Lude.<$> category,
            ("TimeToLive" Lude..=) Lude.<$> timeToLive,
            ("Url" Lude..=) Lude.<$> url,
            ("Sound" Lude..=) Lude.<$> sound,
            ("Action" Lude..=) Lude.<$> action,
            ("MediaUrl" Lude..=) Lude.<$> mediaURL,
            ("PreferredAuthenticationMethod" Lude..=)
              Lude.<$> preferredAuthenticationMethod,
            ("Badge" Lude..=) Lude.<$> badge,
            ("Title" Lude..=) Lude.<$> title,
            ("ThreadId" Lude..=) Lude.<$> threadId,
            ("CollapseId" Lude..=) Lude.<$> collapseId
          ]
      )
