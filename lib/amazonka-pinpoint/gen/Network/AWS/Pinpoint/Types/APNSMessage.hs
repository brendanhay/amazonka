{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.APNSMessage
  ( APNSMessage (..)
  -- * Smart constructor
  , mkAPNSMessage
  -- * Lenses
  , apnsmAPNSPushType
  , apnsmAction
  , apnsmBadge
  , apnsmBody
  , apnsmCategory
  , apnsmCollapseId
  , apnsmData
  , apnsmMediaUrl
  , apnsmPreferredAuthenticationMethod
  , apnsmPriority
  , apnsmRawContent
  , apnsmSilentPush
  , apnsmSound
  , apnsmSubstitutions
  , apnsmThreadId
  , apnsmTimeToLive
  , apnsmTitle
  , apnsmUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the APNs (Apple Push Notification service) channel.
--
-- /See:/ 'mkAPNSMessage' smart constructor.
data APNSMessage = APNSMessage'
  { aPNSPushType :: Core.Maybe Core.Text
    -- ^ The type of push notification to send. Valid values are:
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
  , action :: Core.Maybe Types.Action
    -- ^ The action to occur if the recipient taps the push notification. Valid values are:
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
  , badge :: Core.Maybe Core.Int
    -- ^ The key that indicates whether and how to modify the badge of your app's icon when the recipient receives the push notification. If this key isn't included in the dictionary, the badge doesn't change. To remove the badge, set this value to 0.
  , body :: Core.Maybe Core.Text
    -- ^ The body of the notification message.
  , category :: Core.Maybe Core.Text
    -- ^ The key that indicates the notification type for the push notification. This key is a value that's defined by the identifier property of one of your app's registered categories.
  , collapseId :: Core.Maybe Core.Text
    -- ^ An arbitrary identifier that, if assigned to multiple messages, APNs uses to coalesce the messages into a single push notification instead of delivering each message individually. This value can't exceed 64 bytes.
--
-- Amazon Pinpoint specifies this value in the apns-collapse-id request header when it sends the notification message to APNs.
  , data' :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The JSON payload to use for a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
  , mediaUrl :: Core.Maybe Core.Text
    -- ^ The URL of an image or video to display in the push notification.
  , preferredAuthenticationMethod :: Core.Maybe Core.Text
    -- ^ The authentication method that you want Amazon Pinpoint to use when authenticating with APNs, CERTIFICATE or TOKEN.
  , priority :: Core.Maybe Core.Text
    -- ^ para>5 - Low priority, the notification might be delayed, delivered as part of a group, or throttled.
--
-- /listitem> 
--     * 10 - High priority, the notification is sent immediately. This is the default value. A high priority notification should trigger an alert, play a sound, or badge your app's icon on the recipient's device.
--
-- /para> Amazon Pinpoint specifies this value in the apns-priority request header when it sends the notification message to APNs.
-- The equivalent values for Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), are normal, for 5, and high, for 10. If you specify an FCM value for this property, Amazon Pinpoint accepts and converts the value to the corresponding APNs value.
  , rawContent :: Core.Maybe Core.Text
    -- ^ The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
  , silentPush :: Core.Maybe Core.Bool
    -- ^ Specifies whether the notification is a silent push notification. A silent (or background) push notification isn't displayed on recipients' devices. You can use silent push notifications to make small updates to your app, or to display messages in an in-app message center.
--
-- Amazon Pinpoint uses this property to determine the correct value for the apns-push-type request header when it sends the notification message to APNs. If you specify a value of true for this property, Amazon Pinpoint sets the value for the apns-push-type header field to background.
  , sound :: Core.Maybe Core.Text
    -- ^ The key for the sound to play when the recipient receives the push notification. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
  , substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The default message variables to use in the notification message. You can override these default variables with individual address variables.
  , threadId :: Core.Maybe Core.Text
    -- ^ The key that represents your app-specific identifier for grouping notifications. If you provide a Notification Content app extension, you can use this value to group your notifications together.
  , timeToLive :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, that APNs should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If this value is 0, APNs treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- Amazon Pinpoint specifies this value in the apns-expiration request header when it sends the notification message to APNs.
  , title :: Core.Maybe Core.Text
    -- ^ The title to display above the notification message on the recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSMessage' value with any optional fields omitted.
mkAPNSMessage
    :: APNSMessage
mkAPNSMessage
  = APNSMessage'{aPNSPushType = Core.Nothing, action = Core.Nothing,
                 badge = Core.Nothing, body = Core.Nothing, category = Core.Nothing,
                 collapseId = Core.Nothing, data' = Core.Nothing,
                 mediaUrl = Core.Nothing,
                 preferredAuthenticationMethod = Core.Nothing,
                 priority = Core.Nothing, rawContent = Core.Nothing,
                 silentPush = Core.Nothing, sound = Core.Nothing,
                 substitutions = Core.Nothing, threadId = Core.Nothing,
                 timeToLive = Core.Nothing, title = Core.Nothing,
                 url = Core.Nothing}

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
-- /Note:/ Consider using 'aPNSPushType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmAPNSPushType :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmAPNSPushType = Lens.field @"aPNSPushType"
{-# INLINEABLE apnsmAPNSPushType #-}
{-# DEPRECATED aPNSPushType "Use generic-lens or generic-optics with 'aPNSPushType' instead"  #-}

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
apnsmAction :: Lens.Lens' APNSMessage (Core.Maybe Types.Action)
apnsmAction = Lens.field @"action"
{-# INLINEABLE apnsmAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The key that indicates whether and how to modify the badge of your app's icon when the recipient receives the push notification. If this key isn't included in the dictionary, the badge doesn't change. To remove the badge, set this value to 0.
--
-- /Note:/ Consider using 'badge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmBadge :: Lens.Lens' APNSMessage (Core.Maybe Core.Int)
apnsmBadge = Lens.field @"badge"
{-# INLINEABLE apnsmBadge #-}
{-# DEPRECATED badge "Use generic-lens or generic-optics with 'badge' instead"  #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmBody :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmBody = Lens.field @"body"
{-# INLINEABLE apnsmBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The key that indicates the notification type for the push notification. This key is a value that's defined by the identifier property of one of your app's registered categories.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmCategory :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmCategory = Lens.field @"category"
{-# INLINEABLE apnsmCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | An arbitrary identifier that, if assigned to multiple messages, APNs uses to coalesce the messages into a single push notification instead of delivering each message individually. This value can't exceed 64 bytes.
--
-- Amazon Pinpoint specifies this value in the apns-collapse-id request header when it sends the notification message to APNs.
--
-- /Note:/ Consider using 'collapseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmCollapseId :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmCollapseId = Lens.field @"collapseId"
{-# INLINEABLE apnsmCollapseId #-}
{-# DEPRECATED collapseId "Use generic-lens or generic-optics with 'collapseId' instead"  #-}

-- | The JSON payload to use for a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmData :: Lens.Lens' APNSMessage (Core.Maybe (Core.HashMap Core.Text Core.Text))
apnsmData = Lens.field @"data'"
{-# INLINEABLE apnsmData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The URL of an image or video to display in the push notification.
--
-- /Note:/ Consider using 'mediaUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmMediaUrl :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmMediaUrl = Lens.field @"mediaUrl"
{-# INLINEABLE apnsmMediaUrl #-}
{-# DEPRECATED mediaUrl "Use generic-lens or generic-optics with 'mediaUrl' instead"  #-}

-- | The authentication method that you want Amazon Pinpoint to use when authenticating with APNs, CERTIFICATE or TOKEN.
--
-- /Note:/ Consider using 'preferredAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmPreferredAuthenticationMethod :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmPreferredAuthenticationMethod = Lens.field @"preferredAuthenticationMethod"
{-# INLINEABLE apnsmPreferredAuthenticationMethod #-}
{-# DEPRECATED preferredAuthenticationMethod "Use generic-lens or generic-optics with 'preferredAuthenticationMethod' instead"  #-}

-- | para>5 - Low priority, the notification might be delayed, delivered as part of a group, or throttled.
--
-- /listitem> 
--     * 10 - High priority, the notification is sent immediately. This is the default value. A high priority notification should trigger an alert, play a sound, or badge your app's icon on the recipient's device.
--
-- /para> Amazon Pinpoint specifies this value in the apns-priority request header when it sends the notification message to APNs.
-- The equivalent values for Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), are normal, for 5, and high, for 10. If you specify an FCM value for this property, Amazon Pinpoint accepts and converts the value to the corresponding APNs value.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmPriority :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmPriority = Lens.field @"priority"
{-# INLINEABLE apnsmPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmRawContent :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmRawContent = Lens.field @"rawContent"
{-# INLINEABLE apnsmRawContent #-}
{-# DEPRECATED rawContent "Use generic-lens or generic-optics with 'rawContent' instead"  #-}

-- | Specifies whether the notification is a silent push notification. A silent (or background) push notification isn't displayed on recipients' devices. You can use silent push notifications to make small updates to your app, or to display messages in an in-app message center.
--
-- Amazon Pinpoint uses this property to determine the correct value for the apns-push-type request header when it sends the notification message to APNs. If you specify a value of true for this property, Amazon Pinpoint sets the value for the apns-push-type header field to background.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmSilentPush :: Lens.Lens' APNSMessage (Core.Maybe Core.Bool)
apnsmSilentPush = Lens.field @"silentPush"
{-# INLINEABLE apnsmSilentPush #-}
{-# DEPRECATED silentPush "Use generic-lens or generic-optics with 'silentPush' instead"  #-}

-- | The key for the sound to play when the recipient receives the push notification. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmSound :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmSound = Lens.field @"sound"
{-# INLINEABLE apnsmSound #-}
{-# DEPRECATED sound "Use generic-lens or generic-optics with 'sound' instead"  #-}

-- | The default message variables to use in the notification message. You can override these default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmSubstitutions :: Lens.Lens' APNSMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
apnsmSubstitutions = Lens.field @"substitutions"
{-# INLINEABLE apnsmSubstitutions #-}
{-# DEPRECATED substitutions "Use generic-lens or generic-optics with 'substitutions' instead"  #-}

-- | The key that represents your app-specific identifier for grouping notifications. If you provide a Notification Content app extension, you can use this value to group your notifications together.
--
-- /Note:/ Consider using 'threadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmThreadId :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmThreadId = Lens.field @"threadId"
{-# INLINEABLE apnsmThreadId #-}
{-# DEPRECATED threadId "Use generic-lens or generic-optics with 'threadId' instead"  #-}

-- | The amount of time, in seconds, that APNs should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If this value is 0, APNs treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- Amazon Pinpoint specifies this value in the apns-expiration request header when it sends the notification message to APNs.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmTimeToLive :: Lens.Lens' APNSMessage (Core.Maybe Core.Int)
apnsmTimeToLive = Lens.field @"timeToLive"
{-# INLINEABLE apnsmTimeToLive #-}
{-# DEPRECATED timeToLive "Use generic-lens or generic-optics with 'timeToLive' instead"  #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmTitle :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmTitle = Lens.field @"title"
{-# INLINEABLE apnsmTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnsmUrl :: Lens.Lens' APNSMessage (Core.Maybe Core.Text)
apnsmUrl = Lens.field @"url"
{-# INLINEABLE apnsmUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON APNSMessage where
        toJSON APNSMessage{..}
          = Core.object
              (Core.catMaybes
                 [("APNSPushType" Core..=) Core.<$> aPNSPushType,
                  ("Action" Core..=) Core.<$> action,
                  ("Badge" Core..=) Core.<$> badge, ("Body" Core..=) Core.<$> body,
                  ("Category" Core..=) Core.<$> category,
                  ("CollapseId" Core..=) Core.<$> collapseId,
                  ("Data" Core..=) Core.<$> data',
                  ("MediaUrl" Core..=) Core.<$> mediaUrl,
                  ("PreferredAuthenticationMethod" Core..=) Core.<$>
                    preferredAuthenticationMethod,
                  ("Priority" Core..=) Core.<$> priority,
                  ("RawContent" Core..=) Core.<$> rawContent,
                  ("SilentPush" Core..=) Core.<$> silentPush,
                  ("Sound" Core..=) Core.<$> sound,
                  ("Substitutions" Core..=) Core.<$> substitutions,
                  ("ThreadId" Core..=) Core.<$> threadId,
                  ("TimeToLive" Core..=) Core.<$> timeToLive,
                  ("Title" Core..=) Core.<$> title, ("Url" Core..=) Core.<$> url])
