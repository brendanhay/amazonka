{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.GCMMessage
  ( GCMMessage (..)
  -- * Smart constructor
  , mkGCMMessage
  -- * Lenses
  , gcmmAction
  , gcmmBody
  , gcmmCollapseKey
  , gcmmData
  , gcmmIconReference
  , gcmmImageIconUrl
  , gcmmImageUrl
  , gcmmPriority
  , gcmmRawContent
  , gcmmRestrictedPackageName
  , gcmmSilentPush
  , gcmmSmallImageIconUrl
  , gcmmSound
  , gcmmSubstitutions
  , gcmmTimeToLive
  , gcmmTitle
  , gcmmUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the GCM channel. The GCM channel enables Amazon Pinpoint to send messages to the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'mkGCMMessage' smart constructor.
data GCMMessage = GCMMessage'
  { action :: Core.Maybe Types.Action
    -- ^ The action to occur if the recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
  , body :: Core.Maybe Core.Text
    -- ^ The body of the notification message.
  , collapseKey :: Core.Maybe Core.Text
    -- ^ An arbitrary string that identifies a group of messages that can be collapsed to ensure that only the last message is sent when delivery can resume. This helps avoid sending too many instances of the same messages when the recipient's device comes online again or becomes active.
--
-- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging (FCM) collapse_key parameter when it sends the notification message to FCM.
  , data' :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
  , iconReference :: Core.Maybe Core.Text
    -- ^ The icon image name of the asset saved in your app.
  , imageIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the large icon image to display in the content view of the push notification.
  , imageUrl :: Core.Maybe Core.Text
    -- ^ The URL of an image to display in the push notification.
  , priority :: Core.Maybe Core.Text
    -- ^ para>normal - The notification might be delayed. Delivery is optimized for battery usage on the recipient's device. Use this value unless immediate delivery is required.
--
-- /listitem> 
--     * high - The notification is sent immediately and might wake a sleeping device.
--
-- /para> Amazon Pinpoint specifies this value in the FCM priority parameter when it sends the notification message to FCM.
-- The equivalent values for Apple Push Notification service (APNs) are 5, for normal, and 10, for high. If you specify an APNs value for this property, Amazon Pinpoint accepts and converts the value to the corresponding FCM value.
  , rawContent :: Core.Maybe Core.Text
    -- ^ The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
  , restrictedPackageName :: Core.Maybe Core.Text
    -- ^ The package name of the application where registration tokens must match in order for the recipient to receive the message.
  , silentPush :: Core.Maybe Core.Bool
    -- ^ Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
  , smallImageIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the small icon image to display in the status bar and the content view of the push notification.
  , sound :: Core.Maybe Core.Text
    -- ^ The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
  , substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The default message variables to use in the notification message. You can override the default variables with individual address variables.
  , timeToLive :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, that FCM should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If you don't specify this value, FCM defaults to the maximum value, which is 2,419,200 seconds (28 days).
--
-- Amazon Pinpoint specifies this value in the FCM time_to_live parameter when it sends the notification message to FCM.
  , title :: Core.Maybe Core.Text
    -- ^ The title to display above the notification message on the recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GCMMessage' value with any optional fields omitted.
mkGCMMessage
    :: GCMMessage
mkGCMMessage
  = GCMMessage'{action = Core.Nothing, body = Core.Nothing,
                collapseKey = Core.Nothing, data' = Core.Nothing,
                iconReference = Core.Nothing, imageIconUrl = Core.Nothing,
                imageUrl = Core.Nothing, priority = Core.Nothing,
                rawContent = Core.Nothing, restrictedPackageName = Core.Nothing,
                silentPush = Core.Nothing, smallImageIconUrl = Core.Nothing,
                sound = Core.Nothing, substitutions = Core.Nothing,
                timeToLive = Core.Nothing, title = Core.Nothing,
                url = Core.Nothing}

-- | The action to occur if the recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmAction :: Lens.Lens' GCMMessage (Core.Maybe Types.Action)
gcmmAction = Lens.field @"action"
{-# INLINEABLE gcmmAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmBody :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmBody = Lens.field @"body"
{-# INLINEABLE gcmmBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | An arbitrary string that identifies a group of messages that can be collapsed to ensure that only the last message is sent when delivery can resume. This helps avoid sending too many instances of the same messages when the recipient's device comes online again or becomes active.
--
-- Amazon Pinpoint specifies this value in the Firebase Cloud Messaging (FCM) collapse_key parameter when it sends the notification message to FCM.
--
-- /Note:/ Consider using 'collapseKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmCollapseKey :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmCollapseKey = Lens.field @"collapseKey"
{-# INLINEABLE gcmmCollapseKey #-}
{-# DEPRECATED collapseKey "Use generic-lens or generic-optics with 'collapseKey' instead"  #-}

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmData :: Lens.Lens' GCMMessage (Core.Maybe (Core.HashMap Core.Text Core.Text))
gcmmData = Lens.field @"data'"
{-# INLINEABLE gcmmData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The icon image name of the asset saved in your app.
--
-- /Note:/ Consider using 'iconReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmIconReference :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmIconReference = Lens.field @"iconReference"
{-# INLINEABLE gcmmIconReference #-}
{-# DEPRECATED iconReference "Use generic-lens or generic-optics with 'iconReference' instead"  #-}

-- | The URL of the large icon image to display in the content view of the push notification.
--
-- /Note:/ Consider using 'imageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmImageIconUrl :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmImageIconUrl = Lens.field @"imageIconUrl"
{-# INLINEABLE gcmmImageIconUrl #-}
{-# DEPRECATED imageIconUrl "Use generic-lens or generic-optics with 'imageIconUrl' instead"  #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmImageUrl :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmImageUrl = Lens.field @"imageUrl"
{-# INLINEABLE gcmmImageUrl #-}
{-# DEPRECATED imageUrl "Use generic-lens or generic-optics with 'imageUrl' instead"  #-}

-- | para>normal - The notification might be delayed. Delivery is optimized for battery usage on the recipient's device. Use this value unless immediate delivery is required.
--
-- /listitem> 
--     * high - The notification is sent immediately and might wake a sleeping device.
--
-- /para> Amazon Pinpoint specifies this value in the FCM priority parameter when it sends the notification message to FCM.
-- The equivalent values for Apple Push Notification service (APNs) are 5, for normal, and 10, for high. If you specify an APNs value for this property, Amazon Pinpoint accepts and converts the value to the corresponding FCM value.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmPriority :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmPriority = Lens.field @"priority"
{-# INLINEABLE gcmmPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmRawContent :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmRawContent = Lens.field @"rawContent"
{-# INLINEABLE gcmmRawContent #-}
{-# DEPRECATED rawContent "Use generic-lens or generic-optics with 'rawContent' instead"  #-}

-- | The package name of the application where registration tokens must match in order for the recipient to receive the message.
--
-- /Note:/ Consider using 'restrictedPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmRestrictedPackageName :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmRestrictedPackageName = Lens.field @"restrictedPackageName"
{-# INLINEABLE gcmmRestrictedPackageName #-}
{-# DEPRECATED restrictedPackageName "Use generic-lens or generic-optics with 'restrictedPackageName' instead"  #-}

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmSilentPush :: Lens.Lens' GCMMessage (Core.Maybe Core.Bool)
gcmmSilentPush = Lens.field @"silentPush"
{-# INLINEABLE gcmmSilentPush #-}
{-# DEPRECATED silentPush "Use generic-lens or generic-optics with 'silentPush' instead"  #-}

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- /Note:/ Consider using 'smallImageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmSmallImageIconUrl :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmSmallImageIconUrl = Lens.field @"smallImageIconUrl"
{-# INLINEABLE gcmmSmallImageIconUrl #-}
{-# DEPRECATED smallImageIconUrl "Use generic-lens or generic-optics with 'smallImageIconUrl' instead"  #-}

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmSound :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmSound = Lens.field @"sound"
{-# INLINEABLE gcmmSound #-}
{-# DEPRECATED sound "Use generic-lens or generic-optics with 'sound' instead"  #-}

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmSubstitutions :: Lens.Lens' GCMMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
gcmmSubstitutions = Lens.field @"substitutions"
{-# INLINEABLE gcmmSubstitutions #-}
{-# DEPRECATED substitutions "Use generic-lens or generic-optics with 'substitutions' instead"  #-}

-- | The amount of time, in seconds, that FCM should store and attempt to deliver the push notification, if the service is unable to deliver the notification the first time. If you don't specify this value, FCM defaults to the maximum value, which is 2,419,200 seconds (28 days).
--
-- Amazon Pinpoint specifies this value in the FCM time_to_live parameter when it sends the notification message to FCM.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmTimeToLive :: Lens.Lens' GCMMessage (Core.Maybe Core.Int)
gcmmTimeToLive = Lens.field @"timeToLive"
{-# INLINEABLE gcmmTimeToLive #-}
{-# DEPRECATED timeToLive "Use generic-lens or generic-optics with 'timeToLive' instead"  #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmTitle :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmTitle = Lens.field @"title"
{-# INLINEABLE gcmmTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmmUrl :: Lens.Lens' GCMMessage (Core.Maybe Core.Text)
gcmmUrl = Lens.field @"url"
{-# INLINEABLE gcmmUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON GCMMessage where
        toJSON GCMMessage{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Body" Core..=) Core.<$> body,
                  ("CollapseKey" Core..=) Core.<$> collapseKey,
                  ("Data" Core..=) Core.<$> data',
                  ("IconReference" Core..=) Core.<$> iconReference,
                  ("ImageIconUrl" Core..=) Core.<$> imageIconUrl,
                  ("ImageUrl" Core..=) Core.<$> imageUrl,
                  ("Priority" Core..=) Core.<$> priority,
                  ("RawContent" Core..=) Core.<$> rawContent,
                  ("RestrictedPackageName" Core..=) Core.<$> restrictedPackageName,
                  ("SilentPush" Core..=) Core.<$> silentPush,
                  ("SmallImageIconUrl" Core..=) Core.<$> smallImageIconUrl,
                  ("Sound" Core..=) Core.<$> sound,
                  ("Substitutions" Core..=) Core.<$> substitutions,
                  ("TimeToLive" Core..=) Core.<$> timeToLive,
                  ("Title" Core..=) Core.<$> title, ("Url" Core..=) Core.<$> url])
