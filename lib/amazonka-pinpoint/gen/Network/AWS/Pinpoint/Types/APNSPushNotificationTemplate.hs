{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
  ( APNSPushNotificationTemplate (..)
  -- * Smart constructor
  , mkAPNSPushNotificationTemplate
  -- * Lenses
  , apnspntAction
  , apnspntBody
  , apnspntMediaUrl
  , apnspntRawContent
  , apnspntSound
  , apnspntTitle
  , apnspntUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies channel-specific content and settings for a message template that can be used in push notifications that are sent through the APNs (Apple Push Notification service) channel.
--
-- /See:/ 'mkAPNSPushNotificationTemplate' smart constructor.
data APNSPushNotificationTemplate = APNSPushNotificationTemplate'
  { action :: Core.Maybe Types.Action
    -- ^ The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
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
  , body :: Core.Maybe Core.Text
    -- ^ The message body to use in push notifications that are based on the message template.
  , mediaUrl :: Core.Maybe Core.Text
    -- ^ The URL of an image or video to display in push notifications that are based on the message template.
  , rawContent :: Core.Maybe Core.Text
    -- ^ The raw, JSON-formatted string to use as the payload for push notifications that are based on the message template. If specified, this value overrides all other content for the message template.
  , sound :: Core.Maybe Core.Text
    -- ^ The key for the sound to play when the recipient receives a push notification that's based on the message template. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
  , title :: Core.Maybe Core.Text
    -- ^ The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to open in the recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'APNSPushNotificationTemplate' value with any optional fields omitted.
mkAPNSPushNotificationTemplate
    :: APNSPushNotificationTemplate
mkAPNSPushNotificationTemplate
  = APNSPushNotificationTemplate'{action = Core.Nothing,
                                  body = Core.Nothing, mediaUrl = Core.Nothing,
                                  rawContent = Core.Nothing, sound = Core.Nothing,
                                  title = Core.Nothing, url = Core.Nothing}

-- | The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
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
apnspntAction :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Types.Action)
apnspntAction = Lens.field @"action"
{-# INLINEABLE apnspntAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The message body to use in push notifications that are based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnspntBody :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
apnspntBody = Lens.field @"body"
{-# INLINEABLE apnspntBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The URL of an image or video to display in push notifications that are based on the message template.
--
-- /Note:/ Consider using 'mediaUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnspntMediaUrl :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
apnspntMediaUrl = Lens.field @"mediaUrl"
{-# INLINEABLE apnspntMediaUrl #-}
{-# DEPRECATED mediaUrl "Use generic-lens or generic-optics with 'mediaUrl' instead"  #-}

-- | The raw, JSON-formatted string to use as the payload for push notifications that are based on the message template. If specified, this value overrides all other content for the message template.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnspntRawContent :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
apnspntRawContent = Lens.field @"rawContent"
{-# INLINEABLE apnspntRawContent #-}
{-# DEPRECATED rawContent "Use generic-lens or generic-optics with 'rawContent' instead"  #-}

-- | The key for the sound to play when the recipient receives a push notification that's based on the message template. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnspntSound :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
apnspntSound = Lens.field @"sound"
{-# INLINEABLE apnspntSound #-}
{-# DEPRECATED sound "Use generic-lens or generic-optics with 'sound' instead"  #-}

-- | The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnspntTitle :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
apnspntTitle = Lens.field @"title"
{-# INLINEABLE apnspntTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apnspntUrl :: Lens.Lens' APNSPushNotificationTemplate (Core.Maybe Core.Text)
apnspntUrl = Lens.field @"url"
{-# INLINEABLE apnspntUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON APNSPushNotificationTemplate where
        toJSON APNSPushNotificationTemplate{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Body" Core..=) Core.<$> body,
                  ("MediaUrl" Core..=) Core.<$> mediaUrl,
                  ("RawContent" Core..=) Core.<$> rawContent,
                  ("Sound" Core..=) Core.<$> sound, ("Title" Core..=) Core.<$> title,
                  ("Url" Core..=) Core.<$> url])

instance Core.FromJSON APNSPushNotificationTemplate where
        parseJSON
          = Core.withObject "APNSPushNotificationTemplate" Core.$
              \ x ->
                APNSPushNotificationTemplate' Core.<$>
                  (x Core..:? "Action") Core.<*> x Core..:? "Body" Core.<*>
                    x Core..:? "MediaUrl"
                    Core.<*> x Core..:? "RawContent"
                    Core.<*> x Core..:? "Sound"
                    Core.<*> x Core..:? "Title"
                    Core.<*> x Core..:? "Url"
