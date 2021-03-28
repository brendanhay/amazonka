{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
  ( DefaultPushNotificationTemplate (..)
  -- * Smart constructor
  , mkDefaultPushNotificationTemplate
  -- * Lenses
  , dpntAction
  , dpntBody
  , dpntSound
  , dpntTitle
  , dpntUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the default settings and content for a message template that can be used in messages that are sent through a push notification channel.
--
-- /See:/ 'mkDefaultPushNotificationTemplate' smart constructor.
data DefaultPushNotificationTemplate = DefaultPushNotificationTemplate'
  { action :: Core.Maybe Types.Action
    -- ^ The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS and Android platforms.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
  , body :: Core.Maybe Core.Text
    -- ^ The message body to use in push notifications that are based on the message template.
  , sound :: Core.Maybe Core.Text
    -- ^ The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- For an iOS platform, this value is the key for the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
  , title :: Core.Maybe Core.Text
    -- ^ The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultPushNotificationTemplate' value with any optional fields omitted.
mkDefaultPushNotificationTemplate
    :: DefaultPushNotificationTemplate
mkDefaultPushNotificationTemplate
  = DefaultPushNotificationTemplate'{action = Core.Nothing,
                                     body = Core.Nothing, sound = Core.Nothing,
                                     title = Core.Nothing, url = Core.Nothing}

-- | The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of the iOS and Android platforms.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntAction :: Lens.Lens' DefaultPushNotificationTemplate (Core.Maybe Types.Action)
dpntAction = Lens.field @"action"
{-# INLINEABLE dpntAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The message body to use in push notifications that are based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntBody :: Lens.Lens' DefaultPushNotificationTemplate (Core.Maybe Core.Text)
dpntBody = Lens.field @"body"
{-# INLINEABLE dpntBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- For an iOS platform, this value is the key for the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntSound :: Lens.Lens' DefaultPushNotificationTemplate (Core.Maybe Core.Text)
dpntSound = Lens.field @"sound"
{-# INLINEABLE dpntSound #-}
{-# DEPRECATED sound "Use generic-lens or generic-optics with 'sound' instead"  #-}

-- | The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntTitle :: Lens.Lens' DefaultPushNotificationTemplate (Core.Maybe Core.Text)
dpntTitle = Lens.field @"title"
{-# INLINEABLE dpntTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntUrl :: Lens.Lens' DefaultPushNotificationTemplate (Core.Maybe Core.Text)
dpntUrl = Lens.field @"url"
{-# INLINEABLE dpntUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON DefaultPushNotificationTemplate where
        toJSON DefaultPushNotificationTemplate{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Body" Core..=) Core.<$> body, ("Sound" Core..=) Core.<$> sound,
                  ("Title" Core..=) Core.<$> title, ("Url" Core..=) Core.<$> url])

instance Core.FromJSON DefaultPushNotificationTemplate where
        parseJSON
          = Core.withObject "DefaultPushNotificationTemplate" Core.$
              \ x ->
                DefaultPushNotificationTemplate' Core.<$>
                  (x Core..:? "Action") Core.<*> x Core..:? "Body" Core.<*>
                    x Core..:? "Sound"
                    Core.<*> x Core..:? "Title"
                    Core.<*> x Core..:? "Url"
