{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
  ( AndroidPushNotificationTemplate (..)
  -- * Smart constructor
  , mkAndroidPushNotificationTemplate
  -- * Lenses
  , apntAction
  , apntBody
  , apntImageIconUrl
  , apntImageUrl
  , apntRawContent
  , apntSmallImageIconUrl
  , apntSound
  , apntTitle
  , apntUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies channel-specific content and settings for a message template that can be used in push notifications that are sent through the ADM (Amazon Device Messaging), Baidu (Baidu Cloud Push), or GCM (Firebase Cloud Messaging, formerly Google Cloud Messaging) channel.
--
-- /See:/ 'mkAndroidPushNotificationTemplate' smart constructor.
data AndroidPushNotificationTemplate = AndroidPushNotificationTemplate'
  { action :: Core.Maybe Types.Action
    -- ^ The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
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
    -- ^ The message body to use in a push notification that's based on the message template.
  , imageIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the large icon image to display in the content view of a push notification that's based on the message template.
  , imageUrl :: Core.Maybe Core.Text
    -- ^ The URL of an image to display in a push notification that's based on the message template.
  , rawContent :: Core.Maybe Core.Text
    -- ^ The raw, JSON-formatted string to use as the payload for a push notification that's based on the message template. If specified, this value overrides all other content for the message template.
  , smallImageIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the small icon image to display in the status bar and the content view of a push notification that's based on the message template.
  , sound :: Core.Maybe Core.Text
    -- ^ The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
  , title :: Core.Maybe Core.Text
    -- ^ The title to use in a push notification that's based on the message template. This title appears above the notification message on a recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AndroidPushNotificationTemplate' value with any optional fields omitted.
mkAndroidPushNotificationTemplate
    :: AndroidPushNotificationTemplate
mkAndroidPushNotificationTemplate
  = AndroidPushNotificationTemplate'{action = Core.Nothing,
                                     body = Core.Nothing, imageIconUrl = Core.Nothing,
                                     imageUrl = Core.Nothing, rawContent = Core.Nothing,
                                     smallImageIconUrl = Core.Nothing, sound = Core.Nothing,
                                     title = Core.Nothing, url = Core.Nothing}

-- | The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
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
apntAction :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Types.Action)
apntAction = Lens.field @"action"
{-# INLINEABLE apntAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The message body to use in a push notification that's based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntBody :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntBody = Lens.field @"body"
{-# INLINEABLE apntBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The URL of the large icon image to display in the content view of a push notification that's based on the message template.
--
-- /Note:/ Consider using 'imageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntImageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntImageIconUrl = Lens.field @"imageIconUrl"
{-# INLINEABLE apntImageIconUrl #-}
{-# DEPRECATED imageIconUrl "Use generic-lens or generic-optics with 'imageIconUrl' instead"  #-}

-- | The URL of an image to display in a push notification that's based on the message template.
--
-- /Note:/ Consider using 'imageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntImageUrl :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntImageUrl = Lens.field @"imageUrl"
{-# INLINEABLE apntImageUrl #-}
{-# DEPRECATED imageUrl "Use generic-lens or generic-optics with 'imageUrl' instead"  #-}

-- | The raw, JSON-formatted string to use as the payload for a push notification that's based on the message template. If specified, this value overrides all other content for the message template.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntRawContent :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntRawContent = Lens.field @"rawContent"
{-# INLINEABLE apntRawContent #-}
{-# DEPRECATED rawContent "Use generic-lens or generic-optics with 'rawContent' instead"  #-}

-- | The URL of the small icon image to display in the status bar and the content view of a push notification that's based on the message template.
--
-- /Note:/ Consider using 'smallImageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntSmallImageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntSmallImageIconUrl = Lens.field @"smallImageIconUrl"
{-# INLINEABLE apntSmallImageIconUrl #-}
{-# DEPRECATED smallImageIconUrl "Use generic-lens or generic-optics with 'smallImageIconUrl' instead"  #-}

-- | The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntSound :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntSound = Lens.field @"sound"
{-# INLINEABLE apntSound #-}
{-# DEPRECATED sound "Use generic-lens or generic-optics with 'sound' instead"  #-}

-- | The title to use in a push notification that's based on the message template. This title appears above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntTitle :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntTitle = Lens.field @"title"
{-# INLINEABLE apntTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntUrl :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
apntUrl = Lens.field @"url"
{-# INLINEABLE apntUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON AndroidPushNotificationTemplate where
        toJSON AndroidPushNotificationTemplate{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Body" Core..=) Core.<$> body,
                  ("ImageIconUrl" Core..=) Core.<$> imageIconUrl,
                  ("ImageUrl" Core..=) Core.<$> imageUrl,
                  ("RawContent" Core..=) Core.<$> rawContent,
                  ("SmallImageIconUrl" Core..=) Core.<$> smallImageIconUrl,
                  ("Sound" Core..=) Core.<$> sound, ("Title" Core..=) Core.<$> title,
                  ("Url" Core..=) Core.<$> url])

instance Core.FromJSON AndroidPushNotificationTemplate where
        parseJSON
          = Core.withObject "AndroidPushNotificationTemplate" Core.$
              \ x ->
                AndroidPushNotificationTemplate' Core.<$>
                  (x Core..:? "Action") Core.<*> x Core..:? "Body" Core.<*>
                    x Core..:? "ImageIconUrl"
                    Core.<*> x Core..:? "ImageUrl"
                    Core.<*> x Core..:? "RawContent"
                    Core.<*> x Core..:? "SmallImageIconUrl"
                    Core.<*> x Core..:? "Sound"
                    Core.<*> x Core..:? "Title"
                    Core.<*> x Core..:? "Url"
