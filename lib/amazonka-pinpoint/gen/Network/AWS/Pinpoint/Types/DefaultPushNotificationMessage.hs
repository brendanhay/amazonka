{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
  ( DefaultPushNotificationMessage (..)
  -- * Smart constructor
  , mkDefaultPushNotificationMessage
  -- * Lenses
  , dpnmAction
  , dpnmBody
  , dpnmData
  , dpnmSilentPush
  , dpnmSubstitutions
  , dpnmTitle
  , dpnmUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the default settings and content for a push notification that's sent directly to an endpoint.
--
-- /See:/ 'mkDefaultPushNotificationMessage' smart constructor.
data DefaultPushNotificationMessage = DefaultPushNotificationMessage'
  { action :: Core.Maybe Types.Action
    -- ^ The default action to occur if a recipient taps the push notification. Valid values are:
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
    -- ^ The default body of the notification message.
  , data' :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The JSON data payload to use for the default push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
  , silentPush :: Core.Maybe Core.Bool
    -- ^ Specifies whether the default notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or delivering messages to an in-app notification center.
  , substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The default message variables to use in the notification message. You can override the default variables with individual address variables.
  , title :: Core.Maybe Core.Text
    -- ^ The default title to display above the notification message on a recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The default URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultPushNotificationMessage' value with any optional fields omitted.
mkDefaultPushNotificationMessage
    :: DefaultPushNotificationMessage
mkDefaultPushNotificationMessage
  = DefaultPushNotificationMessage'{action = Core.Nothing,
                                    body = Core.Nothing, data' = Core.Nothing,
                                    silentPush = Core.Nothing, substitutions = Core.Nothing,
                                    title = Core.Nothing, url = Core.Nothing}

-- | The default action to occur if a recipient taps the push notification. Valid values are:
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
dpnmAction :: Lens.Lens' DefaultPushNotificationMessage (Core.Maybe Types.Action)
dpnmAction = Lens.field @"action"
{-# INLINEABLE dpnmAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The default body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmBody :: Lens.Lens' DefaultPushNotificationMessage (Core.Maybe Core.Text)
dpnmBody = Lens.field @"body"
{-# INLINEABLE dpnmBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The JSON data payload to use for the default push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmData :: Lens.Lens' DefaultPushNotificationMessage (Core.Maybe (Core.HashMap Core.Text Core.Text))
dpnmData = Lens.field @"data'"
{-# INLINEABLE dpnmData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | Specifies whether the default notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or delivering messages to an in-app notification center.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmSilentPush :: Lens.Lens' DefaultPushNotificationMessage (Core.Maybe Core.Bool)
dpnmSilentPush = Lens.field @"silentPush"
{-# INLINEABLE dpnmSilentPush #-}
{-# DEPRECATED silentPush "Use generic-lens or generic-optics with 'silentPush' instead"  #-}

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmSubstitutions :: Lens.Lens' DefaultPushNotificationMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
dpnmSubstitutions = Lens.field @"substitutions"
{-# INLINEABLE dpnmSubstitutions #-}
{-# DEPRECATED substitutions "Use generic-lens or generic-optics with 'substitutions' instead"  #-}

-- | The default title to display above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmTitle :: Lens.Lens' DefaultPushNotificationMessage (Core.Maybe Core.Text)
dpnmTitle = Lens.field @"title"
{-# INLINEABLE dpnmTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The default URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmUrl :: Lens.Lens' DefaultPushNotificationMessage (Core.Maybe Core.Text)
dpnmUrl = Lens.field @"url"
{-# INLINEABLE dpnmUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON DefaultPushNotificationMessage where
        toJSON DefaultPushNotificationMessage{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Body" Core..=) Core.<$> body, ("Data" Core..=) Core.<$> data',
                  ("SilentPush" Core..=) Core.<$> silentPush,
                  ("Substitutions" Core..=) Core.<$> substitutions,
                  ("Title" Core..=) Core.<$> title, ("Url" Core..=) Core.<$> url])
