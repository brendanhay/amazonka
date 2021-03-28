{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Message
  ( Message (..)
  -- * Smart constructor
  , mkMessage
  -- * Lenses
  , mAction
  , mBody
  , mImageIconUrl
  , mImageSmallIconUrl
  , mImageUrl
  , mJsonBody
  , mMediaUrl
  , mRawContent
  , mSilentPush
  , mTimeToLive
  , mTitle
  , mUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the content and settings for a push notification that's sent to recipients of a campaign.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { action :: Core.Maybe Types.Action
    -- ^ The action to occur if a recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of iOS and Android.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
  , body :: Core.Maybe Core.Text
    -- ^ The body of the notification message. The maximum number of characters is 200.
  , imageIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the image to display as the push-notification icon, such as the icon for the app.
  , imageSmallIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the image to display as the small, push-notification icon, such as a small version of the icon for the app.
  , imageUrl :: Core.Maybe Core.Text
    -- ^ The URL of an image to display in the push notification.
  , jsonBody :: Core.Maybe Core.Text
    -- ^ The JSON payload to use for a silent push notification.
  , mediaUrl :: Core.Maybe Core.Text
    -- ^ The URL of the image or video to display in the push notification.
  , rawContent :: Core.Maybe Core.Text
    -- ^ The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
  , silentPush :: Core.Maybe Core.Bool
    -- ^ Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration, displaying messages in an in-app message center, or supporting phone home functionality.
  , timeToLive :: Core.Maybe Core.Int
    -- ^ The number of seconds that the push-notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
  , title :: Core.Maybe Core.Text
    -- ^ The title to display above the notification message on a recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Message' value with any optional fields omitted.
mkMessage
    :: Message
mkMessage
  = Message'{action = Core.Nothing, body = Core.Nothing,
             imageIconUrl = Core.Nothing, imageSmallIconUrl = Core.Nothing,
             imageUrl = Core.Nothing, jsonBody = Core.Nothing,
             mediaUrl = Core.Nothing, rawContent = Core.Nothing,
             silentPush = Core.Nothing, timeToLive = Core.Nothing,
             title = Core.Nothing, url = Core.Nothing}

-- | The action to occur if a recipient taps the push notification. Valid values are:
--
--
--     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
--
--
--     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This setting uses the deep-linking features of iOS and Android.
--
--
--     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAction :: Lens.Lens' Message (Core.Maybe Types.Action)
mAction = Lens.field @"action"
{-# INLINEABLE mAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The body of the notification message. The maximum number of characters is 200.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBody :: Lens.Lens' Message (Core.Maybe Core.Text)
mBody = Lens.field @"body"
{-# INLINEABLE mBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The URL of the image to display as the push-notification icon, such as the icon for the app.
--
-- /Note:/ Consider using 'imageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mImageIconUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
mImageIconUrl = Lens.field @"imageIconUrl"
{-# INLINEABLE mImageIconUrl #-}
{-# DEPRECATED imageIconUrl "Use generic-lens or generic-optics with 'imageIconUrl' instead"  #-}

-- | The URL of the image to display as the small, push-notification icon, such as a small version of the icon for the app.
--
-- /Note:/ Consider using 'imageSmallIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mImageSmallIconUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
mImageSmallIconUrl = Lens.field @"imageSmallIconUrl"
{-# INLINEABLE mImageSmallIconUrl #-}
{-# DEPRECATED imageSmallIconUrl "Use generic-lens or generic-optics with 'imageSmallIconUrl' instead"  #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mImageUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
mImageUrl = Lens.field @"imageUrl"
{-# INLINEABLE mImageUrl #-}
{-# DEPRECATED imageUrl "Use generic-lens or generic-optics with 'imageUrl' instead"  #-}

-- | The JSON payload to use for a silent push notification.
--
-- /Note:/ Consider using 'jsonBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mJsonBody :: Lens.Lens' Message (Core.Maybe Core.Text)
mJsonBody = Lens.field @"jsonBody"
{-# INLINEABLE mJsonBody #-}
{-# DEPRECATED jsonBody "Use generic-lens or generic-optics with 'jsonBody' instead"  #-}

-- | The URL of the image or video to display in the push notification.
--
-- /Note:/ Consider using 'mediaUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMediaUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
mMediaUrl = Lens.field @"mediaUrl"
{-# INLINEABLE mMediaUrl #-}
{-# DEPRECATED mediaUrl "Use generic-lens or generic-optics with 'mediaUrl' instead"  #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRawContent :: Lens.Lens' Message (Core.Maybe Core.Text)
mRawContent = Lens.field @"rawContent"
{-# INLINEABLE mRawContent #-}
{-# DEPRECATED rawContent "Use generic-lens or generic-optics with 'rawContent' instead"  #-}

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration, displaying messages in an in-app message center, or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSilentPush :: Lens.Lens' Message (Core.Maybe Core.Bool)
mSilentPush = Lens.field @"silentPush"
{-# INLINEABLE mSilentPush #-}
{-# DEPRECATED silentPush "Use generic-lens or generic-optics with 'silentPush' instead"  #-}

-- | The number of seconds that the push-notification service should keep the message, if the service is unable to deliver the notification the first time. This value is converted to an expiration value when it's sent to a push-notification service. If this value is 0, the service treats the notification as if it expires immediately and the service doesn't store or try to deliver the notification again.
--
-- This value doesn't apply to messages that are sent through the Amazon Device Messaging (ADM) service.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTimeToLive :: Lens.Lens' Message (Core.Maybe Core.Int)
mTimeToLive = Lens.field @"timeToLive"
{-# INLINEABLE mTimeToLive #-}
{-# DEPRECATED timeToLive "Use generic-lens or generic-optics with 'timeToLive' instead"  #-}

-- | The title to display above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTitle :: Lens.Lens' Message (Core.Maybe Core.Text)
mTitle = Lens.field @"title"
{-# INLINEABLE mTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mUrl :: Lens.Lens' Message (Core.Maybe Core.Text)
mUrl = Lens.field @"url"
{-# INLINEABLE mUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON Message where
        toJSON Message{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Body" Core..=) Core.<$> body,
                  ("ImageIconUrl" Core..=) Core.<$> imageIconUrl,
                  ("ImageSmallIconUrl" Core..=) Core.<$> imageSmallIconUrl,
                  ("ImageUrl" Core..=) Core.<$> imageUrl,
                  ("JsonBody" Core..=) Core.<$> jsonBody,
                  ("MediaUrl" Core..=) Core.<$> mediaUrl,
                  ("RawContent" Core..=) Core.<$> rawContent,
                  ("SilentPush" Core..=) Core.<$> silentPush,
                  ("TimeToLive" Core..=) Core.<$> timeToLive,
                  ("Title" Core..=) Core.<$> title, ("Url" Core..=) Core.<$> url])

instance Core.FromJSON Message where
        parseJSON
          = Core.withObject "Message" Core.$
              \ x ->
                Message' Core.<$>
                  (x Core..:? "Action") Core.<*> x Core..:? "Body" Core.<*>
                    x Core..:? "ImageIconUrl"
                    Core.<*> x Core..:? "ImageSmallIconUrl"
                    Core.<*> x Core..:? "ImageUrl"
                    Core.<*> x Core..:? "JsonBody"
                    Core.<*> x Core..:? "MediaUrl"
                    Core.<*> x Core..:? "RawContent"
                    Core.<*> x Core..:? "SilentPush"
                    Core.<*> x Core..:? "TimeToLive"
                    Core.<*> x Core..:? "Title"
                    Core.<*> x Core..:? "Url"
