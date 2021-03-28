{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ADMMessage
  ( ADMMessage (..)
  -- * Smart constructor
  , mkADMMessage
  -- * Lenses
  , admmAction
  , admmBody
  , admmConsolidationKey
  , admmData
  , admmExpiresAfter
  , admmIconReference
  , admmImageIconUrl
  , admmImageUrl
  , admmMD5
  , admmRawContent
  , admmSilentPush
  , admmSmallImageIconUrl
  , admmSound
  , admmSubstitutions
  , admmTitle
  , admmUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the ADM (Amazon Device Messaging) channel.
--
-- /See:/ 'mkADMMessage' smart constructor.
data ADMMessage = ADMMessage'
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
  , consolidationKey :: Core.Maybe Core.Text
    -- ^ An arbitrary string that indicates that multiple messages are logically the same and that Amazon Device Messaging (ADM) can drop previously enqueued messages in favor of this message.
  , data' :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
  , expiresAfter :: Core.Maybe Core.Text
    -- ^ The amount of time, in seconds, that ADM should store the message if the recipient's device is offline. Amazon Pinpoint specifies this value in the expiresAfter parameter when it sends the notification message to ADM.
  , iconReference :: Core.Maybe Core.Text
    -- ^ The icon image name of the asset saved in your app.
  , imageIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the large icon image to display in the content view of the push notification.
  , imageUrl :: Core.Maybe Core.Text
    -- ^ The URL of an image to display in the push notification.
  , mD5 :: Core.Maybe Core.Text
    -- ^ The base64-encoded, MD5 checksum of the value specified by the Data property. ADM uses the MD5 value to verify the integrity of the data.
  , rawContent :: Core.Maybe Core.Text
    -- ^ The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
  , silentPush :: Core.Maybe Core.Bool
    -- ^ Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
  , smallImageIconUrl :: Core.Maybe Core.Text
    -- ^ The URL of the small icon image to display in the status bar and the content view of the push notification.
  , sound :: Core.Maybe Core.Text
    -- ^ The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
  , substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The default message variables to use in the notification message. You can override the default variables with individual address variables.
  , title :: Core.Maybe Core.Text
    -- ^ The title to display above the notification message on the recipient's device.
  , url :: Core.Maybe Core.Text
    -- ^ The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ADMMessage' value with any optional fields omitted.
mkADMMessage
    :: ADMMessage
mkADMMessage
  = ADMMessage'{action = Core.Nothing, body = Core.Nothing,
                consolidationKey = Core.Nothing, data' = Core.Nothing,
                expiresAfter = Core.Nothing, iconReference = Core.Nothing,
                imageIconUrl = Core.Nothing, imageUrl = Core.Nothing,
                mD5 = Core.Nothing, rawContent = Core.Nothing,
                silentPush = Core.Nothing, smallImageIconUrl = Core.Nothing,
                sound = Core.Nothing, substitutions = Core.Nothing,
                title = Core.Nothing, url = Core.Nothing}

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
admmAction :: Lens.Lens' ADMMessage (Core.Maybe Types.Action)
admmAction = Lens.field @"action"
{-# INLINEABLE admmAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmBody :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmBody = Lens.field @"body"
{-# INLINEABLE admmBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | An arbitrary string that indicates that multiple messages are logically the same and that Amazon Device Messaging (ADM) can drop previously enqueued messages in favor of this message.
--
-- /Note:/ Consider using 'consolidationKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmConsolidationKey :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmConsolidationKey = Lens.field @"consolidationKey"
{-# INLINEABLE admmConsolidationKey #-}
{-# DEPRECATED consolidationKey "Use generic-lens or generic-optics with 'consolidationKey' instead"  #-}

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmData :: Lens.Lens' ADMMessage (Core.Maybe (Core.HashMap Core.Text Core.Text))
admmData = Lens.field @"data'"
{-# INLINEABLE admmData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The amount of time, in seconds, that ADM should store the message if the recipient's device is offline. Amazon Pinpoint specifies this value in the expiresAfter parameter when it sends the notification message to ADM.
--
-- /Note:/ Consider using 'expiresAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmExpiresAfter :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmExpiresAfter = Lens.field @"expiresAfter"
{-# INLINEABLE admmExpiresAfter #-}
{-# DEPRECATED expiresAfter "Use generic-lens or generic-optics with 'expiresAfter' instead"  #-}

-- | The icon image name of the asset saved in your app.
--
-- /Note:/ Consider using 'iconReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmIconReference :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmIconReference = Lens.field @"iconReference"
{-# INLINEABLE admmIconReference #-}
{-# DEPRECATED iconReference "Use generic-lens or generic-optics with 'iconReference' instead"  #-}

-- | The URL of the large icon image to display in the content view of the push notification.
--
-- /Note:/ Consider using 'imageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmImageIconUrl :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmImageIconUrl = Lens.field @"imageIconUrl"
{-# INLINEABLE admmImageIconUrl #-}
{-# DEPRECATED imageIconUrl "Use generic-lens or generic-optics with 'imageIconUrl' instead"  #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmImageUrl :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmImageUrl = Lens.field @"imageUrl"
{-# INLINEABLE admmImageUrl #-}
{-# DEPRECATED imageUrl "Use generic-lens or generic-optics with 'imageUrl' instead"  #-}

-- | The base64-encoded, MD5 checksum of the value specified by the Data property. ADM uses the MD5 value to verify the integrity of the data.
--
-- /Note:/ Consider using 'mD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmMD5 :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmMD5 = Lens.field @"mD5"
{-# INLINEABLE admmMD5 #-}
{-# DEPRECATED mD5 "Use generic-lens or generic-optics with 'mD5' instead"  #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmRawContent :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmRawContent = Lens.field @"rawContent"
{-# INLINEABLE admmRawContent #-}
{-# DEPRECATED rawContent "Use generic-lens or generic-optics with 'rawContent' instead"  #-}

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSilentPush :: Lens.Lens' ADMMessage (Core.Maybe Core.Bool)
admmSilentPush = Lens.field @"silentPush"
{-# INLINEABLE admmSilentPush #-}
{-# DEPRECATED silentPush "Use generic-lens or generic-optics with 'silentPush' instead"  #-}

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- /Note:/ Consider using 'smallImageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSmallImageIconUrl :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmSmallImageIconUrl = Lens.field @"smallImageIconUrl"
{-# INLINEABLE admmSmallImageIconUrl #-}
{-# DEPRECATED smallImageIconUrl "Use generic-lens or generic-optics with 'smallImageIconUrl' instead"  #-}

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSound :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmSound = Lens.field @"sound"
{-# INLINEABLE admmSound #-}
{-# DEPRECATED sound "Use generic-lens or generic-optics with 'sound' instead"  #-}

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSubstitutions :: Lens.Lens' ADMMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
admmSubstitutions = Lens.field @"substitutions"
{-# INLINEABLE admmSubstitutions #-}
{-# DEPRECATED substitutions "Use generic-lens or generic-optics with 'substitutions' instead"  #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmTitle :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmTitle = Lens.field @"title"
{-# INLINEABLE admmTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmUrl :: Lens.Lens' ADMMessage (Core.Maybe Core.Text)
admmUrl = Lens.field @"url"
{-# INLINEABLE admmUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON ADMMessage where
        toJSON ADMMessage{..}
          = Core.object
              (Core.catMaybes
                 [("Action" Core..=) Core.<$> action,
                  ("Body" Core..=) Core.<$> body,
                  ("ConsolidationKey" Core..=) Core.<$> consolidationKey,
                  ("Data" Core..=) Core.<$> data',
                  ("ExpiresAfter" Core..=) Core.<$> expiresAfter,
                  ("IconReference" Core..=) Core.<$> iconReference,
                  ("ImageIconUrl" Core..=) Core.<$> imageIconUrl,
                  ("ImageUrl" Core..=) Core.<$> imageUrl,
                  ("MD5" Core..=) Core.<$> mD5,
                  ("RawContent" Core..=) Core.<$> rawContent,
                  ("SilentPush" Core..=) Core.<$> silentPush,
                  ("SmallImageIconUrl" Core..=) Core.<$> smallImageIconUrl,
                  ("Sound" Core..=) Core.<$> sound,
                  ("Substitutions" Core..=) Core.<$> substitutions,
                  ("Title" Core..=) Core.<$> title, ("Url" Core..=) Core.<$> url])
