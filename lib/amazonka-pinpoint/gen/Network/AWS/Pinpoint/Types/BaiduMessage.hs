{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaiduMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduMessage
  ( BaiduMessage (..),

    -- * Smart constructor
    mkBaiduMessage,

    -- * Lenses
    bmAction,
    bmBody,
    bmData,
    bmIconReference,
    bmImageIconUrl,
    bmImageUrl,
    bmRawContent,
    bmSilentPush,
    bmSmallImageIconUrl,
    bmSound,
    bmSubstitutions,
    bmTimeToLive,
    bmTitle,
    bmUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Action as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the Baidu (Baidu Cloud Push) channel.
--
-- /See:/ 'mkBaiduMessage' smart constructor.
data BaiduMessage = BaiduMessage'
  { -- | The action to occur if the recipient taps the push notification. Valid values are:
    --
    --
    --     * OPEN_APP - Your app opens or it becomes the foreground app if it was sent to the background. This is the default action.
    --
    --
    --     * DEEP_LINK - Your app opens and displays a designated user interface in the app. This action uses the deep-linking features of the Android platform.
    --
    --
    --     * URL - The default mobile browser on the recipient's device opens and loads the web page at a URL that you specify.
    action :: Core.Maybe Types.Action,
    -- | The body of the notification message.
    body :: Core.Maybe Core.Text,
    -- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
    data' :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The icon image name of the asset saved in your app.
    iconReference :: Core.Maybe Core.Text,
    -- | The URL of the large icon image to display in the content view of the push notification.
    imageIconUrl :: Core.Maybe Core.Text,
    -- | The URL of an image to display in the push notification.
    imageUrl :: Core.Maybe Core.Text,
    -- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
    rawContent :: Core.Maybe Core.Text,
    -- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
    silentPush :: Core.Maybe Core.Bool,
    -- | The URL of the small icon image to display in the status bar and the content view of the push notification.
    smallImageIconUrl :: Core.Maybe Core.Text,
    -- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
    sound :: Core.Maybe Core.Text,
    -- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The amount of time, in seconds, that the Baidu Cloud Push service should store the message if the recipient's device is offline. The default value and maximum supported time is 604,800 seconds (7 days).
    timeToLive :: Core.Maybe Core.Int,
    -- | The title to display above the notification message on the recipient's device.
    title :: Core.Maybe Core.Text,
    -- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
    url :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BaiduMessage' value with any optional fields omitted.
mkBaiduMessage ::
  BaiduMessage
mkBaiduMessage =
  BaiduMessage'
    { action = Core.Nothing,
      body = Core.Nothing,
      data' = Core.Nothing,
      iconReference = Core.Nothing,
      imageIconUrl = Core.Nothing,
      imageUrl = Core.Nothing,
      rawContent = Core.Nothing,
      silentPush = Core.Nothing,
      smallImageIconUrl = Core.Nothing,
      sound = Core.Nothing,
      substitutions = Core.Nothing,
      timeToLive = Core.Nothing,
      title = Core.Nothing,
      url = Core.Nothing
    }

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
bmAction :: Lens.Lens' BaiduMessage (Core.Maybe Types.Action)
bmAction = Lens.field @"action"
{-# DEPRECATED bmAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmBody :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmBody = Lens.field @"body"
{-# DEPRECATED bmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmData :: Lens.Lens' BaiduMessage (Core.Maybe (Core.HashMap Core.Text Core.Text))
bmData = Lens.field @"data'"
{-# DEPRECATED bmData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The icon image name of the asset saved in your app.
--
-- /Note:/ Consider using 'iconReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmIconReference :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmIconReference = Lens.field @"iconReference"
{-# DEPRECATED bmIconReference "Use generic-lens or generic-optics with 'iconReference' instead." #-}

-- | The URL of the large icon image to display in the content view of the push notification.
--
-- /Note:/ Consider using 'imageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmImageIconUrl :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmImageIconUrl = Lens.field @"imageIconUrl"
{-# DEPRECATED bmImageIconUrl "Use generic-lens or generic-optics with 'imageIconUrl' instead." #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmImageUrl :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmImageUrl = Lens.field @"imageUrl"
{-# DEPRECATED bmImageUrl "Use generic-lens or generic-optics with 'imageUrl' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmRawContent :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmRawContent = Lens.field @"rawContent"
{-# DEPRECATED bmRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSilentPush :: Lens.Lens' BaiduMessage (Core.Maybe Core.Bool)
bmSilentPush = Lens.field @"silentPush"
{-# DEPRECATED bmSilentPush "Use generic-lens or generic-optics with 'silentPush' instead." #-}

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- /Note:/ Consider using 'smallImageIconUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSmallImageIconUrl :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmSmallImageIconUrl = Lens.field @"smallImageIconUrl"
{-# DEPRECATED bmSmallImageIconUrl "Use generic-lens or generic-optics with 'smallImageIconUrl' instead." #-}

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSound :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmSound = Lens.field @"sound"
{-# DEPRECATED bmSound "Use generic-lens or generic-optics with 'sound' instead." #-}

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSubstitutions :: Lens.Lens' BaiduMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
bmSubstitutions = Lens.field @"substitutions"
{-# DEPRECATED bmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The amount of time, in seconds, that the Baidu Cloud Push service should store the message if the recipient's device is offline. The default value and maximum supported time is 604,800 seconds (7 days).
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmTimeToLive :: Lens.Lens' BaiduMessage (Core.Maybe Core.Int)
bmTimeToLive = Lens.field @"timeToLive"
{-# DEPRECATED bmTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmTitle :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmTitle = Lens.field @"title"
{-# DEPRECATED bmTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmUrl :: Lens.Lens' BaiduMessage (Core.Maybe Core.Text)
bmUrl = Lens.field @"url"
{-# DEPRECATED bmUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON BaiduMessage where
  toJSON BaiduMessage {..} =
    Core.object
      ( Core.catMaybes
          [ ("Action" Core..=) Core.<$> action,
            ("Body" Core..=) Core.<$> body,
            ("Data" Core..=) Core.<$> data',
            ("IconReference" Core..=) Core.<$> iconReference,
            ("ImageIconUrl" Core..=) Core.<$> imageIconUrl,
            ("ImageUrl" Core..=) Core.<$> imageUrl,
            ("RawContent" Core..=) Core.<$> rawContent,
            ("SilentPush" Core..=) Core.<$> silentPush,
            ("SmallImageIconUrl" Core..=) Core.<$> smallImageIconUrl,
            ("Sound" Core..=) Core.<$> sound,
            ("Substitutions" Core..=) Core.<$> substitutions,
            ("TimeToLive" Core..=) Core.<$> timeToLive,
            ("Title" Core..=) Core.<$> title,
            ("Url" Core..=) Core.<$> url
          ]
      )
