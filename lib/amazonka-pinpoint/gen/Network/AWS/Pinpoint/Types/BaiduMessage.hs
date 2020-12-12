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
    bmSubstitutions,
    bmSilentPush,
    bmImageIconURL,
    bmRawContent,
    bmData,
    bmSmallImageIconURL,
    bmBody,
    bmTimeToLive,
    bmURL,
    bmSound,
    bmAction,
    bmImageURL,
    bmTitle,
    bmIconReference,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the Baidu (Baidu Cloud Push) channel.
--
-- /See:/ 'mkBaiduMessage' smart constructor.
data BaiduMessage = BaiduMessage'
  { substitutions ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    silentPush :: Lude.Maybe Lude.Bool,
    imageIconURL :: Lude.Maybe Lude.Text,
    rawContent :: Lude.Maybe Lude.Text,
    data' :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    smallImageIconURL :: Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    timeToLive :: Lude.Maybe Lude.Int,
    url :: Lude.Maybe Lude.Text,
    sound :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
    imageURL :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text,
    iconReference :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BaiduMessage' with the minimum fields required to make a request.
--
-- * 'action' - The action to occur if the recipient taps the push notification. Valid values are:
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
-- * 'body' - The body of the notification message.
-- * 'data'' - The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
-- * 'iconReference' - The icon image name of the asset saved in your app.
-- * 'imageIconURL' - The URL of the large icon image to display in the content view of the push notification.
-- * 'imageURL' - The URL of an image to display in the push notification.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
-- * 'silentPush' - Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
-- * 'smallImageIconURL' - The URL of the small icon image to display in the status bar and the content view of the push notification.
-- * 'sound' - The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
-- * 'substitutions' - The default message variables to use in the notification message. You can override the default variables with individual address variables.
-- * 'timeToLive' - The amount of time, in seconds, that the Baidu Cloud Push service should store the message if the recipient's device is offline. The default value and maximum supported time is 604,800 seconds (7 days).
-- * 'title' - The title to display above the notification message on the recipient's device.
-- * 'url' - The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
mkBaiduMessage ::
  BaiduMessage
mkBaiduMessage =
  BaiduMessage'
    { substitutions = Lude.Nothing,
      silentPush = Lude.Nothing,
      imageIconURL = Lude.Nothing,
      rawContent = Lude.Nothing,
      data' = Lude.Nothing,
      smallImageIconURL = Lude.Nothing,
      body = Lude.Nothing,
      timeToLive = Lude.Nothing,
      url = Lude.Nothing,
      sound = Lude.Nothing,
      action = Lude.Nothing,
      imageURL = Lude.Nothing,
      title = Lude.Nothing,
      iconReference = Lude.Nothing
    }

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSubstitutions :: Lens.Lens' BaiduMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
bmSubstitutions = Lens.lens (substitutions :: BaiduMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: BaiduMessage)
{-# DEPRECATED bmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSilentPush :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Bool)
bmSilentPush = Lens.lens (silentPush :: BaiduMessage -> Lude.Maybe Lude.Bool) (\s a -> s {silentPush = a} :: BaiduMessage)
{-# DEPRECATED bmSilentPush "Use generic-lens or generic-optics with 'silentPush' instead." #-}

-- | The URL of the large icon image to display in the content view of the push notification.
--
-- /Note:/ Consider using 'imageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmImageIconURL :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmImageIconURL = Lens.lens (imageIconURL :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {imageIconURL = a} :: BaiduMessage)
{-# DEPRECATED bmImageIconURL "Use generic-lens or generic-optics with 'imageIconURL' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmRawContent :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmRawContent = Lens.lens (rawContent :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: BaiduMessage)
{-# DEPRECATED bmRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmData :: Lens.Lens' BaiduMessage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
bmData = Lens.lens (data' :: BaiduMessage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {data' = a} :: BaiduMessage)
{-# DEPRECATED bmData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- /Note:/ Consider using 'smallImageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSmallImageIconURL :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmSmallImageIconURL = Lens.lens (smallImageIconURL :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {smallImageIconURL = a} :: BaiduMessage)
{-# DEPRECATED bmSmallImageIconURL "Use generic-lens or generic-optics with 'smallImageIconURL' instead." #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmBody :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmBody = Lens.lens (body :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: BaiduMessage)
{-# DEPRECATED bmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The amount of time, in seconds, that the Baidu Cloud Push service should store the message if the recipient's device is offline. The default value and maximum supported time is 604,800 seconds (7 days).
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmTimeToLive :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Int)
bmTimeToLive = Lens.lens (timeToLive :: BaiduMessage -> Lude.Maybe Lude.Int) (\s a -> s {timeToLive = a} :: BaiduMessage)
{-# DEPRECATED bmTimeToLive "Use generic-lens or generic-optics with 'timeToLive' instead." #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmURL :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmURL = Lens.lens (url :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: BaiduMessage)
{-# DEPRECATED bmURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmSound :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmSound = Lens.lens (sound :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {sound = a} :: BaiduMessage)
{-# DEPRECATED bmSound "Use generic-lens or generic-optics with 'sound' instead." #-}

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
bmAction :: Lens.Lens' BaiduMessage (Lude.Maybe Action)
bmAction = Lens.lens (action :: BaiduMessage -> Lude.Maybe Action) (\s a -> s {action = a} :: BaiduMessage)
{-# DEPRECATED bmAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmImageURL :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmImageURL = Lens.lens (imageURL :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {imageURL = a} :: BaiduMessage)
{-# DEPRECATED bmImageURL "Use generic-lens or generic-optics with 'imageURL' instead." #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmTitle :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmTitle = Lens.lens (title :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: BaiduMessage)
{-# DEPRECATED bmTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The icon image name of the asset saved in your app.
--
-- /Note:/ Consider using 'iconReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmIconReference :: Lens.Lens' BaiduMessage (Lude.Maybe Lude.Text)
bmIconReference = Lens.lens (iconReference :: BaiduMessage -> Lude.Maybe Lude.Text) (\s a -> s {iconReference = a} :: BaiduMessage)
{-# DEPRECATED bmIconReference "Use generic-lens or generic-optics with 'iconReference' instead." #-}

instance Lude.ToJSON BaiduMessage where
  toJSON BaiduMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("SilentPush" Lude..=) Lude.<$> silentPush,
            ("ImageIconUrl" Lude..=) Lude.<$> imageIconURL,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("Data" Lude..=) Lude.<$> data',
            ("SmallImageIconUrl" Lude..=) Lude.<$> smallImageIconURL,
            ("Body" Lude..=) Lude.<$> body,
            ("TimeToLive" Lude..=) Lude.<$> timeToLive,
            ("Url" Lude..=) Lude.<$> url,
            ("Sound" Lude..=) Lude.<$> sound,
            ("Action" Lude..=) Lude.<$> action,
            ("ImageUrl" Lude..=) Lude.<$> imageURL,
            ("Title" Lude..=) Lude.<$> title,
            ("IconReference" Lude..=) Lude.<$> iconReference
          ]
      )
