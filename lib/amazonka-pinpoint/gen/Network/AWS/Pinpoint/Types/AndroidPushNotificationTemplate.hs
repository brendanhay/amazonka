-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
  ( AndroidPushNotificationTemplate (..),

    -- * Smart constructor
    mkAndroidPushNotificationTemplate,

    -- * Lenses
    aImageIconURL,
    aRawContent,
    aSmallImageIconURL,
    aBody,
    aURL,
    aSound,
    aAction,
    aImageURL,
    aTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies channel-specific content and settings for a message template that can be used in push notifications that are sent through the ADM (Amazon Device Messaging), Baidu (Baidu Cloud Push), or GCM (Firebase Cloud Messaging, formerly Google Cloud Messaging) channel.
--
-- /See:/ 'mkAndroidPushNotificationTemplate' smart constructor.
data AndroidPushNotificationTemplate = AndroidPushNotificationTemplate'
  { imageIconURL ::
      Lude.Maybe Lude.Text,
    rawContent ::
      Lude.Maybe Lude.Text,
    smallImageIconURL ::
      Lude.Maybe Lude.Text,
    body ::
      Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    sound ::
      Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
    imageURL ::
      Lude.Maybe Lude.Text,
    title ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AndroidPushNotificationTemplate' with the minimum fields required to make a request.
--
-- * 'action' - The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
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
-- * 'body' - The message body to use in a push notification that's based on the message template.
-- * 'imageIconURL' - The URL of the large icon image to display in the content view of a push notification that's based on the message template.
-- * 'imageURL' - The URL of an image to display in a push notification that's based on the message template.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for a push notification that's based on the message template. If specified, this value overrides all other content for the message template.
-- * 'smallImageIconURL' - The URL of the small icon image to display in the status bar and the content view of a push notification that's based on the message template.
-- * 'sound' - The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
-- * 'title' - The title to use in a push notification that's based on the message template. This title appears above the notification message on a recipient's device.
-- * 'url' - The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
mkAndroidPushNotificationTemplate ::
  AndroidPushNotificationTemplate
mkAndroidPushNotificationTemplate =
  AndroidPushNotificationTemplate'
    { imageIconURL = Lude.Nothing,
      rawContent = Lude.Nothing,
      smallImageIconURL = Lude.Nothing,
      body = Lude.Nothing,
      url = Lude.Nothing,
      sound = Lude.Nothing,
      action = Lude.Nothing,
      imageURL = Lude.Nothing,
      title = Lude.Nothing
    }

-- | The URL of the large icon image to display in the content view of a push notification that's based on the message template.
--
-- /Note:/ Consider using 'imageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aImageIconURL :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aImageIconURL = Lens.lens (imageIconURL :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {imageIconURL = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aImageIconURL "Use generic-lens or generic-optics with 'imageIconURL' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for a push notification that's based on the message template. If specified, this value overrides all other content for the message template.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRawContent :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aRawContent = Lens.lens (rawContent :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The URL of the small icon image to display in the status bar and the content view of a push notification that's based on the message template.
--
-- /Note:/ Consider using 'smallImageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSmallImageIconURL :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aSmallImageIconURL = Lens.lens (smallImageIconURL :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {smallImageIconURL = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aSmallImageIconURL "Use generic-lens or generic-optics with 'smallImageIconURL' instead." #-}

-- | The message body to use in a push notification that's based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBody :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aBody = Lens.lens (body :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aURL :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aURL = Lens.lens (url :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSound :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aSound = Lens.lens (sound :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {sound = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aSound "Use generic-lens or generic-optics with 'sound' instead." #-}

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
aAction :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Action)
aAction = Lens.lens (action :: AndroidPushNotificationTemplate -> Lude.Maybe Action) (\s a -> s {action = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The URL of an image to display in a push notification that's based on the message template.
--
-- /Note:/ Consider using 'imageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aImageURL :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aImageURL = Lens.lens (imageURL :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {imageURL = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aImageURL "Use generic-lens or generic-optics with 'imageURL' instead." #-}

-- | The title to use in a push notification that's based on the message template. This title appears above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTitle :: Lens.Lens' AndroidPushNotificationTemplate (Lude.Maybe Lude.Text)
aTitle = Lens.lens (title :: AndroidPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: AndroidPushNotificationTemplate)
{-# DEPRECATED aTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON AndroidPushNotificationTemplate where
  parseJSON =
    Lude.withObject
      "AndroidPushNotificationTemplate"
      ( \x ->
          AndroidPushNotificationTemplate'
            Lude.<$> (x Lude..:? "ImageIconUrl")
            Lude.<*> (x Lude..:? "RawContent")
            Lude.<*> (x Lude..:? "SmallImageIconUrl")
            Lude.<*> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "Sound")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "ImageUrl")
            Lude.<*> (x Lude..:? "Title")
      )

instance Lude.ToJSON AndroidPushNotificationTemplate where
  toJSON AndroidPushNotificationTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ImageIconUrl" Lude..=) Lude.<$> imageIconURL,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("SmallImageIconUrl" Lude..=) Lude.<$> smallImageIconURL,
            ("Body" Lude..=) Lude.<$> body,
            ("Url" Lude..=) Lude.<$> url,
            ("Sound" Lude..=) Lude.<$> sound,
            ("Action" Lude..=) Lude.<$> action,
            ("ImageUrl" Lude..=) Lude.<$> imageURL,
            ("Title" Lude..=) Lude.<$> title
          ]
      )
