-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefaultPushNotificationTemplate
  ( DefaultPushNotificationTemplate (..),

    -- * Smart constructor
    mkDefaultPushNotificationTemplate,

    -- * Lenses
    dpntBody,
    dpntURL,
    dpntSound,
    dpntAction,
    dpntTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies the default settings and content for a message template that can be used in messages that are sent through a push notification channel.
--
-- /See:/ 'mkDefaultPushNotificationTemplate' smart constructor.
data DefaultPushNotificationTemplate = DefaultPushNotificationTemplate'
  { body ::
      Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    sound ::
      Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
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

-- | Creates a value of 'DefaultPushNotificationTemplate' with the minimum fields required to make a request.
--
-- * 'action' - The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
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
-- * 'body' - The message body to use in push notifications that are based on the message template.
-- * 'sound' - The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- For an iOS platform, this value is the key for the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
-- * 'title' - The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
-- * 'url' - The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
mkDefaultPushNotificationTemplate ::
  DefaultPushNotificationTemplate
mkDefaultPushNotificationTemplate =
  DefaultPushNotificationTemplate'
    { body = Lude.Nothing,
      url = Lude.Nothing,
      sound = Lude.Nothing,
      action = Lude.Nothing,
      title = Lude.Nothing
    }

-- | The message body to use in push notifications that are based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntBody :: Lens.Lens' DefaultPushNotificationTemplate (Lude.Maybe Lude.Text)
dpntBody = Lens.lens (body :: DefaultPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: DefaultPushNotificationTemplate)
{-# DEPRECATED dpntBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The URL to open in a recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntURL :: Lens.Lens' DefaultPushNotificationTemplate (Lude.Maybe Lude.Text)
dpntURL = Lens.lens (url :: DefaultPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DefaultPushNotificationTemplate)
{-# DEPRECATED dpntURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The sound to play when a recipient receives a push notification that's based on the message template. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- For an iOS platform, this value is the key for the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntSound :: Lens.Lens' DefaultPushNotificationTemplate (Lude.Maybe Lude.Text)
dpntSound = Lens.lens (sound :: DefaultPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {sound = a} :: DefaultPushNotificationTemplate)
{-# DEPRECATED dpntSound "Use generic-lens or generic-optics with 'sound' instead." #-}

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
dpntAction :: Lens.Lens' DefaultPushNotificationTemplate (Lude.Maybe Action)
dpntAction = Lens.lens (action :: DefaultPushNotificationTemplate -> Lude.Maybe Action) (\s a -> s {action = a} :: DefaultPushNotificationTemplate)
{-# DEPRECATED dpntAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpntTitle :: Lens.Lens' DefaultPushNotificationTemplate (Lude.Maybe Lude.Text)
dpntTitle = Lens.lens (title :: DefaultPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: DefaultPushNotificationTemplate)
{-# DEPRECATED dpntTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON DefaultPushNotificationTemplate where
  parseJSON =
    Lude.withObject
      "DefaultPushNotificationTemplate"
      ( \x ->
          DefaultPushNotificationTemplate'
            Lude.<$> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "Sound")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "Title")
      )

instance Lude.ToJSON DefaultPushNotificationTemplate where
  toJSON DefaultPushNotificationTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Body" Lude..=) Lude.<$> body,
            ("Url" Lude..=) Lude.<$> url,
            ("Sound" Lude..=) Lude.<$> sound,
            ("Action" Lude..=) Lude.<$> action,
            ("Title" Lude..=) Lude.<$> title
          ]
      )
