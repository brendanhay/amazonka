{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSPushNotificationTemplate
  ( APNSPushNotificationTemplate (..),

    -- * Smart constructor
    mkAPNSPushNotificationTemplate,

    -- * Lenses
    apntRawContent,
    apntBody,
    apntURL,
    apntSound,
    apntAction,
    apntMediaURL,
    apntTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies channel-specific content and settings for a message template that can be used in push notifications that are sent through the APNs (Apple Push Notification service) channel.
--
-- /See:/ 'mkAPNSPushNotificationTemplate' smart constructor.
data APNSPushNotificationTemplate = APNSPushNotificationTemplate'
  { -- | The raw, JSON-formatted string to use as the payload for push notifications that are based on the message template. If specified, this value overrides all other content for the message template.
    rawContent :: Lude.Maybe Lude.Text,
    -- | The message body to use in push notifications that are based on the message template.
    body :: Lude.Maybe Lude.Text,
    -- | The URL to open in the recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
    url :: Lude.Maybe Lude.Text,
    -- | The key for the sound to play when the recipient receives a push notification that's based on the message template. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
    sound :: Lude.Maybe Lude.Text,
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
    action :: Lude.Maybe Action,
    -- | The URL of an image or video to display in push notifications that are based on the message template.
    mediaURL :: Lude.Maybe Lude.Text,
    -- | The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
    title :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APNSPushNotificationTemplate' with the minimum fields required to make a request.
--
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for push notifications that are based on the message template. If specified, this value overrides all other content for the message template.
-- * 'body' - The message body to use in push notifications that are based on the message template.
-- * 'url' - The URL to open in the recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
-- * 'sound' - The key for the sound to play when the recipient receives a push notification that's based on the message template. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
-- * 'action' - The action to occur if a recipient taps a push notification that's based on the message template. Valid values are:
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
-- * 'mediaURL' - The URL of an image or video to display in push notifications that are based on the message template.
-- * 'title' - The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
mkAPNSPushNotificationTemplate ::
  APNSPushNotificationTemplate
mkAPNSPushNotificationTemplate =
  APNSPushNotificationTemplate'
    { rawContent = Lude.Nothing,
      body = Lude.Nothing,
      url = Lude.Nothing,
      sound = Lude.Nothing,
      action = Lude.Nothing,
      mediaURL = Lude.Nothing,
      title = Lude.Nothing
    }

-- | The raw, JSON-formatted string to use as the payload for push notifications that are based on the message template. If specified, this value overrides all other content for the message template.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntRawContent :: Lens.Lens' APNSPushNotificationTemplate (Lude.Maybe Lude.Text)
apntRawContent = Lens.lens (rawContent :: APNSPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: APNSPushNotificationTemplate)
{-# DEPRECATED apntRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The message body to use in push notifications that are based on the message template.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntBody :: Lens.Lens' APNSPushNotificationTemplate (Lude.Maybe Lude.Text)
apntBody = Lens.lens (body :: APNSPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: APNSPushNotificationTemplate)
{-# DEPRECATED apntBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps a push notification that's based on the message template and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntURL :: Lens.Lens' APNSPushNotificationTemplate (Lude.Maybe Lude.Text)
apntURL = Lens.lens (url :: APNSPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: APNSPushNotificationTemplate)
{-# DEPRECATED apntURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The key for the sound to play when the recipient receives a push notification that's based on the message template. The value for this key is the name of a sound file in your app's main bundle or the Library/Sounds folder in your app's data container. If the sound file can't be found or you specify default for the value, the system plays the default alert sound.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntSound :: Lens.Lens' APNSPushNotificationTemplate (Lude.Maybe Lude.Text)
apntSound = Lens.lens (sound :: APNSPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {sound = a} :: APNSPushNotificationTemplate)
{-# DEPRECATED apntSound "Use generic-lens or generic-optics with 'sound' instead." #-}

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
apntAction :: Lens.Lens' APNSPushNotificationTemplate (Lude.Maybe Action)
apntAction = Lens.lens (action :: APNSPushNotificationTemplate -> Lude.Maybe Action) (\s a -> s {action = a} :: APNSPushNotificationTemplate)
{-# DEPRECATED apntAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The URL of an image or video to display in push notifications that are based on the message template.
--
-- /Note:/ Consider using 'mediaURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntMediaURL :: Lens.Lens' APNSPushNotificationTemplate (Lude.Maybe Lude.Text)
apntMediaURL = Lens.lens (mediaURL :: APNSPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {mediaURL = a} :: APNSPushNotificationTemplate)
{-# DEPRECATED apntMediaURL "Use generic-lens or generic-optics with 'mediaURL' instead." #-}

-- | The title to use in push notifications that are based on the message template. This title appears above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apntTitle :: Lens.Lens' APNSPushNotificationTemplate (Lude.Maybe Lude.Text)
apntTitle = Lens.lens (title :: APNSPushNotificationTemplate -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: APNSPushNotificationTemplate)
{-# DEPRECATED apntTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON APNSPushNotificationTemplate where
  parseJSON =
    Lude.withObject
      "APNSPushNotificationTemplate"
      ( \x ->
          APNSPushNotificationTemplate'
            Lude.<$> (x Lude..:? "RawContent")
            Lude.<*> (x Lude..:? "Body")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "Sound")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "MediaUrl")
            Lude.<*> (x Lude..:? "Title")
      )

instance Lude.ToJSON APNSPushNotificationTemplate where
  toJSON APNSPushNotificationTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RawContent" Lude..=) Lude.<$> rawContent,
            ("Body" Lude..=) Lude.<$> body,
            ("Url" Lude..=) Lude.<$> url,
            ("Sound" Lude..=) Lude.<$> sound,
            ("Action" Lude..=) Lude.<$> action,
            ("MediaUrl" Lude..=) Lude.<$> mediaURL,
            ("Title" Lude..=) Lude.<$> title
          ]
      )
