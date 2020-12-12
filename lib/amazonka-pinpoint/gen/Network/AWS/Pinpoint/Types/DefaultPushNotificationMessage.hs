{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
  ( DefaultPushNotificationMessage (..),

    -- * Smart constructor
    mkDefaultPushNotificationMessage,

    -- * Lenses
    dpnmSubstitutions,
    dpnmSilentPush,
    dpnmData,
    dpnmBody,
    dpnmURL,
    dpnmAction,
    dpnmTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies the default settings and content for a push notification that's sent directly to an endpoint.
--
-- /See:/ 'mkDefaultPushNotificationMessage' smart constructor.
data DefaultPushNotificationMessage = DefaultPushNotificationMessage'
  { substitutions ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ([Lude.Text])
        ),
    silentPush ::
      Lude.Maybe Lude.Bool,
    data' ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    body :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
    title :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefaultPushNotificationMessage' with the minimum fields required to make a request.
--
-- * 'action' - The default action to occur if a recipient taps the push notification. Valid values are:
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
-- * 'body' - The default body of the notification message.
-- * 'data'' - The JSON data payload to use for the default push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
-- * 'silentPush' - Specifies whether the default notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or delivering messages to an in-app notification center.
-- * 'substitutions' - The default message variables to use in the notification message. You can override the default variables with individual address variables.
-- * 'title' - The default title to display above the notification message on a recipient's device.
-- * 'url' - The default URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
mkDefaultPushNotificationMessage ::
  DefaultPushNotificationMessage
mkDefaultPushNotificationMessage =
  DefaultPushNotificationMessage'
    { substitutions = Lude.Nothing,
      silentPush = Lude.Nothing,
      data' = Lude.Nothing,
      body = Lude.Nothing,
      url = Lude.Nothing,
      action = Lude.Nothing,
      title = Lude.Nothing
    }

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmSubstitutions :: Lens.Lens' DefaultPushNotificationMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
dpnmSubstitutions = Lens.lens (substitutions :: DefaultPushNotificationMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: DefaultPushNotificationMessage)
{-# DEPRECATED dpnmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | Specifies whether the default notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or delivering messages to an in-app notification center.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmSilentPush :: Lens.Lens' DefaultPushNotificationMessage (Lude.Maybe Lude.Bool)
dpnmSilentPush = Lens.lens (silentPush :: DefaultPushNotificationMessage -> Lude.Maybe Lude.Bool) (\s a -> s {silentPush = a} :: DefaultPushNotificationMessage)
{-# DEPRECATED dpnmSilentPush "Use generic-lens or generic-optics with 'silentPush' instead." #-}

-- | The JSON data payload to use for the default push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmData :: Lens.Lens' DefaultPushNotificationMessage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dpnmData = Lens.lens (data' :: DefaultPushNotificationMessage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {data' = a} :: DefaultPushNotificationMessage)
{-# DEPRECATED dpnmData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The default body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmBody :: Lens.Lens' DefaultPushNotificationMessage (Lude.Maybe Lude.Text)
dpnmBody = Lens.lens (body :: DefaultPushNotificationMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: DefaultPushNotificationMessage)
{-# DEPRECATED dpnmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The default URL to open in a recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmURL :: Lens.Lens' DefaultPushNotificationMessage (Lude.Maybe Lude.Text)
dpnmURL = Lens.lens (url :: DefaultPushNotificationMessage -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DefaultPushNotificationMessage)
{-# DEPRECATED dpnmURL "Use generic-lens or generic-optics with 'url' instead." #-}

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
dpnmAction :: Lens.Lens' DefaultPushNotificationMessage (Lude.Maybe Action)
dpnmAction = Lens.lens (action :: DefaultPushNotificationMessage -> Lude.Maybe Action) (\s a -> s {action = a} :: DefaultPushNotificationMessage)
{-# DEPRECATED dpnmAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The default title to display above the notification message on a recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpnmTitle :: Lens.Lens' DefaultPushNotificationMessage (Lude.Maybe Lude.Text)
dpnmTitle = Lens.lens (title :: DefaultPushNotificationMessage -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: DefaultPushNotificationMessage)
{-# DEPRECATED dpnmTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.ToJSON DefaultPushNotificationMessage where
  toJSON DefaultPushNotificationMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("SilentPush" Lude..=) Lude.<$> silentPush,
            ("Data" Lude..=) Lude.<$> data',
            ("Body" Lude..=) Lude.<$> body,
            ("Url" Lude..=) Lude.<$> url,
            ("Action" Lude..=) Lude.<$> action,
            ("Title" Lude..=) Lude.<$> title
          ]
      )
