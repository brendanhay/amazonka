{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMMessage
  ( ADMMessage (..),

    -- * Smart constructor
    mkADMMessage,

    -- * Lenses
    admmSubstitutions,
    admmExpiresAfter,
    admmMD5,
    admmSilentPush,
    admmImageIconURL,
    admmRawContent,
    admmData,
    admmSmallImageIconURL,
    admmBody,
    admmURL,
    admmSound,
    admmAction,
    admmImageURL,
    admmConsolidationKey,
    admmTitle,
    admmIconReference,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a one-time message that's sent directly to an endpoint through the ADM (Amazon Device Messaging) channel.
--
-- /See:/ 'mkADMMessage' smart constructor.
data ADMMessage = ADMMessage'
  { substitutions ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    expiresAfter :: Lude.Maybe Lude.Text,
    md5 :: Lude.Maybe Lude.Text,
    silentPush :: Lude.Maybe Lude.Bool,
    imageIconURL :: Lude.Maybe Lude.Text,
    rawContent :: Lude.Maybe Lude.Text,
    data' :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    smallImageIconURL :: Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    sound :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe Action,
    imageURL :: Lude.Maybe Lude.Text,
    consolidationKey :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ADMMessage' with the minimum fields required to make a request.
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
-- * 'consolidationKey' - An arbitrary string that indicates that multiple messages are logically the same and that Amazon Device Messaging (ADM) can drop previously enqueued messages in favor of this message.
-- * 'data'' - The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
-- * 'expiresAfter' - The amount of time, in seconds, that ADM should store the message if the recipient's device is offline. Amazon Pinpoint specifies this value in the expiresAfter parameter when it sends the notification message to ADM.
-- * 'iconReference' - The icon image name of the asset saved in your app.
-- * 'imageIconURL' - The URL of the large icon image to display in the content view of the push notification.
-- * 'imageURL' - The URL of an image to display in the push notification.
-- * 'md5' - The base64-encoded, MD5 checksum of the value specified by the Data property. ADM uses the MD5 value to verify the integrity of the data.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
-- * 'silentPush' - Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
-- * 'smallImageIconURL' - The URL of the small icon image to display in the status bar and the content view of the push notification.
-- * 'sound' - The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
-- * 'substitutions' - The default message variables to use in the notification message. You can override the default variables with individual address variables.
-- * 'title' - The title to display above the notification message on the recipient's device.
-- * 'url' - The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
mkADMMessage ::
  ADMMessage
mkADMMessage =
  ADMMessage'
    { substitutions = Lude.Nothing,
      expiresAfter = Lude.Nothing,
      md5 = Lude.Nothing,
      silentPush = Lude.Nothing,
      imageIconURL = Lude.Nothing,
      rawContent = Lude.Nothing,
      data' = Lude.Nothing,
      smallImageIconURL = Lude.Nothing,
      body = Lude.Nothing,
      url = Lude.Nothing,
      sound = Lude.Nothing,
      action = Lude.Nothing,
      imageURL = Lude.Nothing,
      consolidationKey = Lude.Nothing,
      title = Lude.Nothing,
      iconReference = Lude.Nothing
    }

-- | The default message variables to use in the notification message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSubstitutions :: Lens.Lens' ADMMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
admmSubstitutions = Lens.lens (substitutions :: ADMMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: ADMMessage)
{-# DEPRECATED admmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The amount of time, in seconds, that ADM should store the message if the recipient's device is offline. Amazon Pinpoint specifies this value in the expiresAfter parameter when it sends the notification message to ADM.
--
-- /Note:/ Consider using 'expiresAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmExpiresAfter :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmExpiresAfter = Lens.lens (expiresAfter :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {expiresAfter = a} :: ADMMessage)
{-# DEPRECATED admmExpiresAfter "Use generic-lens or generic-optics with 'expiresAfter' instead." #-}

-- | The base64-encoded, MD5 checksum of the value specified by the Data property. ADM uses the MD5 value to verify the integrity of the data.
--
-- /Note:/ Consider using 'md5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmMD5 :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmMD5 = Lens.lens (md5 :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {md5 = a} :: ADMMessage)
{-# DEPRECATED admmMD5 "Use generic-lens or generic-optics with 'md5' instead." #-}

-- | Specifies whether the notification is a silent push notification, which is a push notification that doesn't display on a recipient's device. Silent push notifications can be used for cases such as updating an app's configuration or supporting phone home functionality.
--
-- /Note:/ Consider using 'silentPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSilentPush :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Bool)
admmSilentPush = Lens.lens (silentPush :: ADMMessage -> Lude.Maybe Lude.Bool) (\s a -> s {silentPush = a} :: ADMMessage)
{-# DEPRECATED admmSilentPush "Use generic-lens or generic-optics with 'silentPush' instead." #-}

-- | The URL of the large icon image to display in the content view of the push notification.
--
-- /Note:/ Consider using 'imageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmImageIconURL :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmImageIconURL = Lens.lens (imageIconURL :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {imageIconURL = a} :: ADMMessage)
{-# DEPRECATED admmImageIconURL "Use generic-lens or generic-optics with 'imageIconURL' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the notification message. If specified, this value overrides all other content for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmRawContent :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmRawContent = Lens.lens (rawContent :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: ADMMessage)
{-# DEPRECATED admmRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The JSON data payload to use for the push notification, if the notification is a silent push notification. This payload is added to the data.pinpoint.jsonBody object of the notification.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmData :: Lens.Lens' ADMMessage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
admmData = Lens.lens (data' :: ADMMessage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {data' = a} :: ADMMessage)
{-# DEPRECATED admmData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The URL of the small icon image to display in the status bar and the content view of the push notification.
--
-- /Note:/ Consider using 'smallImageIconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSmallImageIconURL :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmSmallImageIconURL = Lens.lens (smallImageIconURL :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {smallImageIconURL = a} :: ADMMessage)
{-# DEPRECATED admmSmallImageIconURL "Use generic-lens or generic-optics with 'smallImageIconURL' instead." #-}

-- | The body of the notification message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmBody :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmBody = Lens.lens (body :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: ADMMessage)
{-# DEPRECATED admmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The URL to open in the recipient's default mobile browser, if a recipient taps the push notification and the value of the Action property is URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmURL :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmURL = Lens.lens (url :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: ADMMessage)
{-# DEPRECATED admmURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The sound to play when the recipient receives the push notification. You can use the default stream or specify the file name of a sound resource that's bundled in your app. On an Android platform, the sound file must reside in /res/raw/.
--
-- /Note:/ Consider using 'sound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmSound :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmSound = Lens.lens (sound :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {sound = a} :: ADMMessage)
{-# DEPRECATED admmSound "Use generic-lens or generic-optics with 'sound' instead." #-}

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
admmAction :: Lens.Lens' ADMMessage (Lude.Maybe Action)
admmAction = Lens.lens (action :: ADMMessage -> Lude.Maybe Action) (\s a -> s {action = a} :: ADMMessage)
{-# DEPRECATED admmAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The URL of an image to display in the push notification.
--
-- /Note:/ Consider using 'imageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmImageURL :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmImageURL = Lens.lens (imageURL :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {imageURL = a} :: ADMMessage)
{-# DEPRECATED admmImageURL "Use generic-lens or generic-optics with 'imageURL' instead." #-}

-- | An arbitrary string that indicates that multiple messages are logically the same and that Amazon Device Messaging (ADM) can drop previously enqueued messages in favor of this message.
--
-- /Note:/ Consider using 'consolidationKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmConsolidationKey :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmConsolidationKey = Lens.lens (consolidationKey :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {consolidationKey = a} :: ADMMessage)
{-# DEPRECATED admmConsolidationKey "Use generic-lens or generic-optics with 'consolidationKey' instead." #-}

-- | The title to display above the notification message on the recipient's device.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmTitle :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmTitle = Lens.lens (title :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: ADMMessage)
{-# DEPRECATED admmTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The icon image name of the asset saved in your app.
--
-- /Note:/ Consider using 'iconReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admmIconReference :: Lens.Lens' ADMMessage (Lude.Maybe Lude.Text)
admmIconReference = Lens.lens (iconReference :: ADMMessage -> Lude.Maybe Lude.Text) (\s a -> s {iconReference = a} :: ADMMessage)
{-# DEPRECATED admmIconReference "Use generic-lens or generic-optics with 'iconReference' instead." #-}

instance Lude.ToJSON ADMMessage where
  toJSON ADMMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("ExpiresAfter" Lude..=) Lude.<$> expiresAfter,
            ("MD5" Lude..=) Lude.<$> md5,
            ("SilentPush" Lude..=) Lude.<$> silentPush,
            ("ImageIconUrl" Lude..=) Lude.<$> imageIconURL,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("Data" Lude..=) Lude.<$> data',
            ("SmallImageIconUrl" Lude..=) Lude.<$> smallImageIconURL,
            ("Body" Lude..=) Lude.<$> body,
            ("Url" Lude..=) Lude.<$> url,
            ("Sound" Lude..=) Lude.<$> sound,
            ("Action" Lude..=) Lude.<$> action,
            ("ImageUrl" Lude..=) Lude.<$> imageURL,
            ("ConsolidationKey" Lude..=) Lude.<$> consolidationKey,
            ("Title" Lude..=) Lude.<$> title,
            ("IconReference" Lude..=) Lude.<$> iconReference
          ]
      )
