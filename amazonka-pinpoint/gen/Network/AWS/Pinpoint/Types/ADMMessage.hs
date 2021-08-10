{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for a one-time message that\'s sent directly to
-- an endpoint through the ADM (Amazon Device Messaging) channel.
--
-- /See:/ 'newADMMessage' smart constructor.
data ADMMessage = ADMMessage'
  { -- | Specifies whether the notification is a silent push notification, which
    -- is a push notification that doesn\'t display on a recipient\'s device.
    -- Silent push notifications can be used for cases such as updating an
    -- app\'s configuration or supporting phone home functionality.
    silentPush :: Prelude.Maybe Prelude.Bool,
    -- | The URL of the large icon image to display in the content view of the
    -- push notification.
    imageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that ADM should store the message if the
    -- recipient\'s device is offline. Amazon Pinpoint specifies this value in
    -- the expiresAfter parameter when it sends the notification message to
    -- ADM.
    expiresAfter :: Prelude.Maybe Prelude.Text,
    -- | The JSON data payload to use for the push notification, if the
    -- notification is a silent push notification. This payload is added to the
    -- data.pinpoint.jsonBody object of the notification.
    data' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The title to display above the notification message on the recipient\'s
    -- device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The icon image name of the asset saved in your app.
    iconReference :: Prelude.Maybe Prelude.Text,
    -- | The body of the notification message.
    body :: Prelude.Maybe Prelude.Text,
    -- | An arbitrary string that indicates that multiple messages are logically
    -- the same and that Amazon Device Messaging (ADM) can drop previously
    -- enqueued messages in favor of this message.
    consolidationKey :: Prelude.Maybe Prelude.Text,
    -- | The default message variables to use in the notification message. You
    -- can override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The URL of an image to display in the push notification.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded, MD5 checksum of the value specified by the Data
    -- property. ADM uses the MD5 value to verify the integrity of the data.
    md5 :: Prelude.Maybe Prelude.Text,
    -- | The action to occur if the recipient taps the push notification. Valid
    -- values are:
    --
    -- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
    --     sent to the background. This is the default action.
    --
    -- -   DEEP_LINK - Your app opens and displays a designated user interface
    --     in the app. This action uses the deep-linking features of the
    --     Android platform.
    --
    -- -   URL - The default mobile browser on the recipient\'s device opens
    --     and loads the web page at a URL that you specify.
    action :: Prelude.Maybe Action,
    -- | The sound to play when the recipient receives the push notification. You
    -- can use the default stream or specify the file name of a sound resource
    -- that\'s bundled in your app. On an Android platform, the sound file must
    -- reside in \/res\/raw\/.
    sound :: Prelude.Maybe Prelude.Text,
    -- | The URL to open in the recipient\'s default mobile browser, if a
    -- recipient taps the push notification and the value of the Action
    -- property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The URL of the small icon image to display in the status bar and the
    -- content view of the push notification.
    smallImageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The raw, JSON-formatted string to use as the payload for the
    -- notification message. If specified, this value overrides all other
    -- content for the message.
    rawContent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ADMMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'silentPush', 'aDMMessage_silentPush' - Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration or supporting phone home functionality.
--
-- 'imageIconUrl', 'aDMMessage_imageIconUrl' - The URL of the large icon image to display in the content view of the
-- push notification.
--
-- 'expiresAfter', 'aDMMessage_expiresAfter' - The amount of time, in seconds, that ADM should store the message if the
-- recipient\'s device is offline. Amazon Pinpoint specifies this value in
-- the expiresAfter parameter when it sends the notification message to
-- ADM.
--
-- 'data'', 'aDMMessage_data' - The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
--
-- 'title', 'aDMMessage_title' - The title to display above the notification message on the recipient\'s
-- device.
--
-- 'iconReference', 'aDMMessage_iconReference' - The icon image name of the asset saved in your app.
--
-- 'body', 'aDMMessage_body' - The body of the notification message.
--
-- 'consolidationKey', 'aDMMessage_consolidationKey' - An arbitrary string that indicates that multiple messages are logically
-- the same and that Amazon Device Messaging (ADM) can drop previously
-- enqueued messages in favor of this message.
--
-- 'substitutions', 'aDMMessage_substitutions' - The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
--
-- 'imageUrl', 'aDMMessage_imageUrl' - The URL of an image to display in the push notification.
--
-- 'md5', 'aDMMessage_md5' - The base64-encoded, MD5 checksum of the value specified by the Data
-- property. ADM uses the MD5 value to verify the integrity of the data.
--
-- 'action', 'aDMMessage_action' - The action to occur if the recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This action uses the deep-linking features of the
--     Android platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
--
-- 'sound', 'aDMMessage_sound' - The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
--
-- 'url', 'aDMMessage_url' - The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
--
-- 'smallImageIconUrl', 'aDMMessage_smallImageIconUrl' - The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
--
-- 'rawContent', 'aDMMessage_rawContent' - The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
newADMMessage ::
  ADMMessage
newADMMessage =
  ADMMessage'
    { silentPush = Prelude.Nothing,
      imageIconUrl = Prelude.Nothing,
      expiresAfter = Prelude.Nothing,
      data' = Prelude.Nothing,
      title = Prelude.Nothing,
      iconReference = Prelude.Nothing,
      body = Prelude.Nothing,
      consolidationKey = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      md5 = Prelude.Nothing,
      action = Prelude.Nothing,
      sound = Prelude.Nothing,
      url = Prelude.Nothing,
      smallImageIconUrl = Prelude.Nothing,
      rawContent = Prelude.Nothing
    }

-- | Specifies whether the notification is a silent push notification, which
-- is a push notification that doesn\'t display on a recipient\'s device.
-- Silent push notifications can be used for cases such as updating an
-- app\'s configuration or supporting phone home functionality.
aDMMessage_silentPush :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Bool)
aDMMessage_silentPush = Lens.lens (\ADMMessage' {silentPush} -> silentPush) (\s@ADMMessage' {} a -> s {silentPush = a} :: ADMMessage)

-- | The URL of the large icon image to display in the content view of the
-- push notification.
aDMMessage_imageIconUrl :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_imageIconUrl = Lens.lens (\ADMMessage' {imageIconUrl} -> imageIconUrl) (\s@ADMMessage' {} a -> s {imageIconUrl = a} :: ADMMessage)

-- | The amount of time, in seconds, that ADM should store the message if the
-- recipient\'s device is offline. Amazon Pinpoint specifies this value in
-- the expiresAfter parameter when it sends the notification message to
-- ADM.
aDMMessage_expiresAfter :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_expiresAfter = Lens.lens (\ADMMessage' {expiresAfter} -> expiresAfter) (\s@ADMMessage' {} a -> s {expiresAfter = a} :: ADMMessage)

-- | The JSON data payload to use for the push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
aDMMessage_data :: Lens.Lens' ADMMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
aDMMessage_data = Lens.lens (\ADMMessage' {data'} -> data') (\s@ADMMessage' {} a -> s {data' = a} :: ADMMessage) Prelude.. Lens.mapping Lens._Coerce

-- | The title to display above the notification message on the recipient\'s
-- device.
aDMMessage_title :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_title = Lens.lens (\ADMMessage' {title} -> title) (\s@ADMMessage' {} a -> s {title = a} :: ADMMessage)

-- | The icon image name of the asset saved in your app.
aDMMessage_iconReference :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_iconReference = Lens.lens (\ADMMessage' {iconReference} -> iconReference) (\s@ADMMessage' {} a -> s {iconReference = a} :: ADMMessage)

-- | The body of the notification message.
aDMMessage_body :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_body = Lens.lens (\ADMMessage' {body} -> body) (\s@ADMMessage' {} a -> s {body = a} :: ADMMessage)

-- | An arbitrary string that indicates that multiple messages are logically
-- the same and that Amazon Device Messaging (ADM) can drop previously
-- enqueued messages in favor of this message.
aDMMessage_consolidationKey :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_consolidationKey = Lens.lens (\ADMMessage' {consolidationKey} -> consolidationKey) (\s@ADMMessage' {} a -> s {consolidationKey = a} :: ADMMessage)

-- | The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
aDMMessage_substitutions :: Lens.Lens' ADMMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
aDMMessage_substitutions = Lens.lens (\ADMMessage' {substitutions} -> substitutions) (\s@ADMMessage' {} a -> s {substitutions = a} :: ADMMessage) Prelude.. Lens.mapping Lens._Coerce

-- | The URL of an image to display in the push notification.
aDMMessage_imageUrl :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_imageUrl = Lens.lens (\ADMMessage' {imageUrl} -> imageUrl) (\s@ADMMessage' {} a -> s {imageUrl = a} :: ADMMessage)

-- | The base64-encoded, MD5 checksum of the value specified by the Data
-- property. ADM uses the MD5 value to verify the integrity of the data.
aDMMessage_md5 :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_md5 = Lens.lens (\ADMMessage' {md5} -> md5) (\s@ADMMessage' {} a -> s {md5 = a} :: ADMMessage)

-- | The action to occur if the recipient taps the push notification. Valid
-- values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This action uses the deep-linking features of the
--     Android platform.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
aDMMessage_action :: Lens.Lens' ADMMessage (Prelude.Maybe Action)
aDMMessage_action = Lens.lens (\ADMMessage' {action} -> action) (\s@ADMMessage' {} a -> s {action = a} :: ADMMessage)

-- | The sound to play when the recipient receives the push notification. You
-- can use the default stream or specify the file name of a sound resource
-- that\'s bundled in your app. On an Android platform, the sound file must
-- reside in \/res\/raw\/.
aDMMessage_sound :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_sound = Lens.lens (\ADMMessage' {sound} -> sound) (\s@ADMMessage' {} a -> s {sound = a} :: ADMMessage)

-- | The URL to open in the recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
aDMMessage_url :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_url = Lens.lens (\ADMMessage' {url} -> url) (\s@ADMMessage' {} a -> s {url = a} :: ADMMessage)

-- | The URL of the small icon image to display in the status bar and the
-- content view of the push notification.
aDMMessage_smallImageIconUrl :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_smallImageIconUrl = Lens.lens (\ADMMessage' {smallImageIconUrl} -> smallImageIconUrl) (\s@ADMMessage' {} a -> s {smallImageIconUrl = a} :: ADMMessage)

-- | The raw, JSON-formatted string to use as the payload for the
-- notification message. If specified, this value overrides all other
-- content for the message.
aDMMessage_rawContent :: Lens.Lens' ADMMessage (Prelude.Maybe Prelude.Text)
aDMMessage_rawContent = Lens.lens (\ADMMessage' {rawContent} -> rawContent) (\s@ADMMessage' {} a -> s {rawContent = a} :: ADMMessage)

instance Prelude.Hashable ADMMessage

instance Prelude.NFData ADMMessage

instance Core.ToJSON ADMMessage where
  toJSON ADMMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SilentPush" Core..=) Prelude.<$> silentPush,
            ("ImageIconUrl" Core..=) Prelude.<$> imageIconUrl,
            ("ExpiresAfter" Core..=) Prelude.<$> expiresAfter,
            ("Data" Core..=) Prelude.<$> data',
            ("Title" Core..=) Prelude.<$> title,
            ("IconReference" Core..=) Prelude.<$> iconReference,
            ("Body" Core..=) Prelude.<$> body,
            ("ConsolidationKey" Core..=)
              Prelude.<$> consolidationKey,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("ImageUrl" Core..=) Prelude.<$> imageUrl,
            ("MD5" Core..=) Prelude.<$> md5,
            ("Action" Core..=) Prelude.<$> action,
            ("Sound" Core..=) Prelude.<$> sound,
            ("Url" Core..=) Prelude.<$> url,
            ("SmallImageIconUrl" Core..=)
              Prelude.<$> smallImageIconUrl,
            ("RawContent" Core..=) Prelude.<$> rawContent
          ]
      )
