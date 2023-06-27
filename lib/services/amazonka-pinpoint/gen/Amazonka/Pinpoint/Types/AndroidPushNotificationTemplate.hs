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
-- Module      : Amazonka.Pinpoint.Types.AndroidPushNotificationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.AndroidPushNotificationTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Specifies channel-specific content and settings for a message template
-- that can be used in push notifications that are sent through the ADM
-- (Amazon Device Messaging), Baidu (Baidu Cloud Push), or GCM (Firebase
-- Cloud Messaging, formerly Google Cloud Messaging) channel.
--
-- /See:/ 'newAndroidPushNotificationTemplate' smart constructor.
data AndroidPushNotificationTemplate = AndroidPushNotificationTemplate'
  { -- | The action to occur if a recipient taps a push notification that\'s
    -- based on the message template. Valid values are:
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
    -- | The message body to use in a push notification that\'s based on the
    -- message template.
    body :: Prelude.Maybe Prelude.Text,
    -- | The URL of the large icon image to display in the content view of a push
    -- notification that\'s based on the message template.
    imageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image to display in a push notification that\'s based on
    -- the message template.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The raw, JSON-formatted string to use as the payload for a push
    -- notification that\'s based on the message template. If specified, this
    -- value overrides all other content for the message template.
    rawContent :: Prelude.Maybe Prelude.Text,
    -- | The URL of the small icon image to display in the status bar and the
    -- content view of a push notification that\'s based on the message
    -- template.
    smallImageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The sound to play when a recipient receives a push notification that\'s
    -- based on the message template. You can use the default stream or specify
    -- the file name of a sound resource that\'s bundled in your app. On an
    -- Android platform, the sound file must reside in \/res\/raw\/.
    sound :: Prelude.Maybe Prelude.Text,
    -- | The title to use in a push notification that\'s based on the message
    -- template. This title appears above the notification message on a
    -- recipient\'s device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The URL to open in a recipient\'s default mobile browser, if a recipient
    -- taps a push notification that\'s based on the message template and the
    -- value of the Action property is URL.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AndroidPushNotificationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'androidPushNotificationTemplate_action' - The action to occur if a recipient taps a push notification that\'s
-- based on the message template. Valid values are:
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
-- 'body', 'androidPushNotificationTemplate_body' - The message body to use in a push notification that\'s based on the
-- message template.
--
-- 'imageIconUrl', 'androidPushNotificationTemplate_imageIconUrl' - The URL of the large icon image to display in the content view of a push
-- notification that\'s based on the message template.
--
-- 'imageUrl', 'androidPushNotificationTemplate_imageUrl' - The URL of an image to display in a push notification that\'s based on
-- the message template.
--
-- 'rawContent', 'androidPushNotificationTemplate_rawContent' - The raw, JSON-formatted string to use as the payload for a push
-- notification that\'s based on the message template. If specified, this
-- value overrides all other content for the message template.
--
-- 'smallImageIconUrl', 'androidPushNotificationTemplate_smallImageIconUrl' - The URL of the small icon image to display in the status bar and the
-- content view of a push notification that\'s based on the message
-- template.
--
-- 'sound', 'androidPushNotificationTemplate_sound' - The sound to play when a recipient receives a push notification that\'s
-- based on the message template. You can use the default stream or specify
-- the file name of a sound resource that\'s bundled in your app. On an
-- Android platform, the sound file must reside in \/res\/raw\/.
--
-- 'title', 'androidPushNotificationTemplate_title' - The title to use in a push notification that\'s based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
--
-- 'url', 'androidPushNotificationTemplate_url' - The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps a push notification that\'s based on the message template and the
-- value of the Action property is URL.
newAndroidPushNotificationTemplate ::
  AndroidPushNotificationTemplate
newAndroidPushNotificationTemplate =
  AndroidPushNotificationTemplate'
    { action =
        Prelude.Nothing,
      body = Prelude.Nothing,
      imageIconUrl = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      rawContent = Prelude.Nothing,
      smallImageIconUrl = Prelude.Nothing,
      sound = Prelude.Nothing,
      title = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The action to occur if a recipient taps a push notification that\'s
-- based on the message template. Valid values are:
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
androidPushNotificationTemplate_action :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Action)
androidPushNotificationTemplate_action = Lens.lens (\AndroidPushNotificationTemplate' {action} -> action) (\s@AndroidPushNotificationTemplate' {} a -> s {action = a} :: AndroidPushNotificationTemplate)

-- | The message body to use in a push notification that\'s based on the
-- message template.
androidPushNotificationTemplate_body :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_body = Lens.lens (\AndroidPushNotificationTemplate' {body} -> body) (\s@AndroidPushNotificationTemplate' {} a -> s {body = a} :: AndroidPushNotificationTemplate)

-- | The URL of the large icon image to display in the content view of a push
-- notification that\'s based on the message template.
androidPushNotificationTemplate_imageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_imageIconUrl = Lens.lens (\AndroidPushNotificationTemplate' {imageIconUrl} -> imageIconUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {imageIconUrl = a} :: AndroidPushNotificationTemplate)

-- | The URL of an image to display in a push notification that\'s based on
-- the message template.
androidPushNotificationTemplate_imageUrl :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_imageUrl = Lens.lens (\AndroidPushNotificationTemplate' {imageUrl} -> imageUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {imageUrl = a} :: AndroidPushNotificationTemplate)

-- | The raw, JSON-formatted string to use as the payload for a push
-- notification that\'s based on the message template. If specified, this
-- value overrides all other content for the message template.
androidPushNotificationTemplate_rawContent :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_rawContent = Lens.lens (\AndroidPushNotificationTemplate' {rawContent} -> rawContent) (\s@AndroidPushNotificationTemplate' {} a -> s {rawContent = a} :: AndroidPushNotificationTemplate)

-- | The URL of the small icon image to display in the status bar and the
-- content view of a push notification that\'s based on the message
-- template.
androidPushNotificationTemplate_smallImageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_smallImageIconUrl = Lens.lens (\AndroidPushNotificationTemplate' {smallImageIconUrl} -> smallImageIconUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {smallImageIconUrl = a} :: AndroidPushNotificationTemplate)

-- | The sound to play when a recipient receives a push notification that\'s
-- based on the message template. You can use the default stream or specify
-- the file name of a sound resource that\'s bundled in your app. On an
-- Android platform, the sound file must reside in \/res\/raw\/.
androidPushNotificationTemplate_sound :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_sound = Lens.lens (\AndroidPushNotificationTemplate' {sound} -> sound) (\s@AndroidPushNotificationTemplate' {} a -> s {sound = a} :: AndroidPushNotificationTemplate)

-- | The title to use in a push notification that\'s based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
androidPushNotificationTemplate_title :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_title = Lens.lens (\AndroidPushNotificationTemplate' {title} -> title) (\s@AndroidPushNotificationTemplate' {} a -> s {title = a} :: AndroidPushNotificationTemplate)

-- | The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps a push notification that\'s based on the message template and the
-- value of the Action property is URL.
androidPushNotificationTemplate_url :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_url = Lens.lens (\AndroidPushNotificationTemplate' {url} -> url) (\s@AndroidPushNotificationTemplate' {} a -> s {url = a} :: AndroidPushNotificationTemplate)

instance
  Data.FromJSON
    AndroidPushNotificationTemplate
  where
  parseJSON =
    Data.withObject
      "AndroidPushNotificationTemplate"
      ( \x ->
          AndroidPushNotificationTemplate'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "Body")
            Prelude.<*> (x Data..:? "ImageIconUrl")
            Prelude.<*> (x Data..:? "ImageUrl")
            Prelude.<*> (x Data..:? "RawContent")
            Prelude.<*> (x Data..:? "SmallImageIconUrl")
            Prelude.<*> (x Data..:? "Sound")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..:? "Url")
      )

instance
  Prelude.Hashable
    AndroidPushNotificationTemplate
  where
  hashWithSalt
    _salt
    AndroidPushNotificationTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` action
        `Prelude.hashWithSalt` body
        `Prelude.hashWithSalt` imageIconUrl
        `Prelude.hashWithSalt` imageUrl
        `Prelude.hashWithSalt` rawContent
        `Prelude.hashWithSalt` smallImageIconUrl
        `Prelude.hashWithSalt` sound
        `Prelude.hashWithSalt` title
        `Prelude.hashWithSalt` url

instance
  Prelude.NFData
    AndroidPushNotificationTemplate
  where
  rnf AndroidPushNotificationTemplate' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf body
      `Prelude.seq` Prelude.rnf imageIconUrl
      `Prelude.seq` Prelude.rnf imageUrl
      `Prelude.seq` Prelude.rnf rawContent
      `Prelude.seq` Prelude.rnf smallImageIconUrl
      `Prelude.seq` Prelude.rnf sound
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf url

instance Data.ToJSON AndroidPushNotificationTemplate where
  toJSON AndroidPushNotificationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("Body" Data..=) Prelude.<$> body,
            ("ImageIconUrl" Data..=) Prelude.<$> imageIconUrl,
            ("ImageUrl" Data..=) Prelude.<$> imageUrl,
            ("RawContent" Data..=) Prelude.<$> rawContent,
            ("SmallImageIconUrl" Data..=)
              Prelude.<$> smallImageIconUrl,
            ("Sound" Data..=) Prelude.<$> sound,
            ("Title" Data..=) Prelude.<$> title,
            ("Url" Data..=) Prelude.<$> url
          ]
      )
