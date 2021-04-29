{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AndroidPushNotificationTemplate where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Prelude

-- | Specifies channel-specific content and settings for a message template
-- that can be used in push notifications that are sent through the ADM
-- (Amazon Device Messaging), Baidu (Baidu Cloud Push), or GCM (Firebase
-- Cloud Messaging, formerly Google Cloud Messaging) channel.
--
-- /See:/ 'newAndroidPushNotificationTemplate' smart constructor.
data AndroidPushNotificationTemplate = AndroidPushNotificationTemplate'
  { -- | The URL of the large icon image to display in the content view of a push
    -- notification that\'s based on the message template.
    imageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The title to use in a push notification that\'s based on the message
    -- template. This title appears above the notification message on a
    -- recipient\'s device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The message body to use in a push notification that\'s based on the
    -- message template.
    body :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image to display in a push notification that\'s based on
    -- the message template.
    imageUrl :: Prelude.Maybe Prelude.Text,
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
    action :: Prelude.Maybe Action,
    -- | The sound to play when a recipient receives a push notification that\'s
    -- based on the message template. You can use the default stream or specify
    -- the file name of a sound resource that\'s bundled in your app. On an
    -- Android platform, the sound file must reside in \/res\/raw\/.
    sound :: Prelude.Maybe Prelude.Text,
    -- | The URL to open in a recipient\'s default mobile browser, if a recipient
    -- taps a push notification that\'s based on the message template and the
    -- value of the Action property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The URL of the small icon image to display in the status bar and the
    -- content view of a push notification that\'s based on the message
    -- template.
    smallImageIconUrl :: Prelude.Maybe Prelude.Text,
    -- | The raw, JSON-formatted string to use as the payload for a push
    -- notification that\'s based on the message template. If specified, this
    -- value overrides all other content for the message template.
    rawContent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AndroidPushNotificationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageIconUrl', 'androidPushNotificationTemplate_imageIconUrl' - The URL of the large icon image to display in the content view of a push
-- notification that\'s based on the message template.
--
-- 'title', 'androidPushNotificationTemplate_title' - The title to use in a push notification that\'s based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
--
-- 'body', 'androidPushNotificationTemplate_body' - The message body to use in a push notification that\'s based on the
-- message template.
--
-- 'imageUrl', 'androidPushNotificationTemplate_imageUrl' - The URL of an image to display in a push notification that\'s based on
-- the message template.
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
-- 'sound', 'androidPushNotificationTemplate_sound' - The sound to play when a recipient receives a push notification that\'s
-- based on the message template. You can use the default stream or specify
-- the file name of a sound resource that\'s bundled in your app. On an
-- Android platform, the sound file must reside in \/res\/raw\/.
--
-- 'url', 'androidPushNotificationTemplate_url' - The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps a push notification that\'s based on the message template and the
-- value of the Action property is URL.
--
-- 'smallImageIconUrl', 'androidPushNotificationTemplate_smallImageIconUrl' - The URL of the small icon image to display in the status bar and the
-- content view of a push notification that\'s based on the message
-- template.
--
-- 'rawContent', 'androidPushNotificationTemplate_rawContent' - The raw, JSON-formatted string to use as the payload for a push
-- notification that\'s based on the message template. If specified, this
-- value overrides all other content for the message template.
newAndroidPushNotificationTemplate ::
  AndroidPushNotificationTemplate
newAndroidPushNotificationTemplate =
  AndroidPushNotificationTemplate'
    { imageIconUrl =
        Prelude.Nothing,
      title = Prelude.Nothing,
      body = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      action = Prelude.Nothing,
      sound = Prelude.Nothing,
      url = Prelude.Nothing,
      smallImageIconUrl = Prelude.Nothing,
      rawContent = Prelude.Nothing
    }

-- | The URL of the large icon image to display in the content view of a push
-- notification that\'s based on the message template.
androidPushNotificationTemplate_imageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_imageIconUrl = Lens.lens (\AndroidPushNotificationTemplate' {imageIconUrl} -> imageIconUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {imageIconUrl = a} :: AndroidPushNotificationTemplate)

-- | The title to use in a push notification that\'s based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
androidPushNotificationTemplate_title :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_title = Lens.lens (\AndroidPushNotificationTemplate' {title} -> title) (\s@AndroidPushNotificationTemplate' {} a -> s {title = a} :: AndroidPushNotificationTemplate)

-- | The message body to use in a push notification that\'s based on the
-- message template.
androidPushNotificationTemplate_body :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_body = Lens.lens (\AndroidPushNotificationTemplate' {body} -> body) (\s@AndroidPushNotificationTemplate' {} a -> s {body = a} :: AndroidPushNotificationTemplate)

-- | The URL of an image to display in a push notification that\'s based on
-- the message template.
androidPushNotificationTemplate_imageUrl :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_imageUrl = Lens.lens (\AndroidPushNotificationTemplate' {imageUrl} -> imageUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {imageUrl = a} :: AndroidPushNotificationTemplate)

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

-- | The sound to play when a recipient receives a push notification that\'s
-- based on the message template. You can use the default stream or specify
-- the file name of a sound resource that\'s bundled in your app. On an
-- Android platform, the sound file must reside in \/res\/raw\/.
androidPushNotificationTemplate_sound :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_sound = Lens.lens (\AndroidPushNotificationTemplate' {sound} -> sound) (\s@AndroidPushNotificationTemplate' {} a -> s {sound = a} :: AndroidPushNotificationTemplate)

-- | The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps a push notification that\'s based on the message template and the
-- value of the Action property is URL.
androidPushNotificationTemplate_url :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_url = Lens.lens (\AndroidPushNotificationTemplate' {url} -> url) (\s@AndroidPushNotificationTemplate' {} a -> s {url = a} :: AndroidPushNotificationTemplate)

-- | The URL of the small icon image to display in the status bar and the
-- content view of a push notification that\'s based on the message
-- template.
androidPushNotificationTemplate_smallImageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_smallImageIconUrl = Lens.lens (\AndroidPushNotificationTemplate' {smallImageIconUrl} -> smallImageIconUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {smallImageIconUrl = a} :: AndroidPushNotificationTemplate)

-- | The raw, JSON-formatted string to use as the payload for a push
-- notification that\'s based on the message template. If specified, this
-- value overrides all other content for the message template.
androidPushNotificationTemplate_rawContent :: Lens.Lens' AndroidPushNotificationTemplate (Prelude.Maybe Prelude.Text)
androidPushNotificationTemplate_rawContent = Lens.lens (\AndroidPushNotificationTemplate' {rawContent} -> rawContent) (\s@AndroidPushNotificationTemplate' {} a -> s {rawContent = a} :: AndroidPushNotificationTemplate)

instance
  Prelude.FromJSON
    AndroidPushNotificationTemplate
  where
  parseJSON =
    Prelude.withObject
      "AndroidPushNotificationTemplate"
      ( \x ->
          AndroidPushNotificationTemplate'
            Prelude.<$> (x Prelude..:? "ImageIconUrl")
            Prelude.<*> (x Prelude..:? "Title")
            Prelude.<*> (x Prelude..:? "Body")
            Prelude.<*> (x Prelude..:? "ImageUrl")
            Prelude.<*> (x Prelude..:? "Action")
            Prelude.<*> (x Prelude..:? "Sound")
            Prelude.<*> (x Prelude..:? "Url")
            Prelude.<*> (x Prelude..:? "SmallImageIconUrl")
            Prelude.<*> (x Prelude..:? "RawContent")
      )

instance
  Prelude.Hashable
    AndroidPushNotificationTemplate

instance
  Prelude.NFData
    AndroidPushNotificationTemplate

instance
  Prelude.ToJSON
    AndroidPushNotificationTemplate
  where
  toJSON AndroidPushNotificationTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ImageIconUrl" Prelude..=)
              Prelude.<$> imageIconUrl,
            ("Title" Prelude..=) Prelude.<$> title,
            ("Body" Prelude..=) Prelude.<$> body,
            ("ImageUrl" Prelude..=) Prelude.<$> imageUrl,
            ("Action" Prelude..=) Prelude.<$> action,
            ("Sound" Prelude..=) Prelude.<$> sound,
            ("Url" Prelude..=) Prelude.<$> url,
            ("SmallImageIconUrl" Prelude..=)
              Prelude.<$> smallImageIconUrl,
            ("RawContent" Prelude..=) Prelude.<$> rawContent
          ]
      )
