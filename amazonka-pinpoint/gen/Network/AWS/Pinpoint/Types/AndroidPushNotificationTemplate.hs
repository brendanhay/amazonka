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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action

-- | Specifies channel-specific content and settings for a message template
-- that can be used in push notifications that are sent through the ADM
-- (Amazon Device Messaging), Baidu (Baidu Cloud Push), or GCM (Firebase
-- Cloud Messaging, formerly Google Cloud Messaging) channel.
--
-- /See:/ 'newAndroidPushNotificationTemplate' smart constructor.
data AndroidPushNotificationTemplate = AndroidPushNotificationTemplate'
  { -- | The URL of the large icon image to display in the content view of a push
    -- notification that\'s based on the message template.
    imageIconUrl :: Core.Maybe Core.Text,
    -- | The title to use in a push notification that\'s based on the message
    -- template. This title appears above the notification message on a
    -- recipient\'s device.
    title :: Core.Maybe Core.Text,
    -- | The message body to use in a push notification that\'s based on the
    -- message template.
    body :: Core.Maybe Core.Text,
    -- | The URL of an image to display in a push notification that\'s based on
    -- the message template.
    imageUrl :: Core.Maybe Core.Text,
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
    action :: Core.Maybe Action,
    -- | The sound to play when a recipient receives a push notification that\'s
    -- based on the message template. You can use the default stream or specify
    -- the file name of a sound resource that\'s bundled in your app. On an
    -- Android platform, the sound file must reside in \/res\/raw\/.
    sound :: Core.Maybe Core.Text,
    -- | The URL to open in a recipient\'s default mobile browser, if a recipient
    -- taps a push notification that\'s based on the message template and the
    -- value of the Action property is URL.
    url :: Core.Maybe Core.Text,
    -- | The URL of the small icon image to display in the status bar and the
    -- content view of a push notification that\'s based on the message
    -- template.
    smallImageIconUrl :: Core.Maybe Core.Text,
    -- | The raw, JSON-formatted string to use as the payload for a push
    -- notification that\'s based on the message template. If specified, this
    -- value overrides all other content for the message template.
    rawContent :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      title = Core.Nothing,
      body = Core.Nothing,
      imageUrl = Core.Nothing,
      action = Core.Nothing,
      sound = Core.Nothing,
      url = Core.Nothing,
      smallImageIconUrl = Core.Nothing,
      rawContent = Core.Nothing
    }

-- | The URL of the large icon image to display in the content view of a push
-- notification that\'s based on the message template.
androidPushNotificationTemplate_imageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
androidPushNotificationTemplate_imageIconUrl = Lens.lens (\AndroidPushNotificationTemplate' {imageIconUrl} -> imageIconUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {imageIconUrl = a} :: AndroidPushNotificationTemplate)

-- | The title to use in a push notification that\'s based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
androidPushNotificationTemplate_title :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
androidPushNotificationTemplate_title = Lens.lens (\AndroidPushNotificationTemplate' {title} -> title) (\s@AndroidPushNotificationTemplate' {} a -> s {title = a} :: AndroidPushNotificationTemplate)

-- | The message body to use in a push notification that\'s based on the
-- message template.
androidPushNotificationTemplate_body :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
androidPushNotificationTemplate_body = Lens.lens (\AndroidPushNotificationTemplate' {body} -> body) (\s@AndroidPushNotificationTemplate' {} a -> s {body = a} :: AndroidPushNotificationTemplate)

-- | The URL of an image to display in a push notification that\'s based on
-- the message template.
androidPushNotificationTemplate_imageUrl :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
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
androidPushNotificationTemplate_action :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Action)
androidPushNotificationTemplate_action = Lens.lens (\AndroidPushNotificationTemplate' {action} -> action) (\s@AndroidPushNotificationTemplate' {} a -> s {action = a} :: AndroidPushNotificationTemplate)

-- | The sound to play when a recipient receives a push notification that\'s
-- based on the message template. You can use the default stream or specify
-- the file name of a sound resource that\'s bundled in your app. On an
-- Android platform, the sound file must reside in \/res\/raw\/.
androidPushNotificationTemplate_sound :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
androidPushNotificationTemplate_sound = Lens.lens (\AndroidPushNotificationTemplate' {sound} -> sound) (\s@AndroidPushNotificationTemplate' {} a -> s {sound = a} :: AndroidPushNotificationTemplate)

-- | The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps a push notification that\'s based on the message template and the
-- value of the Action property is URL.
androidPushNotificationTemplate_url :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
androidPushNotificationTemplate_url = Lens.lens (\AndroidPushNotificationTemplate' {url} -> url) (\s@AndroidPushNotificationTemplate' {} a -> s {url = a} :: AndroidPushNotificationTemplate)

-- | The URL of the small icon image to display in the status bar and the
-- content view of a push notification that\'s based on the message
-- template.
androidPushNotificationTemplate_smallImageIconUrl :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
androidPushNotificationTemplate_smallImageIconUrl = Lens.lens (\AndroidPushNotificationTemplate' {smallImageIconUrl} -> smallImageIconUrl) (\s@AndroidPushNotificationTemplate' {} a -> s {smallImageIconUrl = a} :: AndroidPushNotificationTemplate)

-- | The raw, JSON-formatted string to use as the payload for a push
-- notification that\'s based on the message template. If specified, this
-- value overrides all other content for the message template.
androidPushNotificationTemplate_rawContent :: Lens.Lens' AndroidPushNotificationTemplate (Core.Maybe Core.Text)
androidPushNotificationTemplate_rawContent = Lens.lens (\AndroidPushNotificationTemplate' {rawContent} -> rawContent) (\s@AndroidPushNotificationTemplate' {} a -> s {rawContent = a} :: AndroidPushNotificationTemplate)

instance
  Core.FromJSON
    AndroidPushNotificationTemplate
  where
  parseJSON =
    Core.withObject
      "AndroidPushNotificationTemplate"
      ( \x ->
          AndroidPushNotificationTemplate'
            Core.<$> (x Core..:? "ImageIconUrl")
            Core.<*> (x Core..:? "Title")
            Core.<*> (x Core..:? "Body")
            Core.<*> (x Core..:? "ImageUrl")
            Core.<*> (x Core..:? "Action")
            Core.<*> (x Core..:? "Sound")
            Core.<*> (x Core..:? "Url")
            Core.<*> (x Core..:? "SmallImageIconUrl")
            Core.<*> (x Core..:? "RawContent")
      )

instance
  Core.Hashable
    AndroidPushNotificationTemplate

instance Core.NFData AndroidPushNotificationTemplate

instance Core.ToJSON AndroidPushNotificationTemplate where
  toJSON AndroidPushNotificationTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ImageIconUrl" Core..=) Core.<$> imageIconUrl,
            ("Title" Core..=) Core.<$> title,
            ("Body" Core..=) Core.<$> body,
            ("ImageUrl" Core..=) Core.<$> imageUrl,
            ("Action" Core..=) Core.<$> action,
            ("Sound" Core..=) Core.<$> sound,
            ("Url" Core..=) Core.<$> url,
            ("SmallImageIconUrl" Core..=)
              Core.<$> smallImageIconUrl,
            ("RawContent" Core..=) Core.<$> rawContent
          ]
      )
