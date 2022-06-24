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
-- Module      : Amazonka.Pinpoint.Types.DefaultPushNotificationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.DefaultPushNotificationTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Specifies the default settings and content for a message template that
-- can be used in messages that are sent through a push notification
-- channel.
--
-- /See:/ 'newDefaultPushNotificationTemplate' smart constructor.
data DefaultPushNotificationTemplate = DefaultPushNotificationTemplate'
  { -- | The message body to use in push notifications that are based on the
    -- message template.
    body :: Prelude.Maybe Prelude.Text,
    -- | The URL to open in a recipient\'s default mobile browser, if a recipient
    -- taps a push notification that\'s based on the message template and the
    -- value of the Action property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The sound to play when a recipient receives a push notification that\'s
    -- based on the message template. You can use the default stream or specify
    -- the file name of a sound resource that\'s bundled in your app. On an
    -- Android platform, the sound file must reside in \/res\/raw\/.
    --
    -- For an iOS platform, this value is the key for the name of a sound file
    -- in your app\'s main bundle or the Library\/Sounds folder in your app\'s
    -- data container. If the sound file can\'t be found or you specify default
    -- for the value, the system plays the default alert sound.
    sound :: Prelude.Maybe Prelude.Text,
    -- | The title to use in push notifications that are based on the message
    -- template. This title appears above the notification message on a
    -- recipient\'s device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The action to occur if a recipient taps a push notification that\'s
    -- based on the message template. Valid values are:
    --
    -- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
    --     sent to the background. This is the default action.
    --
    -- -   DEEP_LINK - Your app opens and displays a designated user interface
    --     in the app. This setting uses the deep-linking features of the iOS
    --     and Android platforms.
    --
    -- -   URL - The default mobile browser on the recipient\'s device opens
    --     and loads the web page at a URL that you specify.
    action :: Prelude.Maybe Action
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultPushNotificationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'defaultPushNotificationTemplate_body' - The message body to use in push notifications that are based on the
-- message template.
--
-- 'url', 'defaultPushNotificationTemplate_url' - The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps a push notification that\'s based on the message template and the
-- value of the Action property is URL.
--
-- 'sound', 'defaultPushNotificationTemplate_sound' - The sound to play when a recipient receives a push notification that\'s
-- based on the message template. You can use the default stream or specify
-- the file name of a sound resource that\'s bundled in your app. On an
-- Android platform, the sound file must reside in \/res\/raw\/.
--
-- For an iOS platform, this value is the key for the name of a sound file
-- in your app\'s main bundle or the Library\/Sounds folder in your app\'s
-- data container. If the sound file can\'t be found or you specify default
-- for the value, the system plays the default alert sound.
--
-- 'title', 'defaultPushNotificationTemplate_title' - The title to use in push notifications that are based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
--
-- 'action', 'defaultPushNotificationTemplate_action' - The action to occur if a recipient taps a push notification that\'s
-- based on the message template. Valid values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of the iOS
--     and Android platforms.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
newDefaultPushNotificationTemplate ::
  DefaultPushNotificationTemplate
newDefaultPushNotificationTemplate =
  DefaultPushNotificationTemplate'
    { body =
        Prelude.Nothing,
      url = Prelude.Nothing,
      sound = Prelude.Nothing,
      title = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The message body to use in push notifications that are based on the
-- message template.
defaultPushNotificationTemplate_body :: Lens.Lens' DefaultPushNotificationTemplate (Prelude.Maybe Prelude.Text)
defaultPushNotificationTemplate_body = Lens.lens (\DefaultPushNotificationTemplate' {body} -> body) (\s@DefaultPushNotificationTemplate' {} a -> s {body = a} :: DefaultPushNotificationTemplate)

-- | The URL to open in a recipient\'s default mobile browser, if a recipient
-- taps a push notification that\'s based on the message template and the
-- value of the Action property is URL.
defaultPushNotificationTemplate_url :: Lens.Lens' DefaultPushNotificationTemplate (Prelude.Maybe Prelude.Text)
defaultPushNotificationTemplate_url = Lens.lens (\DefaultPushNotificationTemplate' {url} -> url) (\s@DefaultPushNotificationTemplate' {} a -> s {url = a} :: DefaultPushNotificationTemplate)

-- | The sound to play when a recipient receives a push notification that\'s
-- based on the message template. You can use the default stream or specify
-- the file name of a sound resource that\'s bundled in your app. On an
-- Android platform, the sound file must reside in \/res\/raw\/.
--
-- For an iOS platform, this value is the key for the name of a sound file
-- in your app\'s main bundle or the Library\/Sounds folder in your app\'s
-- data container. If the sound file can\'t be found or you specify default
-- for the value, the system plays the default alert sound.
defaultPushNotificationTemplate_sound :: Lens.Lens' DefaultPushNotificationTemplate (Prelude.Maybe Prelude.Text)
defaultPushNotificationTemplate_sound = Lens.lens (\DefaultPushNotificationTemplate' {sound} -> sound) (\s@DefaultPushNotificationTemplate' {} a -> s {sound = a} :: DefaultPushNotificationTemplate)

-- | The title to use in push notifications that are based on the message
-- template. This title appears above the notification message on a
-- recipient\'s device.
defaultPushNotificationTemplate_title :: Lens.Lens' DefaultPushNotificationTemplate (Prelude.Maybe Prelude.Text)
defaultPushNotificationTemplate_title = Lens.lens (\DefaultPushNotificationTemplate' {title} -> title) (\s@DefaultPushNotificationTemplate' {} a -> s {title = a} :: DefaultPushNotificationTemplate)

-- | The action to occur if a recipient taps a push notification that\'s
-- based on the message template. Valid values are:
--
-- -   OPEN_APP - Your app opens or it becomes the foreground app if it was
--     sent to the background. This is the default action.
--
-- -   DEEP_LINK - Your app opens and displays a designated user interface
--     in the app. This setting uses the deep-linking features of the iOS
--     and Android platforms.
--
-- -   URL - The default mobile browser on the recipient\'s device opens
--     and loads the web page at a URL that you specify.
defaultPushNotificationTemplate_action :: Lens.Lens' DefaultPushNotificationTemplate (Prelude.Maybe Action)
defaultPushNotificationTemplate_action = Lens.lens (\DefaultPushNotificationTemplate' {action} -> action) (\s@DefaultPushNotificationTemplate' {} a -> s {action = a} :: DefaultPushNotificationTemplate)

instance
  Core.FromJSON
    DefaultPushNotificationTemplate
  where
  parseJSON =
    Core.withObject
      "DefaultPushNotificationTemplate"
      ( \x ->
          DefaultPushNotificationTemplate'
            Prelude.<$> (x Core..:? "Body")
            Prelude.<*> (x Core..:? "Url")
            Prelude.<*> (x Core..:? "Sound")
            Prelude.<*> (x Core..:? "Title")
            Prelude.<*> (x Core..:? "Action")
      )

instance
  Prelude.Hashable
    DefaultPushNotificationTemplate
  where
  hashWithSalt
    _salt
    DefaultPushNotificationTemplate' {..} =
      _salt `Prelude.hashWithSalt` body
        `Prelude.hashWithSalt` url
        `Prelude.hashWithSalt` sound
        `Prelude.hashWithSalt` title
        `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    DefaultPushNotificationTemplate
  where
  rnf DefaultPushNotificationTemplate' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf sound
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf action

instance Core.ToJSON DefaultPushNotificationTemplate where
  toJSON DefaultPushNotificationTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Body" Core..=) Prelude.<$> body,
            ("Url" Core..=) Prelude.<$> url,
            ("Sound" Core..=) Prelude.<$> sound,
            ("Title" Core..=) Prelude.<$> title,
            ("Action" Core..=) Prelude.<$> action
          ]
      )
