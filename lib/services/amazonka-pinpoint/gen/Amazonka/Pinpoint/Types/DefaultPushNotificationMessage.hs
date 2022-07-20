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
-- Module      : Amazonka.Pinpoint.Types.DefaultPushNotificationMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.DefaultPushNotificationMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Action
import qualified Amazonka.Prelude as Prelude

-- | Specifies the default settings and content for a push notification
-- that\'s sent directly to an endpoint.
--
-- /See:/ 'newDefaultPushNotificationMessage' smart constructor.
data DefaultPushNotificationMessage = DefaultPushNotificationMessage'
  { -- | The default body of the notification message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The default URL to open in a recipient\'s default mobile browser, if a
    -- recipient taps the push notification and the value of the Action
    -- property is URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | The default message variables to use in the notification message. You
    -- can override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | Specifies whether the default notification is a silent push
    -- notification, which is a push notification that doesn\'t display on a
    -- recipient\'s device. Silent push notifications can be used for cases
    -- such as updating an app\'s configuration or delivering messages to an
    -- in-app notification center.
    silentPush :: Prelude.Maybe Prelude.Bool,
    -- | The default title to display above the notification message on a
    -- recipient\'s device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The default action to occur if a recipient taps the push notification.
    -- Valid values are:
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
    action :: Prelude.Maybe Action,
    -- | The JSON data payload to use for the default push notification, if the
    -- notification is a silent push notification. This payload is added to the
    -- data.pinpoint.jsonBody object of the notification.
    data' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultPushNotificationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'defaultPushNotificationMessage_body' - The default body of the notification message.
--
-- 'url', 'defaultPushNotificationMessage_url' - The default URL to open in a recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
--
-- 'substitutions', 'defaultPushNotificationMessage_substitutions' - The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
--
-- 'silentPush', 'defaultPushNotificationMessage_silentPush' - Specifies whether the default notification is a silent push
-- notification, which is a push notification that doesn\'t display on a
-- recipient\'s device. Silent push notifications can be used for cases
-- such as updating an app\'s configuration or delivering messages to an
-- in-app notification center.
--
-- 'title', 'defaultPushNotificationMessage_title' - The default title to display above the notification message on a
-- recipient\'s device.
--
-- 'action', 'defaultPushNotificationMessage_action' - The default action to occur if a recipient taps the push notification.
-- Valid values are:
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
--
-- 'data'', 'defaultPushNotificationMessage_data' - The JSON data payload to use for the default push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
newDefaultPushNotificationMessage ::
  DefaultPushNotificationMessage
newDefaultPushNotificationMessage =
  DefaultPushNotificationMessage'
    { body =
        Prelude.Nothing,
      url = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      silentPush = Prelude.Nothing,
      title = Prelude.Nothing,
      action = Prelude.Nothing,
      data' = Prelude.Nothing
    }

-- | The default body of the notification message.
defaultPushNotificationMessage_body :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Text)
defaultPushNotificationMessage_body = Lens.lens (\DefaultPushNotificationMessage' {body} -> body) (\s@DefaultPushNotificationMessage' {} a -> s {body = a} :: DefaultPushNotificationMessage)

-- | The default URL to open in a recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
defaultPushNotificationMessage_url :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Text)
defaultPushNotificationMessage_url = Lens.lens (\DefaultPushNotificationMessage' {url} -> url) (\s@DefaultPushNotificationMessage' {} a -> s {url = a} :: DefaultPushNotificationMessage)

-- | The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
defaultPushNotificationMessage_substitutions :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
defaultPushNotificationMessage_substitutions = Lens.lens (\DefaultPushNotificationMessage' {substitutions} -> substitutions) (\s@DefaultPushNotificationMessage' {} a -> s {substitutions = a} :: DefaultPushNotificationMessage) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the default notification is a silent push
-- notification, which is a push notification that doesn\'t display on a
-- recipient\'s device. Silent push notifications can be used for cases
-- such as updating an app\'s configuration or delivering messages to an
-- in-app notification center.
defaultPushNotificationMessage_silentPush :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Bool)
defaultPushNotificationMessage_silentPush = Lens.lens (\DefaultPushNotificationMessage' {silentPush} -> silentPush) (\s@DefaultPushNotificationMessage' {} a -> s {silentPush = a} :: DefaultPushNotificationMessage)

-- | The default title to display above the notification message on a
-- recipient\'s device.
defaultPushNotificationMessage_title :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Text)
defaultPushNotificationMessage_title = Lens.lens (\DefaultPushNotificationMessage' {title} -> title) (\s@DefaultPushNotificationMessage' {} a -> s {title = a} :: DefaultPushNotificationMessage)

-- | The default action to occur if a recipient taps the push notification.
-- Valid values are:
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
defaultPushNotificationMessage_action :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Action)
defaultPushNotificationMessage_action = Lens.lens (\DefaultPushNotificationMessage' {action} -> action) (\s@DefaultPushNotificationMessage' {} a -> s {action = a} :: DefaultPushNotificationMessage)

-- | The JSON data payload to use for the default push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
defaultPushNotificationMessage_data :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
defaultPushNotificationMessage_data = Lens.lens (\DefaultPushNotificationMessage' {data'} -> data') (\s@DefaultPushNotificationMessage' {} a -> s {data' = a} :: DefaultPushNotificationMessage) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    DefaultPushNotificationMessage
  where
  hashWithSalt
    _salt
    DefaultPushNotificationMessage' {..} =
      _salt `Prelude.hashWithSalt` body
        `Prelude.hashWithSalt` url
        `Prelude.hashWithSalt` substitutions
        `Prelude.hashWithSalt` silentPush
        `Prelude.hashWithSalt` title
        `Prelude.hashWithSalt` action
        `Prelude.hashWithSalt` data'

instance
  Prelude.NFData
    DefaultPushNotificationMessage
  where
  rnf DefaultPushNotificationMessage' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf substitutions
      `Prelude.seq` Prelude.rnf silentPush
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf data'

instance Core.ToJSON DefaultPushNotificationMessage where
  toJSON DefaultPushNotificationMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Body" Core..=) Prelude.<$> body,
            ("Url" Core..=) Prelude.<$> url,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("SilentPush" Core..=) Prelude.<$> silentPush,
            ("Title" Core..=) Prelude.<$> title,
            ("Action" Core..=) Prelude.<$> action,
            ("Data" Core..=) Prelude.<$> data'
          ]
      )
