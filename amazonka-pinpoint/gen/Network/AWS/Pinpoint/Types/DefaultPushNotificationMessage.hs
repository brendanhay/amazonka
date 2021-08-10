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
-- Module      : Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefaultPushNotificationMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Action
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the default settings and content for a push notification
-- that\'s sent directly to an endpoint.
--
-- /See:/ 'newDefaultPushNotificationMessage' smart constructor.
data DefaultPushNotificationMessage = DefaultPushNotificationMessage'
  { -- | Specifies whether the default notification is a silent push
    -- notification, which is a push notification that doesn\'t display on a
    -- recipient\'s device. Silent push notifications can be used for cases
    -- such as updating an app\'s configuration or delivering messages to an
    -- in-app notification center.
    silentPush :: Prelude.Maybe Prelude.Bool,
    -- | The JSON data payload to use for the default push notification, if the
    -- notification is a silent push notification. This payload is added to the
    -- data.pinpoint.jsonBody object of the notification.
    data' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The default title to display above the notification message on a
    -- recipient\'s device.
    title :: Prelude.Maybe Prelude.Text,
    -- | The default body of the notification message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The default message variables to use in the notification message. You
    -- can override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
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
    -- | The default URL to open in a recipient\'s default mobile browser, if a
    -- recipient taps the push notification and the value of the Action
    -- property is URL.
    url :: Prelude.Maybe Prelude.Text
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
-- 'silentPush', 'defaultPushNotificationMessage_silentPush' - Specifies whether the default notification is a silent push
-- notification, which is a push notification that doesn\'t display on a
-- recipient\'s device. Silent push notifications can be used for cases
-- such as updating an app\'s configuration or delivering messages to an
-- in-app notification center.
--
-- 'data'', 'defaultPushNotificationMessage_data' - The JSON data payload to use for the default push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
--
-- 'title', 'defaultPushNotificationMessage_title' - The default title to display above the notification message on a
-- recipient\'s device.
--
-- 'body', 'defaultPushNotificationMessage_body' - The default body of the notification message.
--
-- 'substitutions', 'defaultPushNotificationMessage_substitutions' - The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
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
-- 'url', 'defaultPushNotificationMessage_url' - The default URL to open in a recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
newDefaultPushNotificationMessage ::
  DefaultPushNotificationMessage
newDefaultPushNotificationMessage =
  DefaultPushNotificationMessage'
    { silentPush =
        Prelude.Nothing,
      data' = Prelude.Nothing,
      title = Prelude.Nothing,
      body = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      action = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | Specifies whether the default notification is a silent push
-- notification, which is a push notification that doesn\'t display on a
-- recipient\'s device. Silent push notifications can be used for cases
-- such as updating an app\'s configuration or delivering messages to an
-- in-app notification center.
defaultPushNotificationMessage_silentPush :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Bool)
defaultPushNotificationMessage_silentPush = Lens.lens (\DefaultPushNotificationMessage' {silentPush} -> silentPush) (\s@DefaultPushNotificationMessage' {} a -> s {silentPush = a} :: DefaultPushNotificationMessage)

-- | The JSON data payload to use for the default push notification, if the
-- notification is a silent push notification. This payload is added to the
-- data.pinpoint.jsonBody object of the notification.
defaultPushNotificationMessage_data :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
defaultPushNotificationMessage_data = Lens.lens (\DefaultPushNotificationMessage' {data'} -> data') (\s@DefaultPushNotificationMessage' {} a -> s {data' = a} :: DefaultPushNotificationMessage) Prelude.. Lens.mapping Lens._Coerce

-- | The default title to display above the notification message on a
-- recipient\'s device.
defaultPushNotificationMessage_title :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Text)
defaultPushNotificationMessage_title = Lens.lens (\DefaultPushNotificationMessage' {title} -> title) (\s@DefaultPushNotificationMessage' {} a -> s {title = a} :: DefaultPushNotificationMessage)

-- | The default body of the notification message.
defaultPushNotificationMessage_body :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Text)
defaultPushNotificationMessage_body = Lens.lens (\DefaultPushNotificationMessage' {body} -> body) (\s@DefaultPushNotificationMessage' {} a -> s {body = a} :: DefaultPushNotificationMessage)

-- | The default message variables to use in the notification message. You
-- can override the default variables with individual address variables.
defaultPushNotificationMessage_substitutions :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
defaultPushNotificationMessage_substitutions = Lens.lens (\DefaultPushNotificationMessage' {substitutions} -> substitutions) (\s@DefaultPushNotificationMessage' {} a -> s {substitutions = a} :: DefaultPushNotificationMessage) Prelude.. Lens.mapping Lens._Coerce

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

-- | The default URL to open in a recipient\'s default mobile browser, if a
-- recipient taps the push notification and the value of the Action
-- property is URL.
defaultPushNotificationMessage_url :: Lens.Lens' DefaultPushNotificationMessage (Prelude.Maybe Prelude.Text)
defaultPushNotificationMessage_url = Lens.lens (\DefaultPushNotificationMessage' {url} -> url) (\s@DefaultPushNotificationMessage' {} a -> s {url = a} :: DefaultPushNotificationMessage)

instance
  Prelude.Hashable
    DefaultPushNotificationMessage

instance
  Prelude.NFData
    DefaultPushNotificationMessage

instance Core.ToJSON DefaultPushNotificationMessage where
  toJSON DefaultPushNotificationMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SilentPush" Core..=) Prelude.<$> silentPush,
            ("Data" Core..=) Prelude.<$> data',
            ("Title" Core..=) Prelude.<$> title,
            ("Body" Core..=) Prelude.<$> body,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("Action" Core..=) Prelude.<$> action,
            ("Url" Core..=) Prelude.<$> url
          ]
      )
