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
-- Module      : Amazonka.ChimeSDKMessaging.Types.PushNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.PushNotificationConfiguration where

import Amazonka.ChimeSDKMessaging.Types.PushNotificationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The push notification configuration of the message.
--
-- /See:/ 'newPushNotificationConfiguration' smart constructor.
data PushNotificationConfiguration = PushNotificationConfiguration'
  { -- | The body of the push notification.
    body :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The title of the push notification.
    title :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Enum value that indicates the type of the push notification for a
    -- message. @DEFAULT@: Normal mobile push notification. @VOIP@: VOIP mobile
    -- push notification.
    type' :: Prelude.Maybe PushNotificationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'pushNotificationConfiguration_body' - The body of the push notification.
--
-- 'title', 'pushNotificationConfiguration_title' - The title of the push notification.
--
-- 'type'', 'pushNotificationConfiguration_type' - Enum value that indicates the type of the push notification for a
-- message. @DEFAULT@: Normal mobile push notification. @VOIP@: VOIP mobile
-- push notification.
newPushNotificationConfiguration ::
  PushNotificationConfiguration
newPushNotificationConfiguration =
  PushNotificationConfiguration'
    { body =
        Prelude.Nothing,
      title = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The body of the push notification.
pushNotificationConfiguration_body :: Lens.Lens' PushNotificationConfiguration (Prelude.Maybe Prelude.Text)
pushNotificationConfiguration_body = Lens.lens (\PushNotificationConfiguration' {body} -> body) (\s@PushNotificationConfiguration' {} a -> s {body = a} :: PushNotificationConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The title of the push notification.
pushNotificationConfiguration_title :: Lens.Lens' PushNotificationConfiguration (Prelude.Maybe Prelude.Text)
pushNotificationConfiguration_title = Lens.lens (\PushNotificationConfiguration' {title} -> title) (\s@PushNotificationConfiguration' {} a -> s {title = a} :: PushNotificationConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | Enum value that indicates the type of the push notification for a
-- message. @DEFAULT@: Normal mobile push notification. @VOIP@: VOIP mobile
-- push notification.
pushNotificationConfiguration_type :: Lens.Lens' PushNotificationConfiguration (Prelude.Maybe PushNotificationType)
pushNotificationConfiguration_type = Lens.lens (\PushNotificationConfiguration' {type'} -> type') (\s@PushNotificationConfiguration' {} a -> s {type' = a} :: PushNotificationConfiguration)

instance
  Prelude.Hashable
    PushNotificationConfiguration
  where
  hashWithSalt _salt PushNotificationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PushNotificationConfiguration where
  rnf PushNotificationConfiguration' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON PushNotificationConfiguration where
  toJSON PushNotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("Title" Data..=) Prelude.<$> title,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
