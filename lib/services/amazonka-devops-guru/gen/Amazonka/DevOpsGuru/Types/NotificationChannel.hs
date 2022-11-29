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
-- Module      : Amazonka.DevOpsGuru.Types.NotificationChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.NotificationChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.NotificationChannelConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about a notification channel. A notification channel is used
-- to notify you when DevOps Guru creates an insight. The one supported
-- notification channel is Amazon Simple Notification Service (Amazon SNS).
--
-- If you use an Amazon SNS topic in another account, you must attach a
-- policy to it that grants DevOps Guru permission to it notifications.
-- DevOps Guru adds the required policy on your behalf to send
-- notifications using Amazon SNS in your account. DevOps Guru only
-- supports standard SNS topics. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-required-permissions.html Permissions for cross account Amazon SNS topics>.
--
-- If you use an Amazon SNS topic in another account, you must attach a
-- policy to it that grants DevOps Guru permission to it notifications.
-- DevOps Guru adds the required policy on your behalf to send
-- notifications using Amazon SNS in your account. For more information,
-- see Permissions for cross account Amazon SNS topics.
--
-- If you use an Amazon SNS topic that is encrypted by an Amazon Web
-- Services Key Management Service customer-managed key (CMK), then you
-- must add permissions to the CMK. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-kms-permissions.html Permissions for Amazon Web Services KMS–encrypted Amazon SNS topics>.
--
-- /See:/ 'newNotificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { -- | The ID of a notification channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | A @NotificationChannelConfig@ object that contains information about
    -- configured notification channels.
    config :: Prelude.Maybe NotificationChannelConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'notificationChannel_id' - The ID of a notification channel.
--
-- 'config', 'notificationChannel_config' - A @NotificationChannelConfig@ object that contains information about
-- configured notification channels.
newNotificationChannel ::
  NotificationChannel
newNotificationChannel =
  NotificationChannel'
    { id = Prelude.Nothing,
      config = Prelude.Nothing
    }

-- | The ID of a notification channel.
notificationChannel_id :: Lens.Lens' NotificationChannel (Prelude.Maybe Prelude.Text)
notificationChannel_id = Lens.lens (\NotificationChannel' {id} -> id) (\s@NotificationChannel' {} a -> s {id = a} :: NotificationChannel)

-- | A @NotificationChannelConfig@ object that contains information about
-- configured notification channels.
notificationChannel_config :: Lens.Lens' NotificationChannel (Prelude.Maybe NotificationChannelConfig)
notificationChannel_config = Lens.lens (\NotificationChannel' {config} -> config) (\s@NotificationChannel' {} a -> s {config = a} :: NotificationChannel)

instance Core.FromJSON NotificationChannel where
  parseJSON =
    Core.withObject
      "NotificationChannel"
      ( \x ->
          NotificationChannel'
            Prelude.<$> (x Core..:? "Id") Prelude.<*> (x Core..:? "Config")
      )

instance Prelude.Hashable NotificationChannel where
  hashWithSalt _salt NotificationChannel' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` config

instance Prelude.NFData NotificationChannel where
  rnf NotificationChannel' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf config
