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
-- Module      : Amazonka.DevOpsGuru.Types.NotificationChannelConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.NotificationChannelConfig where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.SnsChannelConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about notification channels you have configured with DevOps
-- Guru. The one supported notification channel is Amazon Simple
-- Notification Service (Amazon SNS).
--
-- /See:/ 'newNotificationChannelConfig' smart constructor.
data NotificationChannelConfig = NotificationChannelConfig'
  { -- | Information about a notification channel configured in DevOps Guru to
    -- send notifications when insights are created.
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
    sns :: SnsChannelConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationChannelConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sns', 'notificationChannelConfig_sns' - Information about a notification channel configured in DevOps Guru to
-- send notifications when insights are created.
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
newNotificationChannelConfig ::
  -- | 'sns'
  SnsChannelConfig ->
  NotificationChannelConfig
newNotificationChannelConfig pSns_ =
  NotificationChannelConfig' {sns = pSns_}

-- | Information about a notification channel configured in DevOps Guru to
-- send notifications when insights are created.
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
notificationChannelConfig_sns :: Lens.Lens' NotificationChannelConfig SnsChannelConfig
notificationChannelConfig_sns = Lens.lens (\NotificationChannelConfig' {sns} -> sns) (\s@NotificationChannelConfig' {} a -> s {sns = a} :: NotificationChannelConfig)

instance Core.FromJSON NotificationChannelConfig where
  parseJSON =
    Core.withObject
      "NotificationChannelConfig"
      ( \x ->
          NotificationChannelConfig'
            Prelude.<$> (x Core..: "Sns")
      )

instance Prelude.Hashable NotificationChannelConfig where
  hashWithSalt _salt NotificationChannelConfig' {..} =
    _salt `Prelude.hashWithSalt` sns

instance Prelude.NFData NotificationChannelConfig where
  rnf NotificationChannelConfig' {..} = Prelude.rnf sns

instance Core.ToJSON NotificationChannelConfig where
  toJSON NotificationChannelConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Sns" Core..= sns)]
      )
