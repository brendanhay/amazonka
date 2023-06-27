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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.NotificationChannelConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.NotificationFilterConfig
import Amazonka.DevOpsGuru.Types.SnsChannelConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about notification channels you have configured with DevOps
-- Guru. The one supported notification channel is Amazon Simple
-- Notification Service (Amazon SNS).
--
-- /See:/ 'newNotificationChannelConfig' smart constructor.
data NotificationChannelConfig = NotificationChannelConfig'
  { -- | The filter configurations for the Amazon SNS notification topic you use
    -- with DevOps Guru. If you do not provide filter configurations, the
    -- default configurations are to receive notifications for all message
    -- types of @High@ or @Medium@ severity.
    filters :: Prelude.Maybe NotificationFilterConfig,
    -- | Information about a notification channel configured in DevOps Guru to
    -- send notifications when insights are created.
    --
    -- If you use an Amazon SNS topic in another account, you must attach a
    -- policy to it that grants DevOps Guru permission to send it
    -- notifications. DevOps Guru adds the required policy on your behalf to
    -- send notifications using Amazon SNS in your account. DevOps Guru only
    -- supports standard SNS topics. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-required-permissions.html Permissions for Amazon SNS topics>.
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
-- 'filters', 'notificationChannelConfig_filters' - The filter configurations for the Amazon SNS notification topic you use
-- with DevOps Guru. If you do not provide filter configurations, the
-- default configurations are to receive notifications for all message
-- types of @High@ or @Medium@ severity.
--
-- 'sns', 'notificationChannelConfig_sns' - Information about a notification channel configured in DevOps Guru to
-- send notifications when insights are created.
--
-- If you use an Amazon SNS topic in another account, you must attach a
-- policy to it that grants DevOps Guru permission to send it
-- notifications. DevOps Guru adds the required policy on your behalf to
-- send notifications using Amazon SNS in your account. DevOps Guru only
-- supports standard SNS topics. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-required-permissions.html Permissions for Amazon SNS topics>.
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
  NotificationChannelConfig'
    { filters =
        Prelude.Nothing,
      sns = pSns_
    }

-- | The filter configurations for the Amazon SNS notification topic you use
-- with DevOps Guru. If you do not provide filter configurations, the
-- default configurations are to receive notifications for all message
-- types of @High@ or @Medium@ severity.
notificationChannelConfig_filters :: Lens.Lens' NotificationChannelConfig (Prelude.Maybe NotificationFilterConfig)
notificationChannelConfig_filters = Lens.lens (\NotificationChannelConfig' {filters} -> filters) (\s@NotificationChannelConfig' {} a -> s {filters = a} :: NotificationChannelConfig)

-- | Information about a notification channel configured in DevOps Guru to
-- send notifications when insights are created.
--
-- If you use an Amazon SNS topic in another account, you must attach a
-- policy to it that grants DevOps Guru permission to send it
-- notifications. DevOps Guru adds the required policy on your behalf to
-- send notifications using Amazon SNS in your account. DevOps Guru only
-- supports standard SNS topics. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-required-permissions.html Permissions for Amazon SNS topics>.
--
-- If you use an Amazon SNS topic that is encrypted by an Amazon Web
-- Services Key Management Service customer-managed key (CMK), then you
-- must add permissions to the CMK. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-kms-permissions.html Permissions for Amazon Web Services KMS–encrypted Amazon SNS topics>.
notificationChannelConfig_sns :: Lens.Lens' NotificationChannelConfig SnsChannelConfig
notificationChannelConfig_sns = Lens.lens (\NotificationChannelConfig' {sns} -> sns) (\s@NotificationChannelConfig' {} a -> s {sns = a} :: NotificationChannelConfig)

instance Data.FromJSON NotificationChannelConfig where
  parseJSON =
    Data.withObject
      "NotificationChannelConfig"
      ( \x ->
          NotificationChannelConfig'
            Prelude.<$> (x Data..:? "Filters")
            Prelude.<*> (x Data..: "Sns")
      )

instance Prelude.Hashable NotificationChannelConfig where
  hashWithSalt _salt NotificationChannelConfig' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sns

instance Prelude.NFData NotificationChannelConfig where
  rnf NotificationChannelConfig' {..} =
    Prelude.rnf filters `Prelude.seq` Prelude.rnf sns

instance Data.ToJSON NotificationChannelConfig where
  toJSON NotificationChannelConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            Prelude.Just ("Sns" Data..= sns)
          ]
      )
