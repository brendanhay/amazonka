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
-- Module      : Amazonka.DevOpsGuru.Types.SnsChannelConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.SnsChannelConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the Amazon Resource Name (ARN) of an Amazon Simple Notification
-- Service topic.
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
--
-- /See:/ 'newSnsChannelConfig' smart constructor.
data SnsChannelConfig = SnsChannelConfig'
  { -- | The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
    -- topic.
    topicArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnsChannelConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'snsChannelConfig_topicArn' - The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
-- topic.
newSnsChannelConfig ::
  SnsChannelConfig
newSnsChannelConfig =
  SnsChannelConfig' {topicArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
-- topic.
snsChannelConfig_topicArn :: Lens.Lens' SnsChannelConfig (Prelude.Maybe Prelude.Text)
snsChannelConfig_topicArn = Lens.lens (\SnsChannelConfig' {topicArn} -> topicArn) (\s@SnsChannelConfig' {} a -> s {topicArn = a} :: SnsChannelConfig)

instance Data.FromJSON SnsChannelConfig where
  parseJSON =
    Data.withObject
      "SnsChannelConfig"
      ( \x ->
          SnsChannelConfig'
            Prelude.<$> (x Data..:? "TopicArn")
      )

instance Prelude.Hashable SnsChannelConfig where
  hashWithSalt _salt SnsChannelConfig' {..} =
    _salt `Prelude.hashWithSalt` topicArn

instance Prelude.NFData SnsChannelConfig where
  rnf SnsChannelConfig' {..} = Prelude.rnf topicArn

instance Data.ToJSON SnsChannelConfig where
  toJSON SnsChannelConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TopicArn" Data..=) Prelude.<$> topicArn]
      )
