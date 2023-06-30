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
-- Module      : Amazonka.CloudFront.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.KinesisStreamConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
--
-- /See:/ 'newKinesisStreamConfig' smart constructor.
data KinesisStreamConfig = KinesisStreamConfig'
  { -- | The Amazon Resource Name (ARN) of an Identity and Access Management
    -- (IAM) role that CloudFront can use to send real-time log data to your
    -- Kinesis data stream.
    --
    -- For more information the IAM role, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role>
    -- in the /Amazon CloudFront Developer Guide/.
    roleARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Kinesis data stream where you are
    -- sending real-time log data.
    streamARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'kinesisStreamConfig_roleARN' - The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that CloudFront can use to send real-time log data to your
-- Kinesis data stream.
--
-- For more information the IAM role, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'streamARN', 'kinesisStreamConfig_streamARN' - The Amazon Resource Name (ARN) of the Kinesis data stream where you are
-- sending real-time log data.
newKinesisStreamConfig ::
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'streamARN'
  Prelude.Text ->
  KinesisStreamConfig
newKinesisStreamConfig pRoleARN_ pStreamARN_ =
  KinesisStreamConfig'
    { roleARN = pRoleARN_,
      streamARN = pStreamARN_
    }

-- | The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that CloudFront can use to send real-time log data to your
-- Kinesis data stream.
--
-- For more information the IAM role, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role>
-- in the /Amazon CloudFront Developer Guide/.
kinesisStreamConfig_roleARN :: Lens.Lens' KinesisStreamConfig Prelude.Text
kinesisStreamConfig_roleARN = Lens.lens (\KinesisStreamConfig' {roleARN} -> roleARN) (\s@KinesisStreamConfig' {} a -> s {roleARN = a} :: KinesisStreamConfig)

-- | The Amazon Resource Name (ARN) of the Kinesis data stream where you are
-- sending real-time log data.
kinesisStreamConfig_streamARN :: Lens.Lens' KinesisStreamConfig Prelude.Text
kinesisStreamConfig_streamARN = Lens.lens (\KinesisStreamConfig' {streamARN} -> streamARN) (\s@KinesisStreamConfig' {} a -> s {streamARN = a} :: KinesisStreamConfig)

instance Data.FromXML KinesisStreamConfig where
  parseXML x =
    KinesisStreamConfig'
      Prelude.<$> (x Data..@ "RoleARN")
      Prelude.<*> (x Data..@ "StreamARN")

instance Prelude.Hashable KinesisStreamConfig where
  hashWithSalt _salt KinesisStreamConfig' {..} =
    _salt
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` streamARN

instance Prelude.NFData KinesisStreamConfig where
  rnf KinesisStreamConfig' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf streamARN

instance Data.ToXML KinesisStreamConfig where
  toXML KinesisStreamConfig' {..} =
    Prelude.mconcat
      [ "RoleARN" Data.@= roleARN,
        "StreamARN" Data.@= streamARN
      ]
