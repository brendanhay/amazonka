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
-- Module      : Network.AWS.CloudFront.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KinesisStreamConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
--
-- /See:/ 'newKinesisStreamConfig' smart constructor.
data KinesisStreamConfig = KinesisStreamConfig'
  { -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
    -- (IAM) role that CloudFront can use to send real-time log data to your
    -- Kinesis data stream.
    --
    -- For more information the IAM role, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role>
    -- in the /Amazon CloudFront Developer Guide/.
    roleARN :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the Kinesis data stream where you are
    -- sending real-time log data.
    streamARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisStreamConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'kinesisStreamConfig_roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management
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
  Core.Text ->
  -- | 'streamARN'
  Core.Text ->
  KinesisStreamConfig
newKinesisStreamConfig pRoleARN_ pStreamARN_ =
  KinesisStreamConfig'
    { roleARN = pRoleARN_,
      streamARN = pStreamARN_
    }

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that CloudFront can use to send real-time log data to your
-- Kinesis data stream.
--
-- For more information the IAM role, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role>
-- in the /Amazon CloudFront Developer Guide/.
kinesisStreamConfig_roleARN :: Lens.Lens' KinesisStreamConfig Core.Text
kinesisStreamConfig_roleARN = Lens.lens (\KinesisStreamConfig' {roleARN} -> roleARN) (\s@KinesisStreamConfig' {} a -> s {roleARN = a} :: KinesisStreamConfig)

-- | The Amazon Resource Name (ARN) of the Kinesis data stream where you are
-- sending real-time log data.
kinesisStreamConfig_streamARN :: Lens.Lens' KinesisStreamConfig Core.Text
kinesisStreamConfig_streamARN = Lens.lens (\KinesisStreamConfig' {streamARN} -> streamARN) (\s@KinesisStreamConfig' {} a -> s {streamARN = a} :: KinesisStreamConfig)

instance Core.FromXML KinesisStreamConfig where
  parseXML x =
    KinesisStreamConfig'
      Core.<$> (x Core..@ "RoleARN")
      Core.<*> (x Core..@ "StreamARN")

instance Core.Hashable KinesisStreamConfig

instance Core.NFData KinesisStreamConfig

instance Core.ToXML KinesisStreamConfig where
  toXML KinesisStreamConfig' {..} =
    Core.mconcat
      [ "RoleARN" Core.@= roleARN,
        "StreamARN" Core.@= streamARN
      ]
