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
-- Module      : Network.AWS.ECS.Types.ExecuteCommandLogConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ExecuteCommandLogConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The log configuration for the results of the execute command actions.
-- The logs can be sent to CloudWatch Logs or an Amazon S3 bucket.
--
-- /See:/ 'newExecuteCommandLogConfiguration' smart constructor.
data ExecuteCommandLogConfiguration = ExecuteCommandLogConfiguration'
  { -- | The name of the CloudWatch log group to send logs to.
    --
    -- The CloudWatch log group must already be created.
    cloudWatchLogGroupName :: Prelude.Maybe Prelude.Text,
    -- | Whether or not to enable encryption on the CloudWatch logs. If not
    -- specified, encryption will be disabled.
    cloudWatchEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether or not to enable encryption on the CloudWatch logs. If not
    -- specified, encryption will be disabled.
    s3EncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An optional folder in the S3 bucket to place logs in.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket to send logs to.
    --
    -- The S3 bucket must already be created.
    s3BucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteCommandLogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupName', 'executeCommandLogConfiguration_cloudWatchLogGroupName' - The name of the CloudWatch log group to send logs to.
--
-- The CloudWatch log group must already be created.
--
-- 'cloudWatchEncryptionEnabled', 'executeCommandLogConfiguration_cloudWatchEncryptionEnabled' - Whether or not to enable encryption on the CloudWatch logs. If not
-- specified, encryption will be disabled.
--
-- 's3EncryptionEnabled', 'executeCommandLogConfiguration_s3EncryptionEnabled' - Whether or not to enable encryption on the CloudWatch logs. If not
-- specified, encryption will be disabled.
--
-- 's3KeyPrefix', 'executeCommandLogConfiguration_s3KeyPrefix' - An optional folder in the S3 bucket to place logs in.
--
-- 's3BucketName', 'executeCommandLogConfiguration_s3BucketName' - The name of the S3 bucket to send logs to.
--
-- The S3 bucket must already be created.
newExecuteCommandLogConfiguration ::
  ExecuteCommandLogConfiguration
newExecuteCommandLogConfiguration =
  ExecuteCommandLogConfiguration'
    { cloudWatchLogGroupName =
        Prelude.Nothing,
      cloudWatchEncryptionEnabled =
        Prelude.Nothing,
      s3EncryptionEnabled = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      s3BucketName = Prelude.Nothing
    }

-- | The name of the CloudWatch log group to send logs to.
--
-- The CloudWatch log group must already be created.
executeCommandLogConfiguration_cloudWatchLogGroupName :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Text)
executeCommandLogConfiguration_cloudWatchLogGroupName = Lens.lens (\ExecuteCommandLogConfiguration' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@ExecuteCommandLogConfiguration' {} a -> s {cloudWatchLogGroupName = a} :: ExecuteCommandLogConfiguration)

-- | Whether or not to enable encryption on the CloudWatch logs. If not
-- specified, encryption will be disabled.
executeCommandLogConfiguration_cloudWatchEncryptionEnabled :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Bool)
executeCommandLogConfiguration_cloudWatchEncryptionEnabled = Lens.lens (\ExecuteCommandLogConfiguration' {cloudWatchEncryptionEnabled} -> cloudWatchEncryptionEnabled) (\s@ExecuteCommandLogConfiguration' {} a -> s {cloudWatchEncryptionEnabled = a} :: ExecuteCommandLogConfiguration)

-- | Whether or not to enable encryption on the CloudWatch logs. If not
-- specified, encryption will be disabled.
executeCommandLogConfiguration_s3EncryptionEnabled :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Bool)
executeCommandLogConfiguration_s3EncryptionEnabled = Lens.lens (\ExecuteCommandLogConfiguration' {s3EncryptionEnabled} -> s3EncryptionEnabled) (\s@ExecuteCommandLogConfiguration' {} a -> s {s3EncryptionEnabled = a} :: ExecuteCommandLogConfiguration)

-- | An optional folder in the S3 bucket to place logs in.
executeCommandLogConfiguration_s3KeyPrefix :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Text)
executeCommandLogConfiguration_s3KeyPrefix = Lens.lens (\ExecuteCommandLogConfiguration' {s3KeyPrefix} -> s3KeyPrefix) (\s@ExecuteCommandLogConfiguration' {} a -> s {s3KeyPrefix = a} :: ExecuteCommandLogConfiguration)

-- | The name of the S3 bucket to send logs to.
--
-- The S3 bucket must already be created.
executeCommandLogConfiguration_s3BucketName :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Text)
executeCommandLogConfiguration_s3BucketName = Lens.lens (\ExecuteCommandLogConfiguration' {s3BucketName} -> s3BucketName) (\s@ExecuteCommandLogConfiguration' {} a -> s {s3BucketName = a} :: ExecuteCommandLogConfiguration)

instance Core.FromJSON ExecuteCommandLogConfiguration where
  parseJSON =
    Core.withObject
      "ExecuteCommandLogConfiguration"
      ( \x ->
          ExecuteCommandLogConfiguration'
            Prelude.<$> (x Core..:? "cloudWatchLogGroupName")
            Prelude.<*> (x Core..:? "cloudWatchEncryptionEnabled")
            Prelude.<*> (x Core..:? "s3EncryptionEnabled")
            Prelude.<*> (x Core..:? "s3KeyPrefix")
            Prelude.<*> (x Core..:? "s3BucketName")
      )

instance
  Prelude.Hashable
    ExecuteCommandLogConfiguration

instance
  Prelude.NFData
    ExecuteCommandLogConfiguration

instance Core.ToJSON ExecuteCommandLogConfiguration where
  toJSON ExecuteCommandLogConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogGroupName" Core..=)
              Prelude.<$> cloudWatchLogGroupName,
            ("cloudWatchEncryptionEnabled" Core..=)
              Prelude.<$> cloudWatchEncryptionEnabled,
            ("s3EncryptionEnabled" Core..=)
              Prelude.<$> s3EncryptionEnabled,
            ("s3KeyPrefix" Core..=) Prelude.<$> s3KeyPrefix,
            ("s3BucketName" Core..=) Prelude.<$> s3BucketName
          ]
      )
