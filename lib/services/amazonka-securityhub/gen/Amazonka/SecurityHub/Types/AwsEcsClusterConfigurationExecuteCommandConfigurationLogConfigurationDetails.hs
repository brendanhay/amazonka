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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The log configuration for the results of the run command actions.
--
-- /See:/ 'newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' smart constructor.
data AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails = AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails'
  { -- | Identifies the folder in the S3 bucket to send the logs to.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket to send logs to.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Whether to encrypt the logs that are sent to the S3 bucket.
    s3EncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether to enable encryption on the CloudWatch logs.
    cloudWatchEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CloudWatch log group to send the logs to.
    cloudWatchLogGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix' - Identifies the folder in the S3 bucket to send the logs to.
--
-- 's3BucketName', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName' - The name of the S3 bucket to send logs to.
--
-- 's3EncryptionEnabled', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled' - Whether to encrypt the logs that are sent to the S3 bucket.
--
-- 'cloudWatchEncryptionEnabled', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled' - Whether to enable encryption on the CloudWatch logs.
--
-- 'cloudWatchLogGroupName', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName' - The name of the CloudWatch log group to send the logs to.
newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails ::
  AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails =
  AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails'
    { s3KeyPrefix =
        Prelude.Nothing,
      s3BucketName =
        Prelude.Nothing,
      s3EncryptionEnabled =
        Prelude.Nothing,
      cloudWatchEncryptionEnabled =
        Prelude.Nothing,
      cloudWatchLogGroupName =
        Prelude.Nothing
    }

-- | Identifies the folder in the S3 bucket to send the logs to.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {s3KeyPrefix} -> s3KeyPrefix) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {s3KeyPrefix = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | The name of the S3 bucket to send logs to.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {s3BucketName} -> s3BucketName) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {s3BucketName = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | Whether to encrypt the logs that are sent to the S3 bucket.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {s3EncryptionEnabled} -> s3EncryptionEnabled) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {s3EncryptionEnabled = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | Whether to enable encryption on the CloudWatch logs.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {cloudWatchEncryptionEnabled} -> cloudWatchEncryptionEnabled) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {cloudWatchEncryptionEnabled = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | The name of the CloudWatch log group to send the logs to.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {cloudWatchLogGroupName = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

instance
  Core.FromJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails"
      ( \x ->
          AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails'
            Prelude.<$> (x Core..:? "S3KeyPrefix")
              Prelude.<*> (x Core..:? "S3BucketName")
              Prelude.<*> (x Core..:? "S3EncryptionEnabled")
              Prelude.<*> (x Core..:? "CloudWatchEncryptionEnabled")
              Prelude.<*> (x Core..:? "CloudWatchLogGroupName")
      )

instance
  Prelude.Hashable
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` s3KeyPrefix
        `Prelude.hashWithSalt` s3BucketName
        `Prelude.hashWithSalt` s3EncryptionEnabled
        `Prelude.hashWithSalt` cloudWatchEncryptionEnabled
        `Prelude.hashWithSalt` cloudWatchLogGroupName

instance
  Prelude.NFData
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  rnf
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {..} =
      Prelude.rnf s3KeyPrefix
        `Prelude.seq` Prelude.rnf s3BucketName
        `Prelude.seq` Prelude.rnf s3EncryptionEnabled
        `Prelude.seq` Prelude.rnf cloudWatchEncryptionEnabled
        `Prelude.seq` Prelude.rnf cloudWatchLogGroupName

instance
  Core.ToJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  toJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("S3KeyPrefix" Core..=) Prelude.<$> s3KeyPrefix,
              ("S3BucketName" Core..=) Prelude.<$> s3BucketName,
              ("S3EncryptionEnabled" Core..=)
                Prelude.<$> s3EncryptionEnabled,
              ("CloudWatchEncryptionEnabled" Core..=)
                Prelude.<$> cloudWatchEncryptionEnabled,
              ("CloudWatchLogGroupName" Core..=)
                Prelude.<$> cloudWatchLogGroupName
            ]
        )
