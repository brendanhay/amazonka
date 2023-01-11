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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The log configuration for the results of the run command actions.
--
-- /See:/ 'newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' smart constructor.
data AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails = AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails'
  { -- | Whether to enable encryption on the CloudWatch logs.
    cloudWatchEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CloudWatch log group to send the logs to.
    cloudWatchLogGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket to send logs to.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Whether to encrypt the logs that are sent to the S3 bucket.
    s3EncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Identifies the folder in the S3 bucket to send the logs to.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text
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
-- 'cloudWatchEncryptionEnabled', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled' - Whether to enable encryption on the CloudWatch logs.
--
-- 'cloudWatchLogGroupName', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName' - The name of the CloudWatch log group to send the logs to.
--
-- 's3BucketName', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName' - The name of the S3 bucket to send logs to.
--
-- 's3EncryptionEnabled', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled' - Whether to encrypt the logs that are sent to the S3 bucket.
--
-- 's3KeyPrefix', 'awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix' - Identifies the folder in the S3 bucket to send the logs to.
newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails ::
  AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
newAwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails =
  AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails'
    { cloudWatchEncryptionEnabled =
        Prelude.Nothing,
      cloudWatchLogGroupName =
        Prelude.Nothing,
      s3BucketName =
        Prelude.Nothing,
      s3EncryptionEnabled =
        Prelude.Nothing,
      s3KeyPrefix =
        Prelude.Nothing
    }

-- | Whether to enable encryption on the CloudWatch logs.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchEncryptionEnabled = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {cloudWatchEncryptionEnabled} -> cloudWatchEncryptionEnabled) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {cloudWatchEncryptionEnabled = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | The name of the CloudWatch log group to send the logs to.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_cloudWatchLogGroupName = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {cloudWatchLogGroupName = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | The name of the S3 bucket to send logs to.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3BucketName = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {s3BucketName} -> s3BucketName) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {s3BucketName = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | Whether to encrypt the logs that are sent to the S3 bucket.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3EncryptionEnabled = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {s3EncryptionEnabled} -> s3EncryptionEnabled) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {s3EncryptionEnabled = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

-- | Identifies the folder in the S3 bucket to send the logs to.
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails_s3KeyPrefix = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {s3KeyPrefix} -> s3KeyPrefix) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {} a -> s {s3KeyPrefix = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)

instance
  Data.FromJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails"
      ( \x ->
          AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails'
            Prelude.<$> (x Data..:? "CloudWatchEncryptionEnabled")
              Prelude.<*> (x Data..:? "CloudWatchLogGroupName")
              Prelude.<*> (x Data..:? "S3BucketName")
              Prelude.<*> (x Data..:? "S3EncryptionEnabled")
              Prelude.<*> (x Data..:? "S3KeyPrefix")
      )

instance
  Prelude.Hashable
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchEncryptionEnabled
        `Prelude.hashWithSalt` cloudWatchLogGroupName
        `Prelude.hashWithSalt` s3BucketName
        `Prelude.hashWithSalt` s3EncryptionEnabled
        `Prelude.hashWithSalt` s3KeyPrefix

instance
  Prelude.NFData
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  rnf
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {..} =
      Prelude.rnf cloudWatchEncryptionEnabled
        `Prelude.seq` Prelude.rnf cloudWatchLogGroupName
        `Prelude.seq` Prelude.rnf s3BucketName
        `Prelude.seq` Prelude.rnf s3EncryptionEnabled
        `Prelude.seq` Prelude.rnf s3KeyPrefix

instance
  Data.ToJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails
  where
  toJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CloudWatchEncryptionEnabled" Data..=)
                Prelude.<$> cloudWatchEncryptionEnabled,
              ("CloudWatchLogGroupName" Data..=)
                Prelude.<$> cloudWatchLogGroupName,
              ("S3BucketName" Data..=) Prelude.<$> s3BucketName,
              ("S3EncryptionEnabled" Data..=)
                Prelude.<$> s3EncryptionEnabled,
              ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix
            ]
        )
