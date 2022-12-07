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
-- Module      : Amazonka.ECS.Types.ExecuteCommandLogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ExecuteCommandLogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The log configuration for the results of the execute command actions.
-- The logs can be sent to CloudWatch Logs or an Amazon S3 bucket.
--
-- /See:/ 'newExecuteCommandLogConfiguration' smart constructor.
data ExecuteCommandLogConfiguration = ExecuteCommandLogConfiguration'
  { -- | An optional folder in the S3 bucket to place logs in.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket to send logs to.
    --
    -- The S3 bucket must already be created.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Determines whether to use encryption on the S3 logs. If not specified,
    -- encryption is not used.
    s3EncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether to use encryption on the CloudWatch logs. If not
    -- specified, encryption will be disabled.
    cloudWatchEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CloudWatch log group to send logs to.
    --
    -- The CloudWatch log group must already be created.
    cloudWatchLogGroupName :: Prelude.Maybe Prelude.Text
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
-- 's3KeyPrefix', 'executeCommandLogConfiguration_s3KeyPrefix' - An optional folder in the S3 bucket to place logs in.
--
-- 's3BucketName', 'executeCommandLogConfiguration_s3BucketName' - The name of the S3 bucket to send logs to.
--
-- The S3 bucket must already be created.
--
-- 's3EncryptionEnabled', 'executeCommandLogConfiguration_s3EncryptionEnabled' - Determines whether to use encryption on the S3 logs. If not specified,
-- encryption is not used.
--
-- 'cloudWatchEncryptionEnabled', 'executeCommandLogConfiguration_cloudWatchEncryptionEnabled' - Determines whether to use encryption on the CloudWatch logs. If not
-- specified, encryption will be disabled.
--
-- 'cloudWatchLogGroupName', 'executeCommandLogConfiguration_cloudWatchLogGroupName' - The name of the CloudWatch log group to send logs to.
--
-- The CloudWatch log group must already be created.
newExecuteCommandLogConfiguration ::
  ExecuteCommandLogConfiguration
newExecuteCommandLogConfiguration =
  ExecuteCommandLogConfiguration'
    { s3KeyPrefix =
        Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      s3EncryptionEnabled = Prelude.Nothing,
      cloudWatchEncryptionEnabled =
        Prelude.Nothing,
      cloudWatchLogGroupName = Prelude.Nothing
    }

-- | An optional folder in the S3 bucket to place logs in.
executeCommandLogConfiguration_s3KeyPrefix :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Text)
executeCommandLogConfiguration_s3KeyPrefix = Lens.lens (\ExecuteCommandLogConfiguration' {s3KeyPrefix} -> s3KeyPrefix) (\s@ExecuteCommandLogConfiguration' {} a -> s {s3KeyPrefix = a} :: ExecuteCommandLogConfiguration)

-- | The name of the S3 bucket to send logs to.
--
-- The S3 bucket must already be created.
executeCommandLogConfiguration_s3BucketName :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Text)
executeCommandLogConfiguration_s3BucketName = Lens.lens (\ExecuteCommandLogConfiguration' {s3BucketName} -> s3BucketName) (\s@ExecuteCommandLogConfiguration' {} a -> s {s3BucketName = a} :: ExecuteCommandLogConfiguration)

-- | Determines whether to use encryption on the S3 logs. If not specified,
-- encryption is not used.
executeCommandLogConfiguration_s3EncryptionEnabled :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Bool)
executeCommandLogConfiguration_s3EncryptionEnabled = Lens.lens (\ExecuteCommandLogConfiguration' {s3EncryptionEnabled} -> s3EncryptionEnabled) (\s@ExecuteCommandLogConfiguration' {} a -> s {s3EncryptionEnabled = a} :: ExecuteCommandLogConfiguration)

-- | Determines whether to use encryption on the CloudWatch logs. If not
-- specified, encryption will be disabled.
executeCommandLogConfiguration_cloudWatchEncryptionEnabled :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Bool)
executeCommandLogConfiguration_cloudWatchEncryptionEnabled = Lens.lens (\ExecuteCommandLogConfiguration' {cloudWatchEncryptionEnabled} -> cloudWatchEncryptionEnabled) (\s@ExecuteCommandLogConfiguration' {} a -> s {cloudWatchEncryptionEnabled = a} :: ExecuteCommandLogConfiguration)

-- | The name of the CloudWatch log group to send logs to.
--
-- The CloudWatch log group must already be created.
executeCommandLogConfiguration_cloudWatchLogGroupName :: Lens.Lens' ExecuteCommandLogConfiguration (Prelude.Maybe Prelude.Text)
executeCommandLogConfiguration_cloudWatchLogGroupName = Lens.lens (\ExecuteCommandLogConfiguration' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@ExecuteCommandLogConfiguration' {} a -> s {cloudWatchLogGroupName = a} :: ExecuteCommandLogConfiguration)

instance Data.FromJSON ExecuteCommandLogConfiguration where
  parseJSON =
    Data.withObject
      "ExecuteCommandLogConfiguration"
      ( \x ->
          ExecuteCommandLogConfiguration'
            Prelude.<$> (x Data..:? "s3KeyPrefix")
            Prelude.<*> (x Data..:? "s3BucketName")
            Prelude.<*> (x Data..:? "s3EncryptionEnabled")
            Prelude.<*> (x Data..:? "cloudWatchEncryptionEnabled")
            Prelude.<*> (x Data..:? "cloudWatchLogGroupName")
      )

instance
  Prelude.Hashable
    ExecuteCommandLogConfiguration
  where
  hashWithSalt
    _salt
    ExecuteCommandLogConfiguration' {..} =
      _salt `Prelude.hashWithSalt` s3KeyPrefix
        `Prelude.hashWithSalt` s3BucketName
        `Prelude.hashWithSalt` s3EncryptionEnabled
        `Prelude.hashWithSalt` cloudWatchEncryptionEnabled
        `Prelude.hashWithSalt` cloudWatchLogGroupName

instance
  Prelude.NFData
    ExecuteCommandLogConfiguration
  where
  rnf ExecuteCommandLogConfiguration' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3EncryptionEnabled
      `Prelude.seq` Prelude.rnf cloudWatchEncryptionEnabled
      `Prelude.seq` Prelude.rnf cloudWatchLogGroupName

instance Data.ToJSON ExecuteCommandLogConfiguration where
  toJSON ExecuteCommandLogConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix,
            ("s3BucketName" Data..=) Prelude.<$> s3BucketName,
            ("s3EncryptionEnabled" Data..=)
              Prelude.<$> s3EncryptionEnabled,
            ("cloudWatchEncryptionEnabled" Data..=)
              Prelude.<$> cloudWatchEncryptionEnabled,
            ("cloudWatchLogGroupName" Data..=)
              Prelude.<$> cloudWatchLogGroupName
          ]
      )
