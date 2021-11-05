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
-- Module      : Network.AWS.SecurityHub.Types.AwsS3BucketLoggingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsS3BucketLoggingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about logging for the S3 bucket
--
-- /See:/ 'newAwsS3BucketLoggingConfiguration' smart constructor.
data AwsS3BucketLoggingConfiguration = AwsS3BucketLoggingConfiguration'
  { -- | The prefix added to log files for the S3 bucket.
    logFilePrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket where log files for the S3 bucket are stored.
    destinationBucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logFilePrefix', 'awsS3BucketLoggingConfiguration_logFilePrefix' - The prefix added to log files for the S3 bucket.
--
-- 'destinationBucketName', 'awsS3BucketLoggingConfiguration_destinationBucketName' - The name of the S3 bucket where log files for the S3 bucket are stored.
newAwsS3BucketLoggingConfiguration ::
  AwsS3BucketLoggingConfiguration
newAwsS3BucketLoggingConfiguration =
  AwsS3BucketLoggingConfiguration'
    { logFilePrefix =
        Prelude.Nothing,
      destinationBucketName = Prelude.Nothing
    }

-- | The prefix added to log files for the S3 bucket.
awsS3BucketLoggingConfiguration_logFilePrefix :: Lens.Lens' AwsS3BucketLoggingConfiguration (Prelude.Maybe Prelude.Text)
awsS3BucketLoggingConfiguration_logFilePrefix = Lens.lens (\AwsS3BucketLoggingConfiguration' {logFilePrefix} -> logFilePrefix) (\s@AwsS3BucketLoggingConfiguration' {} a -> s {logFilePrefix = a} :: AwsS3BucketLoggingConfiguration)

-- | The name of the S3 bucket where log files for the S3 bucket are stored.
awsS3BucketLoggingConfiguration_destinationBucketName :: Lens.Lens' AwsS3BucketLoggingConfiguration (Prelude.Maybe Prelude.Text)
awsS3BucketLoggingConfiguration_destinationBucketName = Lens.lens (\AwsS3BucketLoggingConfiguration' {destinationBucketName} -> destinationBucketName) (\s@AwsS3BucketLoggingConfiguration' {} a -> s {destinationBucketName = a} :: AwsS3BucketLoggingConfiguration)

instance
  Core.FromJSON
    AwsS3BucketLoggingConfiguration
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketLoggingConfiguration"
      ( \x ->
          AwsS3BucketLoggingConfiguration'
            Prelude.<$> (x Core..:? "LogFilePrefix")
            Prelude.<*> (x Core..:? "DestinationBucketName")
      )

instance
  Prelude.Hashable
    AwsS3BucketLoggingConfiguration

instance
  Prelude.NFData
    AwsS3BucketLoggingConfiguration

instance Core.ToJSON AwsS3BucketLoggingConfiguration where
  toJSON AwsS3BucketLoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LogFilePrefix" Core..=) Prelude.<$> logFilePrefix,
            ("DestinationBucketName" Core..=)
              Prelude.<$> destinationBucketName
          ]
      )
