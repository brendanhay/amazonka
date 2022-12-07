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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketLoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about logging for the S3 bucket
--
-- /See:/ 'newAwsS3BucketLoggingConfiguration' smart constructor.
data AwsS3BucketLoggingConfiguration = AwsS3BucketLoggingConfiguration'
  { -- | The name of the S3 bucket where log files for the S3 bucket are stored.
    destinationBucketName :: Prelude.Maybe Prelude.Text,
    -- | The prefix added to log files for the S3 bucket.
    logFilePrefix :: Prelude.Maybe Prelude.Text
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
-- 'destinationBucketName', 'awsS3BucketLoggingConfiguration_destinationBucketName' - The name of the S3 bucket where log files for the S3 bucket are stored.
--
-- 'logFilePrefix', 'awsS3BucketLoggingConfiguration_logFilePrefix' - The prefix added to log files for the S3 bucket.
newAwsS3BucketLoggingConfiguration ::
  AwsS3BucketLoggingConfiguration
newAwsS3BucketLoggingConfiguration =
  AwsS3BucketLoggingConfiguration'
    { destinationBucketName =
        Prelude.Nothing,
      logFilePrefix = Prelude.Nothing
    }

-- | The name of the S3 bucket where log files for the S3 bucket are stored.
awsS3BucketLoggingConfiguration_destinationBucketName :: Lens.Lens' AwsS3BucketLoggingConfiguration (Prelude.Maybe Prelude.Text)
awsS3BucketLoggingConfiguration_destinationBucketName = Lens.lens (\AwsS3BucketLoggingConfiguration' {destinationBucketName} -> destinationBucketName) (\s@AwsS3BucketLoggingConfiguration' {} a -> s {destinationBucketName = a} :: AwsS3BucketLoggingConfiguration)

-- | The prefix added to log files for the S3 bucket.
awsS3BucketLoggingConfiguration_logFilePrefix :: Lens.Lens' AwsS3BucketLoggingConfiguration (Prelude.Maybe Prelude.Text)
awsS3BucketLoggingConfiguration_logFilePrefix = Lens.lens (\AwsS3BucketLoggingConfiguration' {logFilePrefix} -> logFilePrefix) (\s@AwsS3BucketLoggingConfiguration' {} a -> s {logFilePrefix = a} :: AwsS3BucketLoggingConfiguration)

instance
  Data.FromJSON
    AwsS3BucketLoggingConfiguration
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketLoggingConfiguration"
      ( \x ->
          AwsS3BucketLoggingConfiguration'
            Prelude.<$> (x Data..:? "DestinationBucketName")
            Prelude.<*> (x Data..:? "LogFilePrefix")
      )

instance
  Prelude.Hashable
    AwsS3BucketLoggingConfiguration
  where
  hashWithSalt
    _salt
    AwsS3BucketLoggingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` destinationBucketName
        `Prelude.hashWithSalt` logFilePrefix

instance
  Prelude.NFData
    AwsS3BucketLoggingConfiguration
  where
  rnf AwsS3BucketLoggingConfiguration' {..} =
    Prelude.rnf destinationBucketName
      `Prelude.seq` Prelude.rnf logFilePrefix

instance Data.ToJSON AwsS3BucketLoggingConfiguration where
  toJSON AwsS3BucketLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationBucketName" Data..=)
              Prelude.<$> destinationBucketName,
            ("LogFilePrefix" Data..=) Prelude.<$> logFilePrefix
          ]
      )
