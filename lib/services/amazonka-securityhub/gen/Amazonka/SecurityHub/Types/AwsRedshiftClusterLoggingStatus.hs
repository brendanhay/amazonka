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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterLoggingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterLoggingStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the logging status of the cluster.
--
-- /See:/ 'newAwsRedshiftClusterLoggingStatus' smart constructor.
data AwsRedshiftClusterLoggingStatus = AwsRedshiftClusterLoggingStatus'
  { -- | The last time that logs were delivered successfully.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastSuccessfulDeliveryTime :: Prelude.Maybe Prelude.Text,
    -- | Provides the prefix applied to the log file names.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The message indicating that the logs failed to be delivered.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether logging is enabled.
    loggingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the S3 bucket where the log files are stored.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The last time when logs failed to be delivered.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastFailureTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterLoggingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastSuccessfulDeliveryTime', 'awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime' - The last time that logs were delivered successfully.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 's3KeyPrefix', 'awsRedshiftClusterLoggingStatus_s3KeyPrefix' - Provides the prefix applied to the log file names.
--
-- 'lastFailureMessage', 'awsRedshiftClusterLoggingStatus_lastFailureMessage' - The message indicating that the logs failed to be delivered.
--
-- 'loggingEnabled', 'awsRedshiftClusterLoggingStatus_loggingEnabled' - Indicates whether logging is enabled.
--
-- 'bucketName', 'awsRedshiftClusterLoggingStatus_bucketName' - The name of the S3 bucket where the log files are stored.
--
-- 'lastFailureTime', 'awsRedshiftClusterLoggingStatus_lastFailureTime' - The last time when logs failed to be delivered.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newAwsRedshiftClusterLoggingStatus ::
  AwsRedshiftClusterLoggingStatus
newAwsRedshiftClusterLoggingStatus =
  AwsRedshiftClusterLoggingStatus'
    { lastSuccessfulDeliveryTime =
        Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      loggingEnabled = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      lastFailureTime = Prelude.Nothing
    }

-- | The last time that logs were delivered successfully.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime = Lens.lens (\AwsRedshiftClusterLoggingStatus' {lastSuccessfulDeliveryTime} -> lastSuccessfulDeliveryTime) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {lastSuccessfulDeliveryTime = a} :: AwsRedshiftClusterLoggingStatus)

-- | Provides the prefix applied to the log file names.
awsRedshiftClusterLoggingStatus_s3KeyPrefix :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_s3KeyPrefix = Lens.lens (\AwsRedshiftClusterLoggingStatus' {s3KeyPrefix} -> s3KeyPrefix) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {s3KeyPrefix = a} :: AwsRedshiftClusterLoggingStatus)

-- | The message indicating that the logs failed to be delivered.
awsRedshiftClusterLoggingStatus_lastFailureMessage :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_lastFailureMessage = Lens.lens (\AwsRedshiftClusterLoggingStatus' {lastFailureMessage} -> lastFailureMessage) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {lastFailureMessage = a} :: AwsRedshiftClusterLoggingStatus)

-- | Indicates whether logging is enabled.
awsRedshiftClusterLoggingStatus_loggingEnabled :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterLoggingStatus_loggingEnabled = Lens.lens (\AwsRedshiftClusterLoggingStatus' {loggingEnabled} -> loggingEnabled) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {loggingEnabled = a} :: AwsRedshiftClusterLoggingStatus)

-- | The name of the S3 bucket where the log files are stored.
awsRedshiftClusterLoggingStatus_bucketName :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_bucketName = Lens.lens (\AwsRedshiftClusterLoggingStatus' {bucketName} -> bucketName) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {bucketName = a} :: AwsRedshiftClusterLoggingStatus)

-- | The last time when logs failed to be delivered.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterLoggingStatus_lastFailureTime :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_lastFailureTime = Lens.lens (\AwsRedshiftClusterLoggingStatus' {lastFailureTime} -> lastFailureTime) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {lastFailureTime = a} :: AwsRedshiftClusterLoggingStatus)

instance
  Core.FromJSON
    AwsRedshiftClusterLoggingStatus
  where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterLoggingStatus"
      ( \x ->
          AwsRedshiftClusterLoggingStatus'
            Prelude.<$> (x Core..:? "LastSuccessfulDeliveryTime")
            Prelude.<*> (x Core..:? "S3KeyPrefix")
            Prelude.<*> (x Core..:? "LastFailureMessage")
            Prelude.<*> (x Core..:? "LoggingEnabled")
            Prelude.<*> (x Core..:? "BucketName")
            Prelude.<*> (x Core..:? "LastFailureTime")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterLoggingStatus
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterLoggingStatus' {..} =
      _salt
        `Prelude.hashWithSalt` lastSuccessfulDeliveryTime
        `Prelude.hashWithSalt` s3KeyPrefix
        `Prelude.hashWithSalt` lastFailureMessage
        `Prelude.hashWithSalt` loggingEnabled
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` lastFailureTime

instance
  Prelude.NFData
    AwsRedshiftClusterLoggingStatus
  where
  rnf AwsRedshiftClusterLoggingStatus' {..} =
    Prelude.rnf lastSuccessfulDeliveryTime
      `Prelude.seq` Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf lastFailureMessage
      `Prelude.seq` Prelude.rnf loggingEnabled
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf lastFailureTime

instance Core.ToJSON AwsRedshiftClusterLoggingStatus where
  toJSON AwsRedshiftClusterLoggingStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LastSuccessfulDeliveryTime" Core..=)
              Prelude.<$> lastSuccessfulDeliveryTime,
            ("S3KeyPrefix" Core..=) Prelude.<$> s3KeyPrefix,
            ("LastFailureMessage" Core..=)
              Prelude.<$> lastFailureMessage,
            ("LoggingEnabled" Core..=)
              Prelude.<$> loggingEnabled,
            ("BucketName" Core..=) Prelude.<$> bucketName,
            ("LastFailureTime" Core..=)
              Prelude.<$> lastFailureTime
          ]
      )
