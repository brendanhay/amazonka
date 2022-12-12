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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the logging status of the cluster.
--
-- /See:/ 'newAwsRedshiftClusterLoggingStatus' smart constructor.
data AwsRedshiftClusterLoggingStatus = AwsRedshiftClusterLoggingStatus'
  { -- | The name of the S3 bucket where the log files are stored.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The message indicating that the logs failed to be delivered.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | The last time when logs failed to be delivered.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastFailureTime :: Prelude.Maybe Prelude.Text,
    -- | The last time that logs were delivered successfully.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastSuccessfulDeliveryTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether logging is enabled.
    loggingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Provides the prefix applied to the log file names.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text
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
-- 'bucketName', 'awsRedshiftClusterLoggingStatus_bucketName' - The name of the S3 bucket where the log files are stored.
--
-- 'lastFailureMessage', 'awsRedshiftClusterLoggingStatus_lastFailureMessage' - The message indicating that the logs failed to be delivered.
--
-- 'lastFailureTime', 'awsRedshiftClusterLoggingStatus_lastFailureTime' - The last time when logs failed to be delivered.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'lastSuccessfulDeliveryTime', 'awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime' - The last time that logs were delivered successfully.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'loggingEnabled', 'awsRedshiftClusterLoggingStatus_loggingEnabled' - Indicates whether logging is enabled.
--
-- 's3KeyPrefix', 'awsRedshiftClusterLoggingStatus_s3KeyPrefix' - Provides the prefix applied to the log file names.
newAwsRedshiftClusterLoggingStatus ::
  AwsRedshiftClusterLoggingStatus
newAwsRedshiftClusterLoggingStatus =
  AwsRedshiftClusterLoggingStatus'
    { bucketName =
        Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      lastFailureTime = Prelude.Nothing,
      lastSuccessfulDeliveryTime =
        Prelude.Nothing,
      loggingEnabled = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing
    }

-- | The name of the S3 bucket where the log files are stored.
awsRedshiftClusterLoggingStatus_bucketName :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_bucketName = Lens.lens (\AwsRedshiftClusterLoggingStatus' {bucketName} -> bucketName) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {bucketName = a} :: AwsRedshiftClusterLoggingStatus)

-- | The message indicating that the logs failed to be delivered.
awsRedshiftClusterLoggingStatus_lastFailureMessage :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_lastFailureMessage = Lens.lens (\AwsRedshiftClusterLoggingStatus' {lastFailureMessage} -> lastFailureMessage) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {lastFailureMessage = a} :: AwsRedshiftClusterLoggingStatus)

-- | The last time when logs failed to be delivered.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterLoggingStatus_lastFailureTime :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_lastFailureTime = Lens.lens (\AwsRedshiftClusterLoggingStatus' {lastFailureTime} -> lastFailureTime) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {lastFailureTime = a} :: AwsRedshiftClusterLoggingStatus)

-- | The last time that logs were delivered successfully.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_lastSuccessfulDeliveryTime = Lens.lens (\AwsRedshiftClusterLoggingStatus' {lastSuccessfulDeliveryTime} -> lastSuccessfulDeliveryTime) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {lastSuccessfulDeliveryTime = a} :: AwsRedshiftClusterLoggingStatus)

-- | Indicates whether logging is enabled.
awsRedshiftClusterLoggingStatus_loggingEnabled :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterLoggingStatus_loggingEnabled = Lens.lens (\AwsRedshiftClusterLoggingStatus' {loggingEnabled} -> loggingEnabled) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {loggingEnabled = a} :: AwsRedshiftClusterLoggingStatus)

-- | Provides the prefix applied to the log file names.
awsRedshiftClusterLoggingStatus_s3KeyPrefix :: Lens.Lens' AwsRedshiftClusterLoggingStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterLoggingStatus_s3KeyPrefix = Lens.lens (\AwsRedshiftClusterLoggingStatus' {s3KeyPrefix} -> s3KeyPrefix) (\s@AwsRedshiftClusterLoggingStatus' {} a -> s {s3KeyPrefix = a} :: AwsRedshiftClusterLoggingStatus)

instance
  Data.FromJSON
    AwsRedshiftClusterLoggingStatus
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterLoggingStatus"
      ( \x ->
          AwsRedshiftClusterLoggingStatus'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "LastFailureMessage")
            Prelude.<*> (x Data..:? "LastFailureTime")
            Prelude.<*> (x Data..:? "LastSuccessfulDeliveryTime")
            Prelude.<*> (x Data..:? "LoggingEnabled")
            Prelude.<*> (x Data..:? "S3KeyPrefix")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterLoggingStatus
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterLoggingStatus' {..} =
      _salt `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` lastFailureMessage
        `Prelude.hashWithSalt` lastFailureTime
        `Prelude.hashWithSalt` lastSuccessfulDeliveryTime
        `Prelude.hashWithSalt` loggingEnabled
        `Prelude.hashWithSalt` s3KeyPrefix

instance
  Prelude.NFData
    AwsRedshiftClusterLoggingStatus
  where
  rnf AwsRedshiftClusterLoggingStatus' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf lastFailureMessage
      `Prelude.seq` Prelude.rnf lastFailureTime
      `Prelude.seq` Prelude.rnf lastSuccessfulDeliveryTime
      `Prelude.seq` Prelude.rnf loggingEnabled
      `Prelude.seq` Prelude.rnf s3KeyPrefix

instance Data.ToJSON AwsRedshiftClusterLoggingStatus where
  toJSON AwsRedshiftClusterLoggingStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("LastFailureMessage" Data..=)
              Prelude.<$> lastFailureMessage,
            ("LastFailureTime" Data..=)
              Prelude.<$> lastFailureTime,
            ("LastSuccessfulDeliveryTime" Data..=)
              Prelude.<$> lastSuccessfulDeliveryTime,
            ("LoggingEnabled" Data..=)
              Prelude.<$> loggingEnabled,
            ("S3KeyPrefix" Data..=) Prelude.<$> s3KeyPrefix
          ]
      )
