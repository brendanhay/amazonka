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
-- Module      : Amazonka.Redshift.Types.LoggingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.LoggingStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.LogDestinationType

-- | Describes the status of logging for a cluster.
--
-- /See:/ 'newLoggingStatus' smart constructor.
data LoggingStatus = LoggingStatus'
  { -- | The name of the S3 bucket where the log files are stored.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The message indicating that logs failed to be delivered.
    lastFailureMessage :: Prelude.Maybe Prelude.Text,
    -- | The last time when logs failed to be delivered.
    lastFailureTime :: Prelude.Maybe Data.ISO8601,
    -- | The last time that logs were delivered.
    lastSuccessfulDeliveryTime :: Prelude.Maybe Data.ISO8601,
    -- | The log destination type. An enum with possible values of @s3@ and
    -- @cloudwatch@.
    logDestinationType :: Prelude.Maybe LogDestinationType,
    -- | The collection of exported log types. Log types include the connection
    -- log, user log and user activity log.
    logExports :: Prelude.Maybe [Prelude.Text],
    -- | @true@ if logging is on, @false@ if logging is off.
    loggingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The prefix applied to the log file names.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'loggingStatus_bucketName' - The name of the S3 bucket where the log files are stored.
--
-- 'lastFailureMessage', 'loggingStatus_lastFailureMessage' - The message indicating that logs failed to be delivered.
--
-- 'lastFailureTime', 'loggingStatus_lastFailureTime' - The last time when logs failed to be delivered.
--
-- 'lastSuccessfulDeliveryTime', 'loggingStatus_lastSuccessfulDeliveryTime' - The last time that logs were delivered.
--
-- 'logDestinationType', 'loggingStatus_logDestinationType' - The log destination type. An enum with possible values of @s3@ and
-- @cloudwatch@.
--
-- 'logExports', 'loggingStatus_logExports' - The collection of exported log types. Log types include the connection
-- log, user log and user activity log.
--
-- 'loggingEnabled', 'loggingStatus_loggingEnabled' - @true@ if logging is on, @false@ if logging is off.
--
-- 's3KeyPrefix', 'loggingStatus_s3KeyPrefix' - The prefix applied to the log file names.
newLoggingStatus ::
  LoggingStatus
newLoggingStatus =
  LoggingStatus'
    { bucketName = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing,
      lastFailureTime = Prelude.Nothing,
      lastSuccessfulDeliveryTime = Prelude.Nothing,
      logDestinationType = Prelude.Nothing,
      logExports = Prelude.Nothing,
      loggingEnabled = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing
    }

-- | The name of the S3 bucket where the log files are stored.
loggingStatus_bucketName :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Text)
loggingStatus_bucketName = Lens.lens (\LoggingStatus' {bucketName} -> bucketName) (\s@LoggingStatus' {} a -> s {bucketName = a} :: LoggingStatus)

-- | The message indicating that logs failed to be delivered.
loggingStatus_lastFailureMessage :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Text)
loggingStatus_lastFailureMessage = Lens.lens (\LoggingStatus' {lastFailureMessage} -> lastFailureMessage) (\s@LoggingStatus' {} a -> s {lastFailureMessage = a} :: LoggingStatus)

-- | The last time when logs failed to be delivered.
loggingStatus_lastFailureTime :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.UTCTime)
loggingStatus_lastFailureTime = Lens.lens (\LoggingStatus' {lastFailureTime} -> lastFailureTime) (\s@LoggingStatus' {} a -> s {lastFailureTime = a} :: LoggingStatus) Prelude.. Lens.mapping Data._Time

-- | The last time that logs were delivered.
loggingStatus_lastSuccessfulDeliveryTime :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.UTCTime)
loggingStatus_lastSuccessfulDeliveryTime = Lens.lens (\LoggingStatus' {lastSuccessfulDeliveryTime} -> lastSuccessfulDeliveryTime) (\s@LoggingStatus' {} a -> s {lastSuccessfulDeliveryTime = a} :: LoggingStatus) Prelude.. Lens.mapping Data._Time

-- | The log destination type. An enum with possible values of @s3@ and
-- @cloudwatch@.
loggingStatus_logDestinationType :: Lens.Lens' LoggingStatus (Prelude.Maybe LogDestinationType)
loggingStatus_logDestinationType = Lens.lens (\LoggingStatus' {logDestinationType} -> logDestinationType) (\s@LoggingStatus' {} a -> s {logDestinationType = a} :: LoggingStatus)

-- | The collection of exported log types. Log types include the connection
-- log, user log and user activity log.
loggingStatus_logExports :: Lens.Lens' LoggingStatus (Prelude.Maybe [Prelude.Text])
loggingStatus_logExports = Lens.lens (\LoggingStatus' {logExports} -> logExports) (\s@LoggingStatus' {} a -> s {logExports = a} :: LoggingStatus) Prelude.. Lens.mapping Lens.coerced

-- | @true@ if logging is on, @false@ if logging is off.
loggingStatus_loggingEnabled :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Bool)
loggingStatus_loggingEnabled = Lens.lens (\LoggingStatus' {loggingEnabled} -> loggingEnabled) (\s@LoggingStatus' {} a -> s {loggingEnabled = a} :: LoggingStatus)

-- | The prefix applied to the log file names.
loggingStatus_s3KeyPrefix :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Text)
loggingStatus_s3KeyPrefix = Lens.lens (\LoggingStatus' {s3KeyPrefix} -> s3KeyPrefix) (\s@LoggingStatus' {} a -> s {s3KeyPrefix = a} :: LoggingStatus)

instance Data.FromXML LoggingStatus where
  parseXML x =
    LoggingStatus'
      Prelude.<$> (x Data..@? "BucketName")
      Prelude.<*> (x Data..@? "LastFailureMessage")
      Prelude.<*> (x Data..@? "LastFailureTime")
      Prelude.<*> (x Data..@? "LastSuccessfulDeliveryTime")
      Prelude.<*> (x Data..@? "LogDestinationType")
      Prelude.<*> ( x Data..@? "LogExports" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "LoggingEnabled")
      Prelude.<*> (x Data..@? "S3KeyPrefix")

instance Prelude.Hashable LoggingStatus where
  hashWithSalt _salt LoggingStatus' {..} =
    _salt `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` lastFailureMessage
      `Prelude.hashWithSalt` lastFailureTime
      `Prelude.hashWithSalt` lastSuccessfulDeliveryTime
      `Prelude.hashWithSalt` logDestinationType
      `Prelude.hashWithSalt` logExports
      `Prelude.hashWithSalt` loggingEnabled
      `Prelude.hashWithSalt` s3KeyPrefix

instance Prelude.NFData LoggingStatus where
  rnf LoggingStatus' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf lastFailureMessage
      `Prelude.seq` Prelude.rnf lastFailureTime
      `Prelude.seq` Prelude.rnf lastSuccessfulDeliveryTime
      `Prelude.seq` Prelude.rnf logDestinationType
      `Prelude.seq` Prelude.rnf logExports
      `Prelude.seq` Prelude.rnf loggingEnabled
      `Prelude.seq` Prelude.rnf s3KeyPrefix
