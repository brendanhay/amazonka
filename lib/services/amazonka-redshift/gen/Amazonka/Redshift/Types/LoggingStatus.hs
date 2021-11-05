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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.LoggingStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes the status of logging for a cluster.
--
-- /See:/ 'newLoggingStatus' smart constructor.
data LoggingStatus = LoggingStatus'
  { -- | The last time when logs failed to be delivered.
    lastFailureTime :: Prelude.Maybe Core.ISO8601,
    -- | The last time that logs were delivered.
    lastSuccessfulDeliveryTime :: Prelude.Maybe Core.ISO8601,
    -- | The prefix applied to the log file names.
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket where the log files are stored.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | @true@ if logging is on, @false@ if logging is off.
    loggingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The message indicating that logs failed to be delivered.
    lastFailureMessage :: Prelude.Maybe Prelude.Text
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
-- 'lastFailureTime', 'loggingStatus_lastFailureTime' - The last time when logs failed to be delivered.
--
-- 'lastSuccessfulDeliveryTime', 'loggingStatus_lastSuccessfulDeliveryTime' - The last time that logs were delivered.
--
-- 's3KeyPrefix', 'loggingStatus_s3KeyPrefix' - The prefix applied to the log file names.
--
-- 'bucketName', 'loggingStatus_bucketName' - The name of the S3 bucket where the log files are stored.
--
-- 'loggingEnabled', 'loggingStatus_loggingEnabled' - @true@ if logging is on, @false@ if logging is off.
--
-- 'lastFailureMessage', 'loggingStatus_lastFailureMessage' - The message indicating that logs failed to be delivered.
newLoggingStatus ::
  LoggingStatus
newLoggingStatus =
  LoggingStatus'
    { lastFailureTime = Prelude.Nothing,
      lastSuccessfulDeliveryTime = Prelude.Nothing,
      s3KeyPrefix = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      loggingEnabled = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing
    }

-- | The last time when logs failed to be delivered.
loggingStatus_lastFailureTime :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.UTCTime)
loggingStatus_lastFailureTime = Lens.lens (\LoggingStatus' {lastFailureTime} -> lastFailureTime) (\s@LoggingStatus' {} a -> s {lastFailureTime = a} :: LoggingStatus) Prelude.. Lens.mapping Core._Time

-- | The last time that logs were delivered.
loggingStatus_lastSuccessfulDeliveryTime :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.UTCTime)
loggingStatus_lastSuccessfulDeliveryTime = Lens.lens (\LoggingStatus' {lastSuccessfulDeliveryTime} -> lastSuccessfulDeliveryTime) (\s@LoggingStatus' {} a -> s {lastSuccessfulDeliveryTime = a} :: LoggingStatus) Prelude.. Lens.mapping Core._Time

-- | The prefix applied to the log file names.
loggingStatus_s3KeyPrefix :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Text)
loggingStatus_s3KeyPrefix = Lens.lens (\LoggingStatus' {s3KeyPrefix} -> s3KeyPrefix) (\s@LoggingStatus' {} a -> s {s3KeyPrefix = a} :: LoggingStatus)

-- | The name of the S3 bucket where the log files are stored.
loggingStatus_bucketName :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Text)
loggingStatus_bucketName = Lens.lens (\LoggingStatus' {bucketName} -> bucketName) (\s@LoggingStatus' {} a -> s {bucketName = a} :: LoggingStatus)

-- | @true@ if logging is on, @false@ if logging is off.
loggingStatus_loggingEnabled :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Bool)
loggingStatus_loggingEnabled = Lens.lens (\LoggingStatus' {loggingEnabled} -> loggingEnabled) (\s@LoggingStatus' {} a -> s {loggingEnabled = a} :: LoggingStatus)

-- | The message indicating that logs failed to be delivered.
loggingStatus_lastFailureMessage :: Lens.Lens' LoggingStatus (Prelude.Maybe Prelude.Text)
loggingStatus_lastFailureMessage = Lens.lens (\LoggingStatus' {lastFailureMessage} -> lastFailureMessage) (\s@LoggingStatus' {} a -> s {lastFailureMessage = a} :: LoggingStatus)

instance Core.FromXML LoggingStatus where
  parseXML x =
    LoggingStatus'
      Prelude.<$> (x Core..@? "LastFailureTime")
      Prelude.<*> (x Core..@? "LastSuccessfulDeliveryTime")
      Prelude.<*> (x Core..@? "S3KeyPrefix")
      Prelude.<*> (x Core..@? "BucketName")
      Prelude.<*> (x Core..@? "LoggingEnabled")
      Prelude.<*> (x Core..@? "LastFailureMessage")

instance Prelude.Hashable LoggingStatus

instance Prelude.NFData LoggingStatus
