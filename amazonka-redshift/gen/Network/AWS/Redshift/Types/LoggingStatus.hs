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
-- Module      : Network.AWS.Redshift.Types.LoggingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.LoggingStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes the status of logging for a cluster.
--
-- /See:/ 'newLoggingStatus' smart constructor.
data LoggingStatus = LoggingStatus'
  { -- | The last time that logs were delivered.
    lastSuccessfulDeliveryTime :: Core.Maybe Core.ISO8601,
    -- | The name of the S3 bucket where the log files are stored.
    bucketName :: Core.Maybe Core.Text,
    -- | @true@ if logging is on, @false@ if logging is off.
    loggingEnabled :: Core.Maybe Core.Bool,
    -- | The last time when logs failed to be delivered.
    lastFailureTime :: Core.Maybe Core.ISO8601,
    -- | The prefix applied to the log file names.
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | The message indicating that logs failed to be delivered.
    lastFailureMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoggingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastSuccessfulDeliveryTime', 'loggingStatus_lastSuccessfulDeliveryTime' - The last time that logs were delivered.
--
-- 'bucketName', 'loggingStatus_bucketName' - The name of the S3 bucket where the log files are stored.
--
-- 'loggingEnabled', 'loggingStatus_loggingEnabled' - @true@ if logging is on, @false@ if logging is off.
--
-- 'lastFailureTime', 'loggingStatus_lastFailureTime' - The last time when logs failed to be delivered.
--
-- 's3KeyPrefix', 'loggingStatus_s3KeyPrefix' - The prefix applied to the log file names.
--
-- 'lastFailureMessage', 'loggingStatus_lastFailureMessage' - The message indicating that logs failed to be delivered.
newLoggingStatus ::
  LoggingStatus
newLoggingStatus =
  LoggingStatus'
    { lastSuccessfulDeliveryTime =
        Core.Nothing,
      bucketName = Core.Nothing,
      loggingEnabled = Core.Nothing,
      lastFailureTime = Core.Nothing,
      s3KeyPrefix = Core.Nothing,
      lastFailureMessage = Core.Nothing
    }

-- | The last time that logs were delivered.
loggingStatus_lastSuccessfulDeliveryTime :: Lens.Lens' LoggingStatus (Core.Maybe Core.UTCTime)
loggingStatus_lastSuccessfulDeliveryTime = Lens.lens (\LoggingStatus' {lastSuccessfulDeliveryTime} -> lastSuccessfulDeliveryTime) (\s@LoggingStatus' {} a -> s {lastSuccessfulDeliveryTime = a} :: LoggingStatus) Core.. Lens.mapping Core._Time

-- | The name of the S3 bucket where the log files are stored.
loggingStatus_bucketName :: Lens.Lens' LoggingStatus (Core.Maybe Core.Text)
loggingStatus_bucketName = Lens.lens (\LoggingStatus' {bucketName} -> bucketName) (\s@LoggingStatus' {} a -> s {bucketName = a} :: LoggingStatus)

-- | @true@ if logging is on, @false@ if logging is off.
loggingStatus_loggingEnabled :: Lens.Lens' LoggingStatus (Core.Maybe Core.Bool)
loggingStatus_loggingEnabled = Lens.lens (\LoggingStatus' {loggingEnabled} -> loggingEnabled) (\s@LoggingStatus' {} a -> s {loggingEnabled = a} :: LoggingStatus)

-- | The last time when logs failed to be delivered.
loggingStatus_lastFailureTime :: Lens.Lens' LoggingStatus (Core.Maybe Core.UTCTime)
loggingStatus_lastFailureTime = Lens.lens (\LoggingStatus' {lastFailureTime} -> lastFailureTime) (\s@LoggingStatus' {} a -> s {lastFailureTime = a} :: LoggingStatus) Core.. Lens.mapping Core._Time

-- | The prefix applied to the log file names.
loggingStatus_s3KeyPrefix :: Lens.Lens' LoggingStatus (Core.Maybe Core.Text)
loggingStatus_s3KeyPrefix = Lens.lens (\LoggingStatus' {s3KeyPrefix} -> s3KeyPrefix) (\s@LoggingStatus' {} a -> s {s3KeyPrefix = a} :: LoggingStatus)

-- | The message indicating that logs failed to be delivered.
loggingStatus_lastFailureMessage :: Lens.Lens' LoggingStatus (Core.Maybe Core.Text)
loggingStatus_lastFailureMessage = Lens.lens (\LoggingStatus' {lastFailureMessage} -> lastFailureMessage) (\s@LoggingStatus' {} a -> s {lastFailureMessage = a} :: LoggingStatus)

instance Core.FromXML LoggingStatus where
  parseXML x =
    LoggingStatus'
      Core.<$> (x Core..@? "LastSuccessfulDeliveryTime")
      Core.<*> (x Core..@? "BucketName")
      Core.<*> (x Core..@? "LoggingEnabled")
      Core.<*> (x Core..@? "LastFailureTime")
      Core.<*> (x Core..@? "S3KeyPrefix")
      Core.<*> (x Core..@? "LastFailureMessage")

instance Core.Hashable LoggingStatus

instance Core.NFData LoggingStatus
