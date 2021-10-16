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
-- Module      : Network.AWS.ElastiCache.Types.PendingLogDeliveryConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.PendingLogDeliveryConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.DestinationDetails
import Network.AWS.ElastiCache.Types.DestinationType
import Network.AWS.ElastiCache.Types.LogFormat
import Network.AWS.ElastiCache.Types.LogType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The log delivery configurations being modified
--
-- /See:/ 'newPendingLogDeliveryConfiguration' smart constructor.
data PendingLogDeliveryConfiguration = PendingLogDeliveryConfiguration'
  { -- | Returns the destination type, either CloudWatch Logs or Kinesis Data
    -- Firehose.
    destinationType :: Prelude.Maybe DestinationType,
    -- | Refers to <https://redis.io/commands/slowlog slow-log>.
    logType :: Prelude.Maybe LogType,
    -- | Returns the log format, either JSON or TEXT
    logFormat :: Prelude.Maybe LogFormat,
    -- | Configuration details of either a CloudWatch Logs destination or Kinesis
    -- Data Firehose destination.
    destinationDetails :: Prelude.Maybe DestinationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingLogDeliveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationType', 'pendingLogDeliveryConfiguration_destinationType' - Returns the destination type, either CloudWatch Logs or Kinesis Data
-- Firehose.
--
-- 'logType', 'pendingLogDeliveryConfiguration_logType' - Refers to <https://redis.io/commands/slowlog slow-log>.
--
-- 'logFormat', 'pendingLogDeliveryConfiguration_logFormat' - Returns the log format, either JSON or TEXT
--
-- 'destinationDetails', 'pendingLogDeliveryConfiguration_destinationDetails' - Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
newPendingLogDeliveryConfiguration ::
  PendingLogDeliveryConfiguration
newPendingLogDeliveryConfiguration =
  PendingLogDeliveryConfiguration'
    { destinationType =
        Prelude.Nothing,
      logType = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      destinationDetails = Prelude.Nothing
    }

-- | Returns the destination type, either CloudWatch Logs or Kinesis Data
-- Firehose.
pendingLogDeliveryConfiguration_destinationType :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe DestinationType)
pendingLogDeliveryConfiguration_destinationType = Lens.lens (\PendingLogDeliveryConfiguration' {destinationType} -> destinationType) (\s@PendingLogDeliveryConfiguration' {} a -> s {destinationType = a} :: PendingLogDeliveryConfiguration)

-- | Refers to <https://redis.io/commands/slowlog slow-log>.
pendingLogDeliveryConfiguration_logType :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe LogType)
pendingLogDeliveryConfiguration_logType = Lens.lens (\PendingLogDeliveryConfiguration' {logType} -> logType) (\s@PendingLogDeliveryConfiguration' {} a -> s {logType = a} :: PendingLogDeliveryConfiguration)

-- | Returns the log format, either JSON or TEXT
pendingLogDeliveryConfiguration_logFormat :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe LogFormat)
pendingLogDeliveryConfiguration_logFormat = Lens.lens (\PendingLogDeliveryConfiguration' {logFormat} -> logFormat) (\s@PendingLogDeliveryConfiguration' {} a -> s {logFormat = a} :: PendingLogDeliveryConfiguration)

-- | Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
pendingLogDeliveryConfiguration_destinationDetails :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe DestinationDetails)
pendingLogDeliveryConfiguration_destinationDetails = Lens.lens (\PendingLogDeliveryConfiguration' {destinationDetails} -> destinationDetails) (\s@PendingLogDeliveryConfiguration' {} a -> s {destinationDetails = a} :: PendingLogDeliveryConfiguration)

instance Core.FromXML PendingLogDeliveryConfiguration where
  parseXML x =
    PendingLogDeliveryConfiguration'
      Prelude.<$> (x Core..@? "DestinationType")
      Prelude.<*> (x Core..@? "LogType")
      Prelude.<*> (x Core..@? "LogFormat")
      Prelude.<*> (x Core..@? "DestinationDetails")

instance
  Prelude.Hashable
    PendingLogDeliveryConfiguration

instance
  Prelude.NFData
    PendingLogDeliveryConfiguration
