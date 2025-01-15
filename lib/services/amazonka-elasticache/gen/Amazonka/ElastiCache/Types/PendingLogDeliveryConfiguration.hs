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
-- Module      : Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.DestinationDetails
import Amazonka.ElastiCache.Types.DestinationType
import Amazonka.ElastiCache.Types.LogFormat
import Amazonka.ElastiCache.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | The log delivery configurations being modified
--
-- /See:/ 'newPendingLogDeliveryConfiguration' smart constructor.
data PendingLogDeliveryConfiguration = PendingLogDeliveryConfiguration'
  { -- | Configuration details of either a CloudWatch Logs destination or Kinesis
    -- Data Firehose destination.
    destinationDetails :: Prelude.Maybe DestinationDetails,
    -- | Returns the destination type, either CloudWatch Logs or Kinesis Data
    -- Firehose.
    destinationType :: Prelude.Maybe DestinationType,
    -- | Returns the log format, either JSON or TEXT
    logFormat :: Prelude.Maybe LogFormat,
    -- | Refers to <https://redis.io/commands/slowlog slow-log> or engine-log..
    logType :: Prelude.Maybe LogType
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
-- 'destinationDetails', 'pendingLogDeliveryConfiguration_destinationDetails' - Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
--
-- 'destinationType', 'pendingLogDeliveryConfiguration_destinationType' - Returns the destination type, either CloudWatch Logs or Kinesis Data
-- Firehose.
--
-- 'logFormat', 'pendingLogDeliveryConfiguration_logFormat' - Returns the log format, either JSON or TEXT
--
-- 'logType', 'pendingLogDeliveryConfiguration_logType' - Refers to <https://redis.io/commands/slowlog slow-log> or engine-log..
newPendingLogDeliveryConfiguration ::
  PendingLogDeliveryConfiguration
newPendingLogDeliveryConfiguration =
  PendingLogDeliveryConfiguration'
    { destinationDetails =
        Prelude.Nothing,
      destinationType = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      logType = Prelude.Nothing
    }

-- | Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
pendingLogDeliveryConfiguration_destinationDetails :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe DestinationDetails)
pendingLogDeliveryConfiguration_destinationDetails = Lens.lens (\PendingLogDeliveryConfiguration' {destinationDetails} -> destinationDetails) (\s@PendingLogDeliveryConfiguration' {} a -> s {destinationDetails = a} :: PendingLogDeliveryConfiguration)

-- | Returns the destination type, either CloudWatch Logs or Kinesis Data
-- Firehose.
pendingLogDeliveryConfiguration_destinationType :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe DestinationType)
pendingLogDeliveryConfiguration_destinationType = Lens.lens (\PendingLogDeliveryConfiguration' {destinationType} -> destinationType) (\s@PendingLogDeliveryConfiguration' {} a -> s {destinationType = a} :: PendingLogDeliveryConfiguration)

-- | Returns the log format, either JSON or TEXT
pendingLogDeliveryConfiguration_logFormat :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe LogFormat)
pendingLogDeliveryConfiguration_logFormat = Lens.lens (\PendingLogDeliveryConfiguration' {logFormat} -> logFormat) (\s@PendingLogDeliveryConfiguration' {} a -> s {logFormat = a} :: PendingLogDeliveryConfiguration)

-- | Refers to <https://redis.io/commands/slowlog slow-log> or engine-log..
pendingLogDeliveryConfiguration_logType :: Lens.Lens' PendingLogDeliveryConfiguration (Prelude.Maybe LogType)
pendingLogDeliveryConfiguration_logType = Lens.lens (\PendingLogDeliveryConfiguration' {logType} -> logType) (\s@PendingLogDeliveryConfiguration' {} a -> s {logType = a} :: PendingLogDeliveryConfiguration)

instance Data.FromXML PendingLogDeliveryConfiguration where
  parseXML x =
    PendingLogDeliveryConfiguration'
      Prelude.<$> (x Data..@? "DestinationDetails")
      Prelude.<*> (x Data..@? "DestinationType")
      Prelude.<*> (x Data..@? "LogFormat")
      Prelude.<*> (x Data..@? "LogType")

instance
  Prelude.Hashable
    PendingLogDeliveryConfiguration
  where
  hashWithSalt
    _salt
    PendingLogDeliveryConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` destinationDetails
        `Prelude.hashWithSalt` destinationType
        `Prelude.hashWithSalt` logFormat
        `Prelude.hashWithSalt` logType

instance
  Prelude.NFData
    PendingLogDeliveryConfiguration
  where
  rnf PendingLogDeliveryConfiguration' {..} =
    Prelude.rnf destinationDetails `Prelude.seq`
      Prelude.rnf destinationType `Prelude.seq`
        Prelude.rnf logFormat `Prelude.seq`
          Prelude.rnf logType
