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
-- Module      : Amazonka.ElastiCache.Types.LogDeliveryConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.LogDeliveryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.DestinationDetails
import Amazonka.ElastiCache.Types.DestinationType
import Amazonka.ElastiCache.Types.LogDeliveryConfigurationStatus
import Amazonka.ElastiCache.Types.LogFormat
import Amazonka.ElastiCache.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | Returns the destination, format and type of the logs.
--
-- /See:/ 'newLogDeliveryConfiguration' smart constructor.
data LogDeliveryConfiguration = LogDeliveryConfiguration'
  { -- | Returns an error message for the log delivery configuration.
    message :: Prelude.Maybe Prelude.Text,
    -- | Refers to <https://redis.io/commands/slowlog slow-log> or engine-log.
    logType :: Prelude.Maybe LogType,
    -- | Returns the destination type, either @cloudwatch-logs@ or
    -- @kinesis-firehose@.
    destinationType :: Prelude.Maybe DestinationType,
    -- | Returns the log delivery configuration status. Values are one of
    -- @enabling@ | @disabling@ | @modifying@ | @active@ | @error@
    status :: Prelude.Maybe LogDeliveryConfigurationStatus,
    -- | Returns the log format, either JSON or TEXT.
    logFormat :: Prelude.Maybe LogFormat,
    -- | Configuration details of either a CloudWatch Logs destination or Kinesis
    -- Data Firehose destination.
    destinationDetails :: Prelude.Maybe DestinationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogDeliveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'logDeliveryConfiguration_message' - Returns an error message for the log delivery configuration.
--
-- 'logType', 'logDeliveryConfiguration_logType' - Refers to <https://redis.io/commands/slowlog slow-log> or engine-log.
--
-- 'destinationType', 'logDeliveryConfiguration_destinationType' - Returns the destination type, either @cloudwatch-logs@ or
-- @kinesis-firehose@.
--
-- 'status', 'logDeliveryConfiguration_status' - Returns the log delivery configuration status. Values are one of
-- @enabling@ | @disabling@ | @modifying@ | @active@ | @error@
--
-- 'logFormat', 'logDeliveryConfiguration_logFormat' - Returns the log format, either JSON or TEXT.
--
-- 'destinationDetails', 'logDeliveryConfiguration_destinationDetails' - Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
newLogDeliveryConfiguration ::
  LogDeliveryConfiguration
newLogDeliveryConfiguration =
  LogDeliveryConfiguration'
    { message =
        Prelude.Nothing,
      logType = Prelude.Nothing,
      destinationType = Prelude.Nothing,
      status = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      destinationDetails = Prelude.Nothing
    }

-- | Returns an error message for the log delivery configuration.
logDeliveryConfiguration_message :: Lens.Lens' LogDeliveryConfiguration (Prelude.Maybe Prelude.Text)
logDeliveryConfiguration_message = Lens.lens (\LogDeliveryConfiguration' {message} -> message) (\s@LogDeliveryConfiguration' {} a -> s {message = a} :: LogDeliveryConfiguration)

-- | Refers to <https://redis.io/commands/slowlog slow-log> or engine-log.
logDeliveryConfiguration_logType :: Lens.Lens' LogDeliveryConfiguration (Prelude.Maybe LogType)
logDeliveryConfiguration_logType = Lens.lens (\LogDeliveryConfiguration' {logType} -> logType) (\s@LogDeliveryConfiguration' {} a -> s {logType = a} :: LogDeliveryConfiguration)

-- | Returns the destination type, either @cloudwatch-logs@ or
-- @kinesis-firehose@.
logDeliveryConfiguration_destinationType :: Lens.Lens' LogDeliveryConfiguration (Prelude.Maybe DestinationType)
logDeliveryConfiguration_destinationType = Lens.lens (\LogDeliveryConfiguration' {destinationType} -> destinationType) (\s@LogDeliveryConfiguration' {} a -> s {destinationType = a} :: LogDeliveryConfiguration)

-- | Returns the log delivery configuration status. Values are one of
-- @enabling@ | @disabling@ | @modifying@ | @active@ | @error@
logDeliveryConfiguration_status :: Lens.Lens' LogDeliveryConfiguration (Prelude.Maybe LogDeliveryConfigurationStatus)
logDeliveryConfiguration_status = Lens.lens (\LogDeliveryConfiguration' {status} -> status) (\s@LogDeliveryConfiguration' {} a -> s {status = a} :: LogDeliveryConfiguration)

-- | Returns the log format, either JSON or TEXT.
logDeliveryConfiguration_logFormat :: Lens.Lens' LogDeliveryConfiguration (Prelude.Maybe LogFormat)
logDeliveryConfiguration_logFormat = Lens.lens (\LogDeliveryConfiguration' {logFormat} -> logFormat) (\s@LogDeliveryConfiguration' {} a -> s {logFormat = a} :: LogDeliveryConfiguration)

-- | Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
logDeliveryConfiguration_destinationDetails :: Lens.Lens' LogDeliveryConfiguration (Prelude.Maybe DestinationDetails)
logDeliveryConfiguration_destinationDetails = Lens.lens (\LogDeliveryConfiguration' {destinationDetails} -> destinationDetails) (\s@LogDeliveryConfiguration' {} a -> s {destinationDetails = a} :: LogDeliveryConfiguration)

instance Data.FromXML LogDeliveryConfiguration where
  parseXML x =
    LogDeliveryConfiguration'
      Prelude.<$> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "LogType")
      Prelude.<*> (x Data..@? "DestinationType")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "LogFormat")
      Prelude.<*> (x Data..@? "DestinationDetails")

instance Prelude.Hashable LogDeliveryConfiguration where
  hashWithSalt _salt LogDeliveryConfiguration' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` logType
      `Prelude.hashWithSalt` destinationType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` logFormat
      `Prelude.hashWithSalt` destinationDetails

instance Prelude.NFData LogDeliveryConfiguration where
  rnf LogDeliveryConfiguration' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf logType
      `Prelude.seq` Prelude.rnf destinationType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf logFormat
      `Prelude.seq` Prelude.rnf destinationDetails
