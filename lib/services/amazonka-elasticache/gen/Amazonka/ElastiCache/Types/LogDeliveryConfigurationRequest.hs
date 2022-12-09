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
-- Module      : Amazonka.ElastiCache.Types.LogDeliveryConfigurationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.LogDeliveryConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.DestinationDetails
import Amazonka.ElastiCache.Types.DestinationType
import Amazonka.ElastiCache.Types.LogFormat
import Amazonka.ElastiCache.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | Specifies the destination, format and type of the logs.
--
-- /See:/ 'newLogDeliveryConfigurationRequest' smart constructor.
data LogDeliveryConfigurationRequest = LogDeliveryConfigurationRequest'
  { -- | Configuration details of either a CloudWatch Logs destination or Kinesis
    -- Data Firehose destination.
    destinationDetails :: Prelude.Maybe DestinationDetails,
    -- | Specify either @cloudwatch-logs@ or @kinesis-firehose@ as the
    -- destination type.
    destinationType :: Prelude.Maybe DestinationType,
    -- | Specify if log delivery is enabled. Default @true@.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies either JSON or TEXT
    logFormat :: Prelude.Maybe LogFormat,
    -- | Refers to <https://redis.io/commands/slowlog slow-log> or engine-log..
    logType :: Prelude.Maybe LogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogDeliveryConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationDetails', 'logDeliveryConfigurationRequest_destinationDetails' - Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
--
-- 'destinationType', 'logDeliveryConfigurationRequest_destinationType' - Specify either @cloudwatch-logs@ or @kinesis-firehose@ as the
-- destination type.
--
-- 'enabled', 'logDeliveryConfigurationRequest_enabled' - Specify if log delivery is enabled. Default @true@.
--
-- 'logFormat', 'logDeliveryConfigurationRequest_logFormat' - Specifies either JSON or TEXT
--
-- 'logType', 'logDeliveryConfigurationRequest_logType' - Refers to <https://redis.io/commands/slowlog slow-log> or engine-log..
newLogDeliveryConfigurationRequest ::
  LogDeliveryConfigurationRequest
newLogDeliveryConfigurationRequest =
  LogDeliveryConfigurationRequest'
    { destinationDetails =
        Prelude.Nothing,
      destinationType = Prelude.Nothing,
      enabled = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      logType = Prelude.Nothing
    }

-- | Configuration details of either a CloudWatch Logs destination or Kinesis
-- Data Firehose destination.
logDeliveryConfigurationRequest_destinationDetails :: Lens.Lens' LogDeliveryConfigurationRequest (Prelude.Maybe DestinationDetails)
logDeliveryConfigurationRequest_destinationDetails = Lens.lens (\LogDeliveryConfigurationRequest' {destinationDetails} -> destinationDetails) (\s@LogDeliveryConfigurationRequest' {} a -> s {destinationDetails = a} :: LogDeliveryConfigurationRequest)

-- | Specify either @cloudwatch-logs@ or @kinesis-firehose@ as the
-- destination type.
logDeliveryConfigurationRequest_destinationType :: Lens.Lens' LogDeliveryConfigurationRequest (Prelude.Maybe DestinationType)
logDeliveryConfigurationRequest_destinationType = Lens.lens (\LogDeliveryConfigurationRequest' {destinationType} -> destinationType) (\s@LogDeliveryConfigurationRequest' {} a -> s {destinationType = a} :: LogDeliveryConfigurationRequest)

-- | Specify if log delivery is enabled. Default @true@.
logDeliveryConfigurationRequest_enabled :: Lens.Lens' LogDeliveryConfigurationRequest (Prelude.Maybe Prelude.Bool)
logDeliveryConfigurationRequest_enabled = Lens.lens (\LogDeliveryConfigurationRequest' {enabled} -> enabled) (\s@LogDeliveryConfigurationRequest' {} a -> s {enabled = a} :: LogDeliveryConfigurationRequest)

-- | Specifies either JSON or TEXT
logDeliveryConfigurationRequest_logFormat :: Lens.Lens' LogDeliveryConfigurationRequest (Prelude.Maybe LogFormat)
logDeliveryConfigurationRequest_logFormat = Lens.lens (\LogDeliveryConfigurationRequest' {logFormat} -> logFormat) (\s@LogDeliveryConfigurationRequest' {} a -> s {logFormat = a} :: LogDeliveryConfigurationRequest)

-- | Refers to <https://redis.io/commands/slowlog slow-log> or engine-log..
logDeliveryConfigurationRequest_logType :: Lens.Lens' LogDeliveryConfigurationRequest (Prelude.Maybe LogType)
logDeliveryConfigurationRequest_logType = Lens.lens (\LogDeliveryConfigurationRequest' {logType} -> logType) (\s@LogDeliveryConfigurationRequest' {} a -> s {logType = a} :: LogDeliveryConfigurationRequest)

instance
  Prelude.Hashable
    LogDeliveryConfigurationRequest
  where
  hashWithSalt
    _salt
    LogDeliveryConfigurationRequest' {..} =
      _salt `Prelude.hashWithSalt` destinationDetails
        `Prelude.hashWithSalt` destinationType
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` logFormat
        `Prelude.hashWithSalt` logType

instance
  Prelude.NFData
    LogDeliveryConfigurationRequest
  where
  rnf LogDeliveryConfigurationRequest' {..} =
    Prelude.rnf destinationDetails
      `Prelude.seq` Prelude.rnf destinationType
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf logFormat
      `Prelude.seq` Prelude.rnf logType

instance Data.ToQuery LogDeliveryConfigurationRequest where
  toQuery LogDeliveryConfigurationRequest' {..} =
    Prelude.mconcat
      [ "DestinationDetails" Data.=: destinationDetails,
        "DestinationType" Data.=: destinationType,
        "Enabled" Data.=: enabled,
        "LogFormat" Data.=: logFormat,
        "LogType" Data.=: logType
      ]
