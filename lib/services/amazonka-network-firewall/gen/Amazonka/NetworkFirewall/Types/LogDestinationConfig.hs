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
-- Module      : Amazonka.NetworkFirewall.Types.LogDestinationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.LogDestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.LogDestinationType
import Amazonka.NetworkFirewall.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | Defines where Network Firewall sends logs for the firewall for one log
-- type. This is used in LoggingConfiguration. You can send each type of
-- log to an Amazon S3 bucket, a CloudWatch log group, or a Kinesis Data
-- Firehose delivery stream.
--
-- Network Firewall generates logs for stateful rule groups. You can save
-- alert and flow log types. The stateful rules engine records flow logs
-- for all network traffic that it receives. It records alert logs for
-- traffic that matches stateful rules that have the rule action set to
-- @DROP@ or @ALERT@.
--
-- /See:/ 'newLogDestinationConfig' smart constructor.
data LogDestinationConfig = LogDestinationConfig'
  { -- | The type of log to send. Alert logs report traffic that matches a
    -- StatefulRule with an action setting that sends an alert log message.
    -- Flow logs are standard network traffic flow logs.
    logType :: LogType,
    -- | The type of storage destination to send these logs to. You can send logs
    -- to an Amazon S3 bucket, a CloudWatch log group, or a Kinesis Data
    -- Firehose delivery stream.
    logDestinationType :: LogDestinationType,
    -- | The named location for the logs, provided in a key:value mapping that is
    -- specific to the chosen destination type.
    --
    -- -   For an Amazon S3 bucket, provide the name of the bucket, with key
    --     @bucketName@, and optionally provide a prefix, with key @prefix@.
    --     The following example specifies an Amazon S3 bucket named
    --     @DOC-EXAMPLE-BUCKET@ and the prefix @alerts@:
    --
    --     @\"LogDestination\": { \"bucketName\": \"DOC-EXAMPLE-BUCKET\", \"prefix\": \"alerts\" }@
    --
    -- -   For a CloudWatch log group, provide the name of the CloudWatch log
    --     group, with key @logGroup@. The following example specifies a log
    --     group named @alert-log-group@:
    --
    --     @\"LogDestination\": { \"logGroup\": \"alert-log-group\" }@
    --
    -- -   For a Kinesis Data Firehose delivery stream, provide the name of the
    --     delivery stream, with key @deliveryStream@. The following example
    --     specifies a delivery stream named @alert-delivery-stream@:
    --
    --     @\"LogDestination\": { \"deliveryStream\": \"alert-delivery-stream\" }@
    logDestination :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogDestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logType', 'logDestinationConfig_logType' - The type of log to send. Alert logs report traffic that matches a
-- StatefulRule with an action setting that sends an alert log message.
-- Flow logs are standard network traffic flow logs.
--
-- 'logDestinationType', 'logDestinationConfig_logDestinationType' - The type of storage destination to send these logs to. You can send logs
-- to an Amazon S3 bucket, a CloudWatch log group, or a Kinesis Data
-- Firehose delivery stream.
--
-- 'logDestination', 'logDestinationConfig_logDestination' - The named location for the logs, provided in a key:value mapping that is
-- specific to the chosen destination type.
--
-- -   For an Amazon S3 bucket, provide the name of the bucket, with key
--     @bucketName@, and optionally provide a prefix, with key @prefix@.
--     The following example specifies an Amazon S3 bucket named
--     @DOC-EXAMPLE-BUCKET@ and the prefix @alerts@:
--
--     @\"LogDestination\": { \"bucketName\": \"DOC-EXAMPLE-BUCKET\", \"prefix\": \"alerts\" }@
--
-- -   For a CloudWatch log group, provide the name of the CloudWatch log
--     group, with key @logGroup@. The following example specifies a log
--     group named @alert-log-group@:
--
--     @\"LogDestination\": { \"logGroup\": \"alert-log-group\" }@
--
-- -   For a Kinesis Data Firehose delivery stream, provide the name of the
--     delivery stream, with key @deliveryStream@. The following example
--     specifies a delivery stream named @alert-delivery-stream@:
--
--     @\"LogDestination\": { \"deliveryStream\": \"alert-delivery-stream\" }@
newLogDestinationConfig ::
  -- | 'logType'
  LogType ->
  -- | 'logDestinationType'
  LogDestinationType ->
  LogDestinationConfig
newLogDestinationConfig
  pLogType_
  pLogDestinationType_ =
    LogDestinationConfig'
      { logType = pLogType_,
        logDestinationType = pLogDestinationType_,
        logDestination = Prelude.mempty
      }

-- | The type of log to send. Alert logs report traffic that matches a
-- StatefulRule with an action setting that sends an alert log message.
-- Flow logs are standard network traffic flow logs.
logDestinationConfig_logType :: Lens.Lens' LogDestinationConfig LogType
logDestinationConfig_logType = Lens.lens (\LogDestinationConfig' {logType} -> logType) (\s@LogDestinationConfig' {} a -> s {logType = a} :: LogDestinationConfig)

-- | The type of storage destination to send these logs to. You can send logs
-- to an Amazon S3 bucket, a CloudWatch log group, or a Kinesis Data
-- Firehose delivery stream.
logDestinationConfig_logDestinationType :: Lens.Lens' LogDestinationConfig LogDestinationType
logDestinationConfig_logDestinationType = Lens.lens (\LogDestinationConfig' {logDestinationType} -> logDestinationType) (\s@LogDestinationConfig' {} a -> s {logDestinationType = a} :: LogDestinationConfig)

-- | The named location for the logs, provided in a key:value mapping that is
-- specific to the chosen destination type.
--
-- -   For an Amazon S3 bucket, provide the name of the bucket, with key
--     @bucketName@, and optionally provide a prefix, with key @prefix@.
--     The following example specifies an Amazon S3 bucket named
--     @DOC-EXAMPLE-BUCKET@ and the prefix @alerts@:
--
--     @\"LogDestination\": { \"bucketName\": \"DOC-EXAMPLE-BUCKET\", \"prefix\": \"alerts\" }@
--
-- -   For a CloudWatch log group, provide the name of the CloudWatch log
--     group, with key @logGroup@. The following example specifies a log
--     group named @alert-log-group@:
--
--     @\"LogDestination\": { \"logGroup\": \"alert-log-group\" }@
--
-- -   For a Kinesis Data Firehose delivery stream, provide the name of the
--     delivery stream, with key @deliveryStream@. The following example
--     specifies a delivery stream named @alert-delivery-stream@:
--
--     @\"LogDestination\": { \"deliveryStream\": \"alert-delivery-stream\" }@
logDestinationConfig_logDestination :: Lens.Lens' LogDestinationConfig (Prelude.HashMap Prelude.Text Prelude.Text)
logDestinationConfig_logDestination = Lens.lens (\LogDestinationConfig' {logDestination} -> logDestination) (\s@LogDestinationConfig' {} a -> s {logDestination = a} :: LogDestinationConfig) Prelude.. Lens.coerced

instance Data.FromJSON LogDestinationConfig where
  parseJSON =
    Data.withObject
      "LogDestinationConfig"
      ( \x ->
          LogDestinationConfig'
            Prelude.<$> (x Data..: "LogType")
            Prelude.<*> (x Data..: "LogDestinationType")
            Prelude.<*> ( x
                            Data..:? "LogDestination"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LogDestinationConfig where
  hashWithSalt _salt LogDestinationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` logType
      `Prelude.hashWithSalt` logDestinationType
      `Prelude.hashWithSalt` logDestination

instance Prelude.NFData LogDestinationConfig where
  rnf LogDestinationConfig' {..} =
    Prelude.rnf logType
      `Prelude.seq` Prelude.rnf logDestinationType
      `Prelude.seq` Prelude.rnf logDestination

instance Data.ToJSON LogDestinationConfig where
  toJSON LogDestinationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("LogType" Data..= logType),
            Prelude.Just
              ("LogDestinationType" Data..= logDestinationType),
            Prelude.Just
              ("LogDestination" Data..= logDestination)
          ]
      )
