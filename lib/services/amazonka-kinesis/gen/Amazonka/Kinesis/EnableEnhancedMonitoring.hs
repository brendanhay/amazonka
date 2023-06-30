{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kinesis.EnableEnhancedMonitoring
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables enhanced Kinesis data stream monitoring for shard-level metrics.
--
-- When invoking this API, it is recommended you use the @StreamARN@ input
-- parameter rather than the @StreamName@ input parameter.
module Amazonka.Kinesis.EnableEnhancedMonitoring
  ( -- * Creating a Request
    EnableEnhancedMonitoring (..),
    newEnableEnhancedMonitoring,

    -- * Request Lenses
    enableEnhancedMonitoring_streamARN,
    enableEnhancedMonitoring_streamName,
    enableEnhancedMonitoring_shardLevelMetrics,

    -- * Destructuring the Response
    EnhancedMonitoringOutput (..),
    newEnhancedMonitoringOutput,

    -- * Response Lenses
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_streamARN,
    enhancedMonitoringOutput_streamName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for EnableEnhancedMonitoring.
--
-- /See:/ 'newEnableEnhancedMonitoring' smart constructor.
data EnableEnhancedMonitoring = EnableEnhancedMonitoring'
  { -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream for which to enable enhanced monitoring.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | List of shard-level metrics to enable.
    --
    -- The following are the valid shard-level metrics. The value \"@ALL@\"
    -- enables every metric.
    --
    -- -   @IncomingBytes@
    --
    -- -   @IncomingRecords@
    --
    -- -   @OutgoingBytes@
    --
    -- -   @OutgoingRecords@
    --
    -- -   @WriteProvisionedThroughputExceeded@
    --
    -- -   @ReadProvisionedThroughputExceeded@
    --
    -- -   @IteratorAgeMilliseconds@
    --
    -- -   @ALL@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch>
    -- in the /Amazon Kinesis Data Streams Developer Guide/.
    shardLevelMetrics :: [MetricsName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableEnhancedMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'enableEnhancedMonitoring_streamARN' - The ARN of the stream.
--
-- 'streamName', 'enableEnhancedMonitoring_streamName' - The name of the stream for which to enable enhanced monitoring.
--
-- 'shardLevelMetrics', 'enableEnhancedMonitoring_shardLevelMetrics' - List of shard-level metrics to enable.
--
-- The following are the valid shard-level metrics. The value \"@ALL@\"
-- enables every metric.
--
-- -   @IncomingBytes@
--
-- -   @IncomingRecords@
--
-- -   @OutgoingBytes@
--
-- -   @OutgoingRecords@
--
-- -   @WriteProvisionedThroughputExceeded@
--
-- -   @ReadProvisionedThroughputExceeded@
--
-- -   @IteratorAgeMilliseconds@
--
-- -   @ALL@
--
-- For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
newEnableEnhancedMonitoring ::
  EnableEnhancedMonitoring
newEnableEnhancedMonitoring =
  EnableEnhancedMonitoring'
    { streamARN =
        Prelude.Nothing,
      streamName = Prelude.Nothing,
      shardLevelMetrics = Prelude.mempty
    }

-- | The ARN of the stream.
enableEnhancedMonitoring_streamARN :: Lens.Lens' EnableEnhancedMonitoring (Prelude.Maybe Prelude.Text)
enableEnhancedMonitoring_streamARN = Lens.lens (\EnableEnhancedMonitoring' {streamARN} -> streamARN) (\s@EnableEnhancedMonitoring' {} a -> s {streamARN = a} :: EnableEnhancedMonitoring)

-- | The name of the stream for which to enable enhanced monitoring.
enableEnhancedMonitoring_streamName :: Lens.Lens' EnableEnhancedMonitoring (Prelude.Maybe Prelude.Text)
enableEnhancedMonitoring_streamName = Lens.lens (\EnableEnhancedMonitoring' {streamName} -> streamName) (\s@EnableEnhancedMonitoring' {} a -> s {streamName = a} :: EnableEnhancedMonitoring)

-- | List of shard-level metrics to enable.
--
-- The following are the valid shard-level metrics. The value \"@ALL@\"
-- enables every metric.
--
-- -   @IncomingBytes@
--
-- -   @IncomingRecords@
--
-- -   @OutgoingBytes@
--
-- -   @OutgoingRecords@
--
-- -   @WriteProvisionedThroughputExceeded@
--
-- -   @ReadProvisionedThroughputExceeded@
--
-- -   @IteratorAgeMilliseconds@
--
-- -   @ALL@
--
-- For more information, see
-- <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch>
-- in the /Amazon Kinesis Data Streams Developer Guide/.
enableEnhancedMonitoring_shardLevelMetrics :: Lens.Lens' EnableEnhancedMonitoring [MetricsName]
enableEnhancedMonitoring_shardLevelMetrics = Lens.lens (\EnableEnhancedMonitoring' {shardLevelMetrics} -> shardLevelMetrics) (\s@EnableEnhancedMonitoring' {} a -> s {shardLevelMetrics = a} :: EnableEnhancedMonitoring) Prelude.. Lens.coerced

instance Core.AWSRequest EnableEnhancedMonitoring where
  type
    AWSResponse EnableEnhancedMonitoring =
      EnhancedMonitoringOutput
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable EnableEnhancedMonitoring where
  hashWithSalt _salt EnableEnhancedMonitoring' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` shardLevelMetrics

instance Prelude.NFData EnableEnhancedMonitoring where
  rnf EnableEnhancedMonitoring' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf shardLevelMetrics

instance Data.ToHeaders EnableEnhancedMonitoring where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.EnableEnhancedMonitoring" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableEnhancedMonitoring where
  toJSON EnableEnhancedMonitoring' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just
              ("ShardLevelMetrics" Data..= shardLevelMetrics)
          ]
      )

instance Data.ToPath EnableEnhancedMonitoring where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableEnhancedMonitoring where
  toQuery = Prelude.const Prelude.mempty
