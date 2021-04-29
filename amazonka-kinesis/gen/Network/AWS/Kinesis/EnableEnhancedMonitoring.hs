{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Kinesis.EnableEnhancedMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables enhanced Kinesis data stream monitoring for shard-level metrics.
module Network.AWS.Kinesis.EnableEnhancedMonitoring
  ( -- * Creating a Request
    EnableEnhancedMonitoring (..),
    newEnableEnhancedMonitoring,

    -- * Request Lenses
    enableEnhancedMonitoring_streamName,
    enableEnhancedMonitoring_shardLevelMetrics,

    -- * Destructuring the Response
    EnhancedMonitoringOutput (..),
    newEnhancedMonitoringOutput,

    -- * Response Lenses
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,
    enhancedMonitoringOutput_desiredShardLevelMetrics,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for EnableEnhancedMonitoring.
--
-- /See:/ 'newEnableEnhancedMonitoring' smart constructor.
data EnableEnhancedMonitoring = EnableEnhancedMonitoring'
  { -- | The name of the stream for which to enable enhanced monitoring.
    streamName :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableEnhancedMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
  -- | 'streamName'
  Prelude.Text ->
  EnableEnhancedMonitoring
newEnableEnhancedMonitoring pStreamName_ =
  EnableEnhancedMonitoring'
    { streamName =
        pStreamName_,
      shardLevelMetrics = Prelude.mempty
    }

-- | The name of the stream for which to enable enhanced monitoring.
enableEnhancedMonitoring_streamName :: Lens.Lens' EnableEnhancedMonitoring Prelude.Text
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
enableEnhancedMonitoring_shardLevelMetrics = Lens.lens (\EnableEnhancedMonitoring' {shardLevelMetrics} -> shardLevelMetrics) (\s@EnableEnhancedMonitoring' {} a -> s {shardLevelMetrics = a} :: EnableEnhancedMonitoring) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest EnableEnhancedMonitoring where
  type
    Rs EnableEnhancedMonitoring =
      EnhancedMonitoringOutput
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable EnableEnhancedMonitoring

instance Prelude.NFData EnableEnhancedMonitoring

instance Prelude.ToHeaders EnableEnhancedMonitoring where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Kinesis_20131202.EnableEnhancedMonitoring" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EnableEnhancedMonitoring where
  toJSON EnableEnhancedMonitoring' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Prelude..= streamName),
            Prelude.Just
              ("ShardLevelMetrics" Prelude..= shardLevelMetrics)
          ]
      )

instance Prelude.ToPath EnableEnhancedMonitoring where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableEnhancedMonitoring where
  toQuery = Prelude.const Prelude.mempty
