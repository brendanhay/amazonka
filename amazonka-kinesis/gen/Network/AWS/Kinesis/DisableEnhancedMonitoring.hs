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
-- Module      : Network.AWS.Kinesis.DisableEnhancedMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables enhanced monitoring.
module Network.AWS.Kinesis.DisableEnhancedMonitoring
  ( -- * Creating a Request
    DisableEnhancedMonitoring (..),
    newDisableEnhancedMonitoring,

    -- * Request Lenses
    disableEnhancedMonitoring_streamName,
    disableEnhancedMonitoring_shardLevelMetrics,

    -- * Destructuring the Response
    EnhancedMonitoringOutput (..),
    newEnhancedMonitoringOutput,

    -- * Response Lenses
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,
    enhancedMonitoringOutput_desiredShardLevelMetrics,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for DisableEnhancedMonitoring.
--
-- /See:/ 'newDisableEnhancedMonitoring' smart constructor.
data DisableEnhancedMonitoring = DisableEnhancedMonitoring'
  { -- | The name of the Kinesis data stream for which to disable enhanced
    -- monitoring.
    streamName :: Core.Text,
    -- | List of shard-level metrics to disable.
    --
    -- The following are the valid shard-level metrics. The value \"@ALL@\"
    -- disables every metric.
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableEnhancedMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'disableEnhancedMonitoring_streamName' - The name of the Kinesis data stream for which to disable enhanced
-- monitoring.
--
-- 'shardLevelMetrics', 'disableEnhancedMonitoring_shardLevelMetrics' - List of shard-level metrics to disable.
--
-- The following are the valid shard-level metrics. The value \"@ALL@\"
-- disables every metric.
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
newDisableEnhancedMonitoring ::
  -- | 'streamName'
  Core.Text ->
  DisableEnhancedMonitoring
newDisableEnhancedMonitoring pStreamName_ =
  DisableEnhancedMonitoring'
    { streamName =
        pStreamName_,
      shardLevelMetrics = Core.mempty
    }

-- | The name of the Kinesis data stream for which to disable enhanced
-- monitoring.
disableEnhancedMonitoring_streamName :: Lens.Lens' DisableEnhancedMonitoring Core.Text
disableEnhancedMonitoring_streamName = Lens.lens (\DisableEnhancedMonitoring' {streamName} -> streamName) (\s@DisableEnhancedMonitoring' {} a -> s {streamName = a} :: DisableEnhancedMonitoring)

-- | List of shard-level metrics to disable.
--
-- The following are the valid shard-level metrics. The value \"@ALL@\"
-- disables every metric.
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
disableEnhancedMonitoring_shardLevelMetrics :: Lens.Lens' DisableEnhancedMonitoring [MetricsName]
disableEnhancedMonitoring_shardLevelMetrics = Lens.lens (\DisableEnhancedMonitoring' {shardLevelMetrics} -> shardLevelMetrics) (\s@DisableEnhancedMonitoring' {} a -> s {shardLevelMetrics = a} :: DisableEnhancedMonitoring) Core.. Lens._Coerce

instance Core.AWSRequest DisableEnhancedMonitoring where
  type
    AWSResponse DisableEnhancedMonitoring =
      EnhancedMonitoringOutput
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable DisableEnhancedMonitoring

instance Core.NFData DisableEnhancedMonitoring

instance Core.ToHeaders DisableEnhancedMonitoring where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.DisableEnhancedMonitoring" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableEnhancedMonitoring where
  toJSON DisableEnhancedMonitoring' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just
              ("ShardLevelMetrics" Core..= shardLevelMetrics)
          ]
      )

instance Core.ToPath DisableEnhancedMonitoring where
  toPath = Core.const "/"

instance Core.ToQuery DisableEnhancedMonitoring where
  toQuery = Core.const Core.mempty
