{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DisableEnhancedMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables enhanced monitoring.
module Network.AWS.Kinesis.DisableEnhancedMonitoring
  ( -- * Creating a request
    DisableEnhancedMonitoring (..),
    mkDisableEnhancedMonitoring,

    -- ** Request lenses
    demStreamName,
    demShardLevelMetrics,

    -- * Destructuring the response
    Types.EnhancedMonitoringOutput (..),
    Types.mkEnhancedMonitoringOutput,

    -- ** Response lenses
    Types.emoCurrentShardLevelMetrics,
    Types.emoDesiredShardLevelMetrics,
    Types.emoStreamName,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for 'DisableEnhancedMonitoring' .
--
-- /See:/ 'mkDisableEnhancedMonitoring' smart constructor.
data DisableEnhancedMonitoring = DisableEnhancedMonitoring'
  { -- | The name of the Kinesis data stream for which to disable enhanced monitoring.
    streamName :: Types.StreamName,
    -- | List of shard-level metrics to disable.
    --
    -- The following are the valid shard-level metrics. The value "@ALL@ " disables every metric.
    --
    --     * @IncomingBytes@
    --
    --
    --     * @IncomingRecords@
    --
    --
    --     * @OutgoingBytes@
    --
    --
    --     * @OutgoingRecords@
    --
    --
    --     * @WriteProvisionedThroughputExceeded@
    --
    --
    --     * @ReadProvisionedThroughputExceeded@
    --
    --
    --     * @IteratorAgeMilliseconds@
    --
    --
    --     * @ALL@
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
    shardLevelMetrics :: [Types.MetricsName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableEnhancedMonitoring' value with any optional fields omitted.
mkDisableEnhancedMonitoring ::
  -- | 'streamName'
  Types.StreamName ->
  DisableEnhancedMonitoring
mkDisableEnhancedMonitoring streamName =
  DisableEnhancedMonitoring'
    { streamName,
      shardLevelMetrics = Core.mempty
    }

-- | The name of the Kinesis data stream for which to disable enhanced monitoring.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demStreamName :: Lens.Lens' DisableEnhancedMonitoring Types.StreamName
demStreamName = Lens.field @"streamName"
{-# DEPRECATED demStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | List of shard-level metrics to disable.
--
-- The following are the valid shard-level metrics. The value "@ALL@ " disables every metric.
--
--     * @IncomingBytes@
--
--
--     * @IncomingRecords@
--
--
--     * @OutgoingBytes@
--
--
--     * @OutgoingRecords@
--
--
--     * @WriteProvisionedThroughputExceeded@
--
--
--     * @ReadProvisionedThroughputExceeded@
--
--
--     * @IteratorAgeMilliseconds@
--
--
--     * @ALL@
--
--
-- For more information, see <https://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
--
-- /Note:/ Consider using 'shardLevelMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demShardLevelMetrics :: Lens.Lens' DisableEnhancedMonitoring [Types.MetricsName]
demShardLevelMetrics = Lens.field @"shardLevelMetrics"
{-# DEPRECATED demShardLevelMetrics "Use generic-lens or generic-optics with 'shardLevelMetrics' instead." #-}

instance Core.FromJSON DisableEnhancedMonitoring where
  toJSON DisableEnhancedMonitoring {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("ShardLevelMetrics" Core..= shardLevelMetrics)
          ]
      )

instance Core.AWSRequest DisableEnhancedMonitoring where
  type Rs DisableEnhancedMonitoring = Types.EnhancedMonitoringOutput
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Kinesis_20131202.DisableEnhancedMonitoring")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
