{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.EnableEnhancedMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables enhanced Kinesis data stream monitoring for shard-level metrics.
module Network.AWS.Kinesis.EnableEnhancedMonitoring
  ( -- * Creating a request
    EnableEnhancedMonitoring (..),
    mkEnableEnhancedMonitoring,

    -- ** Request lenses
    eemShardLevelMetrics,
    eemStreamName,

    -- * Destructuring the response
    EnhancedMonitoringOutput (..),
    mkEnhancedMonitoringOutput,

    -- ** Response lenses
    emoDesiredShardLevelMetrics,
    emoCurrentShardLevelMetrics,
    emoStreamName,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for 'EnableEnhancedMonitoring' .
--
-- /See:/ 'mkEnableEnhancedMonitoring' smart constructor.
data EnableEnhancedMonitoring = EnableEnhancedMonitoring'
  { -- | List of shard-level metrics to enable.
    --
    -- The following are the valid shard-level metrics. The value "@ALL@ " enables every metric.
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
    shardLevelMetrics :: [MetricsName],
    -- | The name of the stream for which to enable enhanced monitoring.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableEnhancedMonitoring' with the minimum fields required to make a request.
--
-- * 'shardLevelMetrics' - List of shard-level metrics to enable.
--
-- The following are the valid shard-level metrics. The value "@ALL@ " enables every metric.
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
-- * 'streamName' - The name of the stream for which to enable enhanced monitoring.
mkEnableEnhancedMonitoring ::
  -- | 'streamName'
  Lude.Text ->
  EnableEnhancedMonitoring
mkEnableEnhancedMonitoring pStreamName_ =
  EnableEnhancedMonitoring'
    { shardLevelMetrics = Lude.mempty,
      streamName = pStreamName_
    }

-- | List of shard-level metrics to enable.
--
-- The following are the valid shard-level metrics. The value "@ALL@ " enables every metric.
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
eemShardLevelMetrics :: Lens.Lens' EnableEnhancedMonitoring [MetricsName]
eemShardLevelMetrics = Lens.lens (shardLevelMetrics :: EnableEnhancedMonitoring -> [MetricsName]) (\s a -> s {shardLevelMetrics = a} :: EnableEnhancedMonitoring)
{-# DEPRECATED eemShardLevelMetrics "Use generic-lens or generic-optics with 'shardLevelMetrics' instead." #-}

-- | The name of the stream for which to enable enhanced monitoring.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eemStreamName :: Lens.Lens' EnableEnhancedMonitoring Lude.Text
eemStreamName = Lens.lens (streamName :: EnableEnhancedMonitoring -> Lude.Text) (\s a -> s {streamName = a} :: EnableEnhancedMonitoring)
{-# DEPRECATED eemStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest EnableEnhancedMonitoring where
  type Rs EnableEnhancedMonitoring = EnhancedMonitoringOutput
  request = Req.postJSON kinesisService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders EnableEnhancedMonitoring where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.EnableEnhancedMonitoring" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableEnhancedMonitoring where
  toJSON EnableEnhancedMonitoring' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ShardLevelMetrics" Lude..= shardLevelMetrics),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath EnableEnhancedMonitoring where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableEnhancedMonitoring where
  toQuery = Lude.const Lude.mempty
