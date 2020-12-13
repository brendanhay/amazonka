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
    demShardLevelMetrics,
    demStreamName,

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

-- | Represents the input for 'DisableEnhancedMonitoring' .
--
-- /See:/ 'mkDisableEnhancedMonitoring' smart constructor.
data DisableEnhancedMonitoring = DisableEnhancedMonitoring'
  { -- | List of shard-level metrics to disable.
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
    shardLevelMetrics :: [MetricsName],
    -- | The name of the Kinesis data stream for which to disable enhanced monitoring.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableEnhancedMonitoring' with the minimum fields required to make a request.
--
-- * 'shardLevelMetrics' - List of shard-level metrics to disable.
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
-- * 'streamName' - The name of the Kinesis data stream for which to disable enhanced monitoring.
mkDisableEnhancedMonitoring ::
  -- | 'streamName'
  Lude.Text ->
  DisableEnhancedMonitoring
mkDisableEnhancedMonitoring pStreamName_ =
  DisableEnhancedMonitoring'
    { shardLevelMetrics = Lude.mempty,
      streamName = pStreamName_
    }

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
demShardLevelMetrics :: Lens.Lens' DisableEnhancedMonitoring [MetricsName]
demShardLevelMetrics = Lens.lens (shardLevelMetrics :: DisableEnhancedMonitoring -> [MetricsName]) (\s a -> s {shardLevelMetrics = a} :: DisableEnhancedMonitoring)
{-# DEPRECATED demShardLevelMetrics "Use generic-lens or generic-optics with 'shardLevelMetrics' instead." #-}

-- | The name of the Kinesis data stream for which to disable enhanced monitoring.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demStreamName :: Lens.Lens' DisableEnhancedMonitoring Lude.Text
demStreamName = Lens.lens (streamName :: DisableEnhancedMonitoring -> Lude.Text) (\s a -> s {streamName = a} :: DisableEnhancedMonitoring)
{-# DEPRECATED demStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest DisableEnhancedMonitoring where
  type Rs DisableEnhancedMonitoring = EnhancedMonitoringOutput
  request = Req.postJSON kinesisService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DisableEnhancedMonitoring where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.DisableEnhancedMonitoring" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableEnhancedMonitoring where
  toJSON DisableEnhancedMonitoring' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ShardLevelMetrics" Lude..= shardLevelMetrics),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath DisableEnhancedMonitoring where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableEnhancedMonitoring where
  toQuery = Lude.const Lude.mempty
