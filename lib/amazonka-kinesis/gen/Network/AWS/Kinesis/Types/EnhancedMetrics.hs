-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.EnhancedMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.EnhancedMetrics
  ( EnhancedMetrics (..),

    -- * Smart constructor
    mkEnhancedMetrics,

    -- * Lenses
    emShardLevelMetrics,
  )
where

import Network.AWS.Kinesis.Types.MetricsName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents enhanced metrics types.
--
-- /See:/ 'mkEnhancedMetrics' smart constructor.
newtype EnhancedMetrics = EnhancedMetrics'
  { shardLevelMetrics ::
      Lude.Maybe [MetricsName]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnhancedMetrics' with the minimum fields required to make a request.
--
-- * 'shardLevelMetrics' - List of shard-level metrics.
--
-- The following are the valid shard-level metrics. The value "@ALL@ " enhances every metric.
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
mkEnhancedMetrics ::
  EnhancedMetrics
mkEnhancedMetrics =
  EnhancedMetrics' {shardLevelMetrics = Lude.Nothing}

-- | List of shard-level metrics.
--
-- The following are the valid shard-level metrics. The value "@ALL@ " enhances every metric.
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
emShardLevelMetrics :: Lens.Lens' EnhancedMetrics (Lude.Maybe [MetricsName])
emShardLevelMetrics = Lens.lens (shardLevelMetrics :: EnhancedMetrics -> Lude.Maybe [MetricsName]) (\s a -> s {shardLevelMetrics = a} :: EnhancedMetrics)
{-# DEPRECATED emShardLevelMetrics "Use generic-lens or generic-optics with 'shardLevelMetrics' instead." #-}

instance Lude.FromJSON EnhancedMetrics where
  parseJSON =
    Lude.withObject
      "EnhancedMetrics"
      ( \x ->
          EnhancedMetrics'
            Lude.<$> (x Lude..:? "ShardLevelMetrics" Lude..!= Lude.mempty)
      )
