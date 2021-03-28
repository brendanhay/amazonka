{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.EnhancedMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.EnhancedMetrics
  ( EnhancedMetrics (..)
  -- * Smart constructor
  , mkEnhancedMetrics
  -- * Lenses
  , emShardLevelMetrics
  ) where

import qualified Network.AWS.Kinesis.Types.MetricsName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents enhanced metrics types.
--
-- /See:/ 'mkEnhancedMetrics' smart constructor.
newtype EnhancedMetrics = EnhancedMetrics'
  { shardLevelMetrics :: Core.Maybe [Types.MetricsName]
    -- ^ List of shard-level metrics.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnhancedMetrics' value with any optional fields omitted.
mkEnhancedMetrics
    :: EnhancedMetrics
mkEnhancedMetrics
  = EnhancedMetrics'{shardLevelMetrics = Core.Nothing}

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
emShardLevelMetrics :: Lens.Lens' EnhancedMetrics (Core.Maybe [Types.MetricsName])
emShardLevelMetrics = Lens.field @"shardLevelMetrics"
{-# INLINEABLE emShardLevelMetrics #-}
{-# DEPRECATED shardLevelMetrics "Use generic-lens or generic-optics with 'shardLevelMetrics' instead"  #-}

instance Core.FromJSON EnhancedMetrics where
        parseJSON
          = Core.withObject "EnhancedMetrics" Core.$
              \ x -> EnhancedMetrics' Core.<$> (x Core..:? "ShardLevelMetrics")
