{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
  ( EnhancedMonitoringOutput (..)
  -- * Smart constructor
  , mkEnhancedMonitoringOutput
  -- * Lenses
  , emoCurrentShardLevelMetrics
  , emoDesiredShardLevelMetrics
  , emoStreamName
  ) where

import qualified Network.AWS.Kinesis.Types.MetricsName as Types
import qualified Network.AWS.Kinesis.Types.StreamName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output for 'EnableEnhancedMonitoring' and 'DisableEnhancedMonitoring' .
--
-- /See:/ 'mkEnhancedMonitoringOutput' smart constructor.
data EnhancedMonitoringOutput = EnhancedMonitoringOutput'
  { currentShardLevelMetrics :: Core.Maybe [Types.MetricsName]
    -- ^ Represents the current state of the metrics that are in the enhanced state before the operation.
  , desiredShardLevelMetrics :: Core.Maybe [Types.MetricsName]
    -- ^ Represents the list of all the metrics that would be in the enhanced state after the operation.
  , streamName :: Core.Maybe Types.StreamName
    -- ^ The name of the Kinesis data stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnhancedMonitoringOutput' value with any optional fields omitted.
mkEnhancedMonitoringOutput
    :: EnhancedMonitoringOutput
mkEnhancedMonitoringOutput
  = EnhancedMonitoringOutput'{currentShardLevelMetrics =
                                Core.Nothing,
                              desiredShardLevelMetrics = Core.Nothing, streamName = Core.Nothing}

-- | Represents the current state of the metrics that are in the enhanced state before the operation.
--
-- /Note:/ Consider using 'currentShardLevelMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emoCurrentShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Core.Maybe [Types.MetricsName])
emoCurrentShardLevelMetrics = Lens.field @"currentShardLevelMetrics"
{-# INLINEABLE emoCurrentShardLevelMetrics #-}
{-# DEPRECATED currentShardLevelMetrics "Use generic-lens or generic-optics with 'currentShardLevelMetrics' instead"  #-}

-- | Represents the list of all the metrics that would be in the enhanced state after the operation.
--
-- /Note:/ Consider using 'desiredShardLevelMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emoDesiredShardLevelMetrics :: Lens.Lens' EnhancedMonitoringOutput (Core.Maybe [Types.MetricsName])
emoDesiredShardLevelMetrics = Lens.field @"desiredShardLevelMetrics"
{-# INLINEABLE emoDesiredShardLevelMetrics #-}
{-# DEPRECATED desiredShardLevelMetrics "Use generic-lens or generic-optics with 'desiredShardLevelMetrics' instead"  #-}

-- | The name of the Kinesis data stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emoStreamName :: Lens.Lens' EnhancedMonitoringOutput (Core.Maybe Types.StreamName)
emoStreamName = Lens.field @"streamName"
{-# INLINEABLE emoStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.FromJSON EnhancedMonitoringOutput where
        parseJSON
          = Core.withObject "EnhancedMonitoringOutput" Core.$
              \ x ->
                EnhancedMonitoringOutput' Core.<$>
                  (x Core..:? "CurrentShardLevelMetrics") Core.<*>
                    x Core..:? "DesiredShardLevelMetrics"
                    Core.<*> x Core..:? "StreamName"
