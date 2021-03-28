{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Anomaly
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Anomaly
  ( Anomaly (..)
  -- * Smart constructor
  , mkAnomaly
  -- * Lenses
  , aAnomalyId
  , aAnomalyScore
  , aImpact
  , aMonitorArn
  , aAnomalyEndDate
  , aAnomalyStartDate
  , aDimensionValue
  , aFeedback
  , aRootCauses
  ) where

import qualified Network.AWS.CostExplorer.Types.AnomalyEndDate as Types
import qualified Network.AWS.CostExplorer.Types.AnomalyFeedbackType as Types
import qualified Network.AWS.CostExplorer.Types.AnomalyId as Types
import qualified Network.AWS.CostExplorer.Types.AnomalyScore as Types
import qualified Network.AWS.CostExplorer.Types.AnomalyStartDate as Types
import qualified Network.AWS.CostExplorer.Types.DimensionValue as Types
import qualified Network.AWS.CostExplorer.Types.Impact as Types
import qualified Network.AWS.CostExplorer.Types.MonitorArn as Types
import qualified Network.AWS.CostExplorer.Types.RootCause as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An unusual cost pattern. This consists of the detailed metadata and the current status of the anomaly object. 
--
-- /See:/ 'mkAnomaly' smart constructor.
data Anomaly = Anomaly'
  { anomalyId :: Types.AnomalyId
    -- ^ The unique identifier for the anomaly. 
  , anomalyScore :: Types.AnomalyScore
    -- ^ The latest and maximum score for the anomaly. 
  , impact :: Types.Impact
    -- ^ The dollar impact for the anomaly. 
  , monitorArn :: Types.MonitorArn
    -- ^ The Amazon Resource Name (ARN) for the cost monitor that generated this anomaly. 
  , anomalyEndDate :: Core.Maybe Types.AnomalyEndDate
    -- ^ The last day the anomaly is detected. 
  , anomalyStartDate :: Core.Maybe Types.AnomalyStartDate
    -- ^ The first day the anomaly is detected. 
  , dimensionValue :: Core.Maybe Types.DimensionValue
    -- ^ The dimension for the anomaly. For example, an AWS service in a service monitor. 
  , feedback :: Core.Maybe Types.AnomalyFeedbackType
    -- ^ The feedback value. 
  , rootCauses :: Core.Maybe [Types.RootCause]
    -- ^ The list of identified root causes for the anomaly. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Anomaly' value with any optional fields omitted.
mkAnomaly
    :: Types.AnomalyId -- ^ 'anomalyId'
    -> Types.AnomalyScore -- ^ 'anomalyScore'
    -> Types.Impact -- ^ 'impact'
    -> Types.MonitorArn -- ^ 'monitorArn'
    -> Anomaly
mkAnomaly anomalyId anomalyScore impact monitorArn
  = Anomaly'{anomalyId, anomalyScore, impact, monitorArn,
             anomalyEndDate = Core.Nothing, anomalyStartDate = Core.Nothing,
             dimensionValue = Core.Nothing, feedback = Core.Nothing,
             rootCauses = Core.Nothing}

-- | The unique identifier for the anomaly. 
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyId :: Lens.Lens' Anomaly Types.AnomalyId
aAnomalyId = Lens.field @"anomalyId"
{-# INLINEABLE aAnomalyId #-}
{-# DEPRECATED anomalyId "Use generic-lens or generic-optics with 'anomalyId' instead"  #-}

-- | The latest and maximum score for the anomaly. 
--
-- /Note:/ Consider using 'anomalyScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyScore :: Lens.Lens' Anomaly Types.AnomalyScore
aAnomalyScore = Lens.field @"anomalyScore"
{-# INLINEABLE aAnomalyScore #-}
{-# DEPRECATED anomalyScore "Use generic-lens or generic-optics with 'anomalyScore' instead"  #-}

-- | The dollar impact for the anomaly. 
--
-- /Note:/ Consider using 'impact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aImpact :: Lens.Lens' Anomaly Types.Impact
aImpact = Lens.field @"impact"
{-# INLINEABLE aImpact #-}
{-# DEPRECATED impact "Use generic-lens or generic-optics with 'impact' instead"  #-}

-- | The Amazon Resource Name (ARN) for the cost monitor that generated this anomaly. 
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMonitorArn :: Lens.Lens' Anomaly Types.MonitorArn
aMonitorArn = Lens.field @"monitorArn"
{-# INLINEABLE aMonitorArn #-}
{-# DEPRECATED monitorArn "Use generic-lens or generic-optics with 'monitorArn' instead"  #-}

-- | The last day the anomaly is detected. 
--
-- /Note:/ Consider using 'anomalyEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyEndDate :: Lens.Lens' Anomaly (Core.Maybe Types.AnomalyEndDate)
aAnomalyEndDate = Lens.field @"anomalyEndDate"
{-# INLINEABLE aAnomalyEndDate #-}
{-# DEPRECATED anomalyEndDate "Use generic-lens or generic-optics with 'anomalyEndDate' instead"  #-}

-- | The first day the anomaly is detected. 
--
-- /Note:/ Consider using 'anomalyStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyStartDate :: Lens.Lens' Anomaly (Core.Maybe Types.AnomalyStartDate)
aAnomalyStartDate = Lens.field @"anomalyStartDate"
{-# INLINEABLE aAnomalyStartDate #-}
{-# DEPRECATED anomalyStartDate "Use generic-lens or generic-optics with 'anomalyStartDate' instead"  #-}

-- | The dimension for the anomaly. For example, an AWS service in a service monitor. 
--
-- /Note:/ Consider using 'dimensionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDimensionValue :: Lens.Lens' Anomaly (Core.Maybe Types.DimensionValue)
aDimensionValue = Lens.field @"dimensionValue"
{-# INLINEABLE aDimensionValue #-}
{-# DEPRECATED dimensionValue "Use generic-lens or generic-optics with 'dimensionValue' instead"  #-}

-- | The feedback value. 
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFeedback :: Lens.Lens' Anomaly (Core.Maybe Types.AnomalyFeedbackType)
aFeedback = Lens.field @"feedback"
{-# INLINEABLE aFeedback #-}
{-# DEPRECATED feedback "Use generic-lens or generic-optics with 'feedback' instead"  #-}

-- | The list of identified root causes for the anomaly. 
--
-- /Note:/ Consider using 'rootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRootCauses :: Lens.Lens' Anomaly (Core.Maybe [Types.RootCause])
aRootCauses = Lens.field @"rootCauses"
{-# INLINEABLE aRootCauses #-}
{-# DEPRECATED rootCauses "Use generic-lens or generic-optics with 'rootCauses' instead"  #-}

instance Core.FromJSON Anomaly where
        parseJSON
          = Core.withObject "Anomaly" Core.$
              \ x ->
                Anomaly' Core.<$>
                  (x Core..: "AnomalyId") Core.<*> x Core..: "AnomalyScore" Core.<*>
                    x Core..: "Impact"
                    Core.<*> x Core..: "MonitorArn"
                    Core.<*> x Core..:? "AnomalyEndDate"
                    Core.<*> x Core..:? "AnomalyStartDate"
                    Core.<*> x Core..:? "DimensionValue"
                    Core.<*> x Core..:? "Feedback"
                    Core.<*> x Core..:? "RootCauses"
