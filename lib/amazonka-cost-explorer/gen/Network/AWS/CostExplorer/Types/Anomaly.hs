{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Anomaly
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Anomaly
  ( Anomaly (..),

    -- * Smart constructor
    mkAnomaly,

    -- * Lenses
    aAnomalyId,
    aAnomalyScore,
    aImpact,
    aMonitorArn,
    aAnomalyEndDate,
    aAnomalyStartDate,
    aDimensionValue,
    aFeedback,
    aRootCauses,
  )
where

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
  { -- | The unique identifier for the anomaly.
    anomalyId :: Types.AnomalyId,
    -- | The latest and maximum score for the anomaly.
    anomalyScore :: Types.AnomalyScore,
    -- | The dollar impact for the anomaly.
    impact :: Types.Impact,
    -- | The Amazon Resource Name (ARN) for the cost monitor that generated this anomaly.
    monitorArn :: Types.MonitorArn,
    -- | The last day the anomaly is detected.
    anomalyEndDate :: Core.Maybe Types.AnomalyEndDate,
    -- | The first day the anomaly is detected.
    anomalyStartDate :: Core.Maybe Types.AnomalyStartDate,
    -- | The dimension for the anomaly. For example, an AWS service in a service monitor.
    dimensionValue :: Core.Maybe Types.DimensionValue,
    -- | The feedback value.
    feedback :: Core.Maybe Types.AnomalyFeedbackType,
    -- | The list of identified root causes for the anomaly.
    rootCauses :: Core.Maybe [Types.RootCause]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Anomaly' value with any optional fields omitted.
mkAnomaly ::
  -- | 'anomalyId'
  Types.AnomalyId ->
  -- | 'anomalyScore'
  Types.AnomalyScore ->
  -- | 'impact'
  Types.Impact ->
  -- | 'monitorArn'
  Types.MonitorArn ->
  Anomaly
mkAnomaly anomalyId anomalyScore impact monitorArn =
  Anomaly'
    { anomalyId,
      anomalyScore,
      impact,
      monitorArn,
      anomalyEndDate = Core.Nothing,
      anomalyStartDate = Core.Nothing,
      dimensionValue = Core.Nothing,
      feedback = Core.Nothing,
      rootCauses = Core.Nothing
    }

-- | The unique identifier for the anomaly.
--
-- /Note:/ Consider using 'anomalyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyId :: Lens.Lens' Anomaly Types.AnomalyId
aAnomalyId = Lens.field @"anomalyId"
{-# DEPRECATED aAnomalyId "Use generic-lens or generic-optics with 'anomalyId' instead." #-}

-- | The latest and maximum score for the anomaly.
--
-- /Note:/ Consider using 'anomalyScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyScore :: Lens.Lens' Anomaly Types.AnomalyScore
aAnomalyScore = Lens.field @"anomalyScore"
{-# DEPRECATED aAnomalyScore "Use generic-lens or generic-optics with 'anomalyScore' instead." #-}

-- | The dollar impact for the anomaly.
--
-- /Note:/ Consider using 'impact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aImpact :: Lens.Lens' Anomaly Types.Impact
aImpact = Lens.field @"impact"
{-# DEPRECATED aImpact "Use generic-lens or generic-optics with 'impact' instead." #-}

-- | The Amazon Resource Name (ARN) for the cost monitor that generated this anomaly.
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMonitorArn :: Lens.Lens' Anomaly Types.MonitorArn
aMonitorArn = Lens.field @"monitorArn"
{-# DEPRECATED aMonitorArn "Use generic-lens or generic-optics with 'monitorArn' instead." #-}

-- | The last day the anomaly is detected.
--
-- /Note:/ Consider using 'anomalyEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyEndDate :: Lens.Lens' Anomaly (Core.Maybe Types.AnomalyEndDate)
aAnomalyEndDate = Lens.field @"anomalyEndDate"
{-# DEPRECATED aAnomalyEndDate "Use generic-lens or generic-optics with 'anomalyEndDate' instead." #-}

-- | The first day the anomaly is detected.
--
-- /Note:/ Consider using 'anomalyStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAnomalyStartDate :: Lens.Lens' Anomaly (Core.Maybe Types.AnomalyStartDate)
aAnomalyStartDate = Lens.field @"anomalyStartDate"
{-# DEPRECATED aAnomalyStartDate "Use generic-lens or generic-optics with 'anomalyStartDate' instead." #-}

-- | The dimension for the anomaly. For example, an AWS service in a service monitor.
--
-- /Note:/ Consider using 'dimensionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDimensionValue :: Lens.Lens' Anomaly (Core.Maybe Types.DimensionValue)
aDimensionValue = Lens.field @"dimensionValue"
{-# DEPRECATED aDimensionValue "Use generic-lens or generic-optics with 'dimensionValue' instead." #-}

-- | The feedback value.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFeedback :: Lens.Lens' Anomaly (Core.Maybe Types.AnomalyFeedbackType)
aFeedback = Lens.field @"feedback"
{-# DEPRECATED aFeedback "Use generic-lens or generic-optics with 'feedback' instead." #-}

-- | The list of identified root causes for the anomaly.
--
-- /Note:/ Consider using 'rootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRootCauses :: Lens.Lens' Anomaly (Core.Maybe [Types.RootCause])
aRootCauses = Lens.field @"rootCauses"
{-# DEPRECATED aRootCauses "Use generic-lens or generic-optics with 'rootCauses' instead." #-}

instance Core.FromJSON Anomaly where
  parseJSON =
    Core.withObject "Anomaly" Core.$
      \x ->
        Anomaly'
          Core.<$> (x Core..: "AnomalyId")
          Core.<*> (x Core..: "AnomalyScore")
          Core.<*> (x Core..: "Impact")
          Core.<*> (x Core..: "MonitorArn")
          Core.<*> (x Core..:? "AnomalyEndDate")
          Core.<*> (x Core..:? "AnomalyStartDate")
          Core.<*> (x Core..:? "DimensionValue")
          Core.<*> (x Core..:? "Feedback")
          Core.<*> (x Core..:? "RootCauses")
