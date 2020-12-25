{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Insight
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Insight
  ( Insight (..),

    -- * Smart constructor
    mkInsight,

    -- * Lenses
    iCategories,
    iClientRequestImpactStatistics,
    iEndTime,
    iGroupARN,
    iGroupName,
    iInsightId,
    iRootCauseServiceId,
    iRootCauseServiceRequestImpactStatistics,
    iStartTime,
    iState,
    iSummary,
    iTopAnomalousServices,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.AnomalousService as Types
import qualified Network.AWS.XRay.Types.GroupARN as Types
import qualified Network.AWS.XRay.Types.GroupName as Types
import qualified Network.AWS.XRay.Types.InsightCategory as Types
import qualified Network.AWS.XRay.Types.InsightId as Types
import qualified Network.AWS.XRay.Types.InsightState as Types
import qualified Network.AWS.XRay.Types.RequestImpactStatistics as Types
import qualified Network.AWS.XRay.Types.ServiceId as Types
import qualified Network.AWS.XRay.Types.Summary as Types

-- | When fault rates go outside of the expected range, X-Ray creates an insight. Insights tracks emergent issues within your applications.
--
-- /See:/ 'mkInsight' smart constructor.
data Insight = Insight'
  { -- | The categories that label and describe the type of insight.
    categories :: Core.Maybe [Types.InsightCategory],
    -- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
    clientRequestImpactStatistics :: Core.Maybe Types.RequestImpactStatistics,
    -- | The time, in Unix seconds, at which the insight ended.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
    groupARN :: Core.Maybe Types.GroupARN,
    -- | The name of the group that the insight belongs to.
    groupName :: Core.Maybe Types.GroupName,
    -- | The insights unique identifier.
    insightId :: Core.Maybe Types.InsightId,
    rootCauseServiceId :: Core.Maybe Types.ServiceId,
    -- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
    rootCauseServiceRequestImpactStatistics :: Core.Maybe Types.RequestImpactStatistics,
    -- | The time, in Unix seconds, at which the insight began.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The current state of the insight.
    state :: Core.Maybe Types.InsightState,
    -- | A brief description of the insight.
    summary :: Core.Maybe Types.Summary,
    -- | The service within the insight that is most impacted by the incident.
    topAnomalousServices :: Core.Maybe [Types.AnomalousService]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Insight' value with any optional fields omitted.
mkInsight ::
  Insight
mkInsight =
  Insight'
    { categories = Core.Nothing,
      clientRequestImpactStatistics = Core.Nothing,
      endTime = Core.Nothing,
      groupARN = Core.Nothing,
      groupName = Core.Nothing,
      insightId = Core.Nothing,
      rootCauseServiceId = Core.Nothing,
      rootCauseServiceRequestImpactStatistics = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      summary = Core.Nothing,
      topAnomalousServices = Core.Nothing
    }

-- | The categories that label and describe the type of insight.
--
-- /Note:/ Consider using 'categories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCategories :: Lens.Lens' Insight (Core.Maybe [Types.InsightCategory])
iCategories = Lens.field @"categories"
{-# DEPRECATED iCategories "Use generic-lens or generic-optics with 'categories' instead." #-}

-- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'clientRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iClientRequestImpactStatistics :: Lens.Lens' Insight (Core.Maybe Types.RequestImpactStatistics)
iClientRequestImpactStatistics = Lens.field @"clientRequestImpactStatistics"
{-# DEPRECATED iClientRequestImpactStatistics "Use generic-lens or generic-optics with 'clientRequestImpactStatistics' instead." #-}

-- | The time, in Unix seconds, at which the insight ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEndTime :: Lens.Lens' Insight (Core.Maybe Core.NominalDiffTime)
iEndTime = Lens.field @"endTime"
{-# DEPRECATED iEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iGroupARN :: Lens.Lens' Insight (Core.Maybe Types.GroupARN)
iGroupARN = Lens.field @"groupARN"
{-# DEPRECATED iGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of the group that the insight belongs to.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iGroupName :: Lens.Lens' Insight (Core.Maybe Types.GroupName)
iGroupName = Lens.field @"groupName"
{-# DEPRECATED iGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The insights unique identifier.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInsightId :: Lens.Lens' Insight (Core.Maybe Types.InsightId)
iInsightId = Lens.field @"insightId"
{-# DEPRECATED iInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rootCauseServiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootCauseServiceId :: Lens.Lens' Insight (Core.Maybe Types.ServiceId)
iRootCauseServiceId = Lens.field @"rootCauseServiceId"
{-# DEPRECATED iRootCauseServiceId "Use generic-lens or generic-optics with 'rootCauseServiceId' instead." #-}

-- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'rootCauseServiceRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootCauseServiceRequestImpactStatistics :: Lens.Lens' Insight (Core.Maybe Types.RequestImpactStatistics)
iRootCauseServiceRequestImpactStatistics = Lens.field @"rootCauseServiceRequestImpactStatistics"
{-# DEPRECATED iRootCauseServiceRequestImpactStatistics "Use generic-lens or generic-optics with 'rootCauseServiceRequestImpactStatistics' instead." #-}

-- | The time, in Unix seconds, at which the insight began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStartTime :: Lens.Lens' Insight (Core.Maybe Core.NominalDiffTime)
iStartTime = Lens.field @"startTime"
{-# DEPRECATED iStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The current state of the insight.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Insight (Core.Maybe Types.InsightState)
iState = Lens.field @"state"
{-# DEPRECATED iState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A brief description of the insight.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSummary :: Lens.Lens' Insight (Core.Maybe Types.Summary)
iSummary = Lens.field @"summary"
{-# DEPRECATED iSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The service within the insight that is most impacted by the incident.
--
-- /Note:/ Consider using 'topAnomalousServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTopAnomalousServices :: Lens.Lens' Insight (Core.Maybe [Types.AnomalousService])
iTopAnomalousServices = Lens.field @"topAnomalousServices"
{-# DEPRECATED iTopAnomalousServices "Use generic-lens or generic-optics with 'topAnomalousServices' instead." #-}

instance Core.FromJSON Insight where
  parseJSON =
    Core.withObject "Insight" Core.$
      \x ->
        Insight'
          Core.<$> (x Core..:? "Categories")
          Core.<*> (x Core..:? "ClientRequestImpactStatistics")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "GroupARN")
          Core.<*> (x Core..:? "GroupName")
          Core.<*> (x Core..:? "InsightId")
          Core.<*> (x Core..:? "RootCauseServiceId")
          Core.<*> (x Core..:? "RootCauseServiceRequestImpactStatistics")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "Summary")
          Core.<*> (x Core..:? "TopAnomalousServices")
