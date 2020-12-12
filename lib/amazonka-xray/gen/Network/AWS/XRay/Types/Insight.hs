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
    iSummary,
    iState,
    iStartTime,
    iInsightId,
    iCategories,
    iRootCauseServiceRequestImpactStatistics,
    iTopAnomalousServices,
    iRootCauseServiceId,
    iClientRequestImpactStatistics,
    iEndTime,
    iGroupARN,
    iGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.InsightCategory
import Network.AWS.XRay.Types.InsightState
import Network.AWS.XRay.Types.RequestImpactStatistics
import Network.AWS.XRay.Types.ServiceId

-- | When fault rates go outside of the expected range, X-Ray creates an insight. Insights tracks emergent issues within your applications.
--
-- /See:/ 'mkInsight' smart constructor.
data Insight = Insight'
  { summary :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe InsightState,
    startTime :: Lude.Maybe Lude.Timestamp,
    insightId :: Lude.Maybe Lude.Text,
    categories :: Lude.Maybe [InsightCategory],
    rootCauseServiceRequestImpactStatistics ::
      Lude.Maybe RequestImpactStatistics,
    topAnomalousServices :: Lude.Maybe [AnomalousService],
    rootCauseServiceId :: Lude.Maybe ServiceId,
    clientRequestImpactStatistics ::
      Lude.Maybe RequestImpactStatistics,
    endTime :: Lude.Maybe Lude.Timestamp,
    groupARN :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Insight' with the minimum fields required to make a request.
--
-- * 'categories' - The categories that label and describe the type of insight.
-- * 'clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
-- * 'endTime' - The time, in Unix seconds, at which the insight ended.
-- * 'groupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
-- * 'groupName' - The name of the group that the insight belongs to.
-- * 'insightId' - The insights unique identifier.
-- * 'rootCauseServiceId' - Undocumented field.
-- * 'rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
-- * 'startTime' - The time, in Unix seconds, at which the insight began.
-- * 'state' - The current state of the insight.
-- * 'summary' - A brief description of the insight.
-- * 'topAnomalousServices' - The service within the insight that is most impacted by the incident.
mkInsight ::
  Insight
mkInsight =
  Insight'
    { summary = Lude.Nothing,
      state = Lude.Nothing,
      startTime = Lude.Nothing,
      insightId = Lude.Nothing,
      categories = Lude.Nothing,
      rootCauseServiceRequestImpactStatistics = Lude.Nothing,
      topAnomalousServices = Lude.Nothing,
      rootCauseServiceId = Lude.Nothing,
      clientRequestImpactStatistics = Lude.Nothing,
      endTime = Lude.Nothing,
      groupARN = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | A brief description of the insight.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSummary :: Lens.Lens' Insight (Lude.Maybe Lude.Text)
iSummary = Lens.lens (summary :: Insight -> Lude.Maybe Lude.Text) (\s a -> s {summary = a} :: Insight)
{-# DEPRECATED iSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The current state of the insight.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Insight (Lude.Maybe InsightState)
iState = Lens.lens (state :: Insight -> Lude.Maybe InsightState) (\s a -> s {state = a} :: Insight)
{-# DEPRECATED iState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time, in Unix seconds, at which the insight began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStartTime :: Lens.Lens' Insight (Lude.Maybe Lude.Timestamp)
iStartTime = Lens.lens (startTime :: Insight -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Insight)
{-# DEPRECATED iStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The insights unique identifier.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInsightId :: Lens.Lens' Insight (Lude.Maybe Lude.Text)
iInsightId = Lens.lens (insightId :: Insight -> Lude.Maybe Lude.Text) (\s a -> s {insightId = a} :: Insight)
{-# DEPRECATED iInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

-- | The categories that label and describe the type of insight.
--
-- /Note:/ Consider using 'categories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCategories :: Lens.Lens' Insight (Lude.Maybe [InsightCategory])
iCategories = Lens.lens (categories :: Insight -> Lude.Maybe [InsightCategory]) (\s a -> s {categories = a} :: Insight)
{-# DEPRECATED iCategories "Use generic-lens or generic-optics with 'categories' instead." #-}

-- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'rootCauseServiceRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootCauseServiceRequestImpactStatistics :: Lens.Lens' Insight (Lude.Maybe RequestImpactStatistics)
iRootCauseServiceRequestImpactStatistics = Lens.lens (rootCauseServiceRequestImpactStatistics :: Insight -> Lude.Maybe RequestImpactStatistics) (\s a -> s {rootCauseServiceRequestImpactStatistics = a} :: Insight)
{-# DEPRECATED iRootCauseServiceRequestImpactStatistics "Use generic-lens or generic-optics with 'rootCauseServiceRequestImpactStatistics' instead." #-}

-- | The service within the insight that is most impacted by the incident.
--
-- /Note:/ Consider using 'topAnomalousServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTopAnomalousServices :: Lens.Lens' Insight (Lude.Maybe [AnomalousService])
iTopAnomalousServices = Lens.lens (topAnomalousServices :: Insight -> Lude.Maybe [AnomalousService]) (\s a -> s {topAnomalousServices = a} :: Insight)
{-# DEPRECATED iTopAnomalousServices "Use generic-lens or generic-optics with 'topAnomalousServices' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rootCauseServiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRootCauseServiceId :: Lens.Lens' Insight (Lude.Maybe ServiceId)
iRootCauseServiceId = Lens.lens (rootCauseServiceId :: Insight -> Lude.Maybe ServiceId) (\s a -> s {rootCauseServiceId = a} :: Insight)
{-# DEPRECATED iRootCauseServiceId "Use generic-lens or generic-optics with 'rootCauseServiceId' instead." #-}

-- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'clientRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iClientRequestImpactStatistics :: Lens.Lens' Insight (Lude.Maybe RequestImpactStatistics)
iClientRequestImpactStatistics = Lens.lens (clientRequestImpactStatistics :: Insight -> Lude.Maybe RequestImpactStatistics) (\s a -> s {clientRequestImpactStatistics = a} :: Insight)
{-# DEPRECATED iClientRequestImpactStatistics "Use generic-lens or generic-optics with 'clientRequestImpactStatistics' instead." #-}

-- | The time, in Unix seconds, at which the insight ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEndTime :: Lens.Lens' Insight (Lude.Maybe Lude.Timestamp)
iEndTime = Lens.lens (endTime :: Insight -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: Insight)
{-# DEPRECATED iEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iGroupARN :: Lens.Lens' Insight (Lude.Maybe Lude.Text)
iGroupARN = Lens.lens (groupARN :: Insight -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: Insight)
{-# DEPRECATED iGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of the group that the insight belongs to.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iGroupName :: Lens.Lens' Insight (Lude.Maybe Lude.Text)
iGroupName = Lens.lens (groupName :: Insight -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: Insight)
{-# DEPRECATED iGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON Insight where
  parseJSON =
    Lude.withObject
      "Insight"
      ( \x ->
          Insight'
            Lude.<$> (x Lude..:? "Summary")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "InsightId")
            Lude.<*> (x Lude..:? "Categories" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RootCauseServiceRequestImpactStatistics")
            Lude.<*> (x Lude..:? "TopAnomalousServices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RootCauseServiceId")
            Lude.<*> (x Lude..:? "ClientRequestImpactStatistics")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "GroupARN")
            Lude.<*> (x Lude..:? "GroupName")
      )
