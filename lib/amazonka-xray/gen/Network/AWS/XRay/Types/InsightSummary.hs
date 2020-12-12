{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightSummary
  ( InsightSummary (..),

    -- * Smart constructor
    mkInsightSummary,

    -- * Lenses
    isSummary,
    isState,
    isStartTime,
    isInsightId,
    isCategories,
    isRootCauseServiceRequestImpactStatistics,
    isTopAnomalousServices,
    isRootCauseServiceId,
    isClientRequestImpactStatistics,
    isEndTime,
    isGroupARN,
    isGroupName,
    isLastUpdateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.AnomalousService
import Network.AWS.XRay.Types.InsightCategory
import Network.AWS.XRay.Types.InsightState
import Network.AWS.XRay.Types.RequestImpactStatistics
import Network.AWS.XRay.Types.ServiceId

-- | Information that describes an insight.
--
-- /See:/ 'mkInsightSummary' smart constructor.
data InsightSummary = InsightSummary'
  { summary ::
      Lude.Maybe Lude.Text,
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
    groupName :: Lude.Maybe Lude.Text,
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightSummary' with the minimum fields required to make a request.
--
-- * 'categories' - Categories The categories that label and describe the type of insight.
-- * 'clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
-- * 'endTime' - The time, in Unix seconds, at which the insight ended.
-- * 'groupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
-- * 'groupName' - The name of the group that the insight belongs to.
-- * 'insightId' - The insights unique identifier.
-- * 'lastUpdateTime' - The time, in Unix seconds, that the insight was last updated.
-- * 'rootCauseServiceId' - Undocumented field.
-- * 'rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
-- * 'startTime' - The time, in Unix seconds, at which the insight began.
-- * 'state' - The current state of the insight.
-- * 'summary' - A brief description of the insight.
-- * 'topAnomalousServices' - The service within the insight that is most impacted by the incident.
mkInsightSummary ::
  InsightSummary
mkInsightSummary =
  InsightSummary'
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
      groupName = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | A brief description of the insight.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSummary :: Lens.Lens' InsightSummary (Lude.Maybe Lude.Text)
isSummary = Lens.lens (summary :: InsightSummary -> Lude.Maybe Lude.Text) (\s a -> s {summary = a} :: InsightSummary)
{-# DEPRECATED isSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The current state of the insight.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isState :: Lens.Lens' InsightSummary (Lude.Maybe InsightState)
isState = Lens.lens (state :: InsightSummary -> Lude.Maybe InsightState) (\s a -> s {state = a} :: InsightSummary)
{-# DEPRECATED isState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time, in Unix seconds, at which the insight began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isStartTime :: Lens.Lens' InsightSummary (Lude.Maybe Lude.Timestamp)
isStartTime = Lens.lens (startTime :: InsightSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: InsightSummary)
{-# DEPRECATED isStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The insights unique identifier.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInsightId :: Lens.Lens' InsightSummary (Lude.Maybe Lude.Text)
isInsightId = Lens.lens (insightId :: InsightSummary -> Lude.Maybe Lude.Text) (\s a -> s {insightId = a} :: InsightSummary)
{-# DEPRECATED isInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

-- | Categories The categories that label and describe the type of insight.
--
-- /Note:/ Consider using 'categories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCategories :: Lens.Lens' InsightSummary (Lude.Maybe [InsightCategory])
isCategories = Lens.lens (categories :: InsightSummary -> Lude.Maybe [InsightCategory]) (\s a -> s {categories = a} :: InsightSummary)
{-# DEPRECATED isCategories "Use generic-lens or generic-optics with 'categories' instead." #-}

-- | The impact statistics of the root cause service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'rootCauseServiceRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isRootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightSummary (Lude.Maybe RequestImpactStatistics)
isRootCauseServiceRequestImpactStatistics = Lens.lens (rootCauseServiceRequestImpactStatistics :: InsightSummary -> Lude.Maybe RequestImpactStatistics) (\s a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightSummary)
{-# DEPRECATED isRootCauseServiceRequestImpactStatistics "Use generic-lens or generic-optics with 'rootCauseServiceRequestImpactStatistics' instead." #-}

-- | The service within the insight that is most impacted by the incident.
--
-- /Note:/ Consider using 'topAnomalousServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTopAnomalousServices :: Lens.Lens' InsightSummary (Lude.Maybe [AnomalousService])
isTopAnomalousServices = Lens.lens (topAnomalousServices :: InsightSummary -> Lude.Maybe [AnomalousService]) (\s a -> s {topAnomalousServices = a} :: InsightSummary)
{-# DEPRECATED isTopAnomalousServices "Use generic-lens or generic-optics with 'topAnomalousServices' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rootCauseServiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isRootCauseServiceId :: Lens.Lens' InsightSummary (Lude.Maybe ServiceId)
isRootCauseServiceId = Lens.lens (rootCauseServiceId :: InsightSummary -> Lude.Maybe ServiceId) (\s a -> s {rootCauseServiceId = a} :: InsightSummary)
{-# DEPRECATED isRootCauseServiceId "Use generic-lens or generic-optics with 'rootCauseServiceId' instead." #-}

-- | The impact statistics of the client side service. This includes the number of requests to the client service and whether the requests were faults or okay.
--
-- /Note:/ Consider using 'clientRequestImpactStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isClientRequestImpactStatistics :: Lens.Lens' InsightSummary (Lude.Maybe RequestImpactStatistics)
isClientRequestImpactStatistics = Lens.lens (clientRequestImpactStatistics :: InsightSummary -> Lude.Maybe RequestImpactStatistics) (\s a -> s {clientRequestImpactStatistics = a} :: InsightSummary)
{-# DEPRECATED isClientRequestImpactStatistics "Use generic-lens or generic-optics with 'clientRequestImpactStatistics' instead." #-}

-- | The time, in Unix seconds, at which the insight ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isEndTime :: Lens.Lens' InsightSummary (Lude.Maybe Lude.Timestamp)
isEndTime = Lens.lens (endTime :: InsightSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: InsightSummary)
{-# DEPRECATED isEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isGroupARN :: Lens.Lens' InsightSummary (Lude.Maybe Lude.Text)
isGroupARN = Lens.lens (groupARN :: InsightSummary -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: InsightSummary)
{-# DEPRECATED isGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of the group that the insight belongs to.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isGroupName :: Lens.Lens' InsightSummary (Lude.Maybe Lude.Text)
isGroupName = Lens.lens (groupName :: InsightSummary -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: InsightSummary)
{-# DEPRECATED isGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The time, in Unix seconds, that the insight was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isLastUpdateTime :: Lens.Lens' InsightSummary (Lude.Maybe Lude.Timestamp)
isLastUpdateTime = Lens.lens (lastUpdateTime :: InsightSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: InsightSummary)
{-# DEPRECATED isLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON InsightSummary where
  parseJSON =
    Lude.withObject
      "InsightSummary"
      ( \x ->
          InsightSummary'
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
            Lude.<*> (x Lude..:? "LastUpdateTime")
      )
