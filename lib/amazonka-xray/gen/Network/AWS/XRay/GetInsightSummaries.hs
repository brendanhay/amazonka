{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsightSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summaries of all insights in the specified group matching the provided filter values.
module Network.AWS.XRay.GetInsightSummaries
  ( -- * Creating a request
    GetInsightSummaries (..),
    mkGetInsightSummaries,

    -- ** Request lenses
    gisStates,
    gisNextToken,
    gisGroupARN,
    gisGroupName,
    gisMaxResults,
    gisStartTime,
    gisEndTime,

    -- * Destructuring the response
    GetInsightSummariesResponse (..),
    mkGetInsightSummariesResponse,

    -- ** Response lenses
    gisrsInsightSummaries,
    gisrsNextToken,
    gisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetInsightSummaries' smart constructor.
data GetInsightSummaries = GetInsightSummaries'
  { states ::
      Lude.Maybe [InsightState],
    nextToken :: Lude.Maybe Lude.Text,
    groupARN :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    startTime :: Lude.Timestamp,
    endTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsightSummaries' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time frame in which the insights ended. The end time can't be more than 30 days old.
-- * 'groupARN' - The Amazon Resource Name (ARN) of the group. Required if the GroupName isn't provided.
-- * 'groupName' - The name of the group. Required if the GroupARN isn't provided.
-- * 'maxResults' - The maximum number of results to display.
-- * 'nextToken' - Pagination token.
-- * 'startTime' - The beginning of the time frame in which the insights started. The start time can't be more than 30 days old.
-- * 'states' - The list of insight states.
mkGetInsightSummaries ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  GetInsightSummaries
mkGetInsightSummaries pStartTime_ pEndTime_ =
  GetInsightSummaries'
    { states = Lude.Nothing,
      nextToken = Lude.Nothing,
      groupARN = Lude.Nothing,
      groupName = Lude.Nothing,
      maxResults = Lude.Nothing,
      startTime = pStartTime_,
      endTime = pEndTime_
    }

-- | The list of insight states.
--
-- /Note:/ Consider using 'states' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisStates :: Lens.Lens' GetInsightSummaries (Lude.Maybe [InsightState])
gisStates = Lens.lens (states :: GetInsightSummaries -> Lude.Maybe [InsightState]) (\s a -> s {states = a} :: GetInsightSummaries)
{-# DEPRECATED gisStates "Use generic-lens or generic-optics with 'states' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisNextToken :: Lens.Lens' GetInsightSummaries (Lude.Maybe Lude.Text)
gisNextToken = Lens.lens (nextToken :: GetInsightSummaries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInsightSummaries)
{-# DEPRECATED gisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the group. Required if the GroupName isn't provided.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisGroupARN :: Lens.Lens' GetInsightSummaries (Lude.Maybe Lude.Text)
gisGroupARN = Lens.lens (groupARN :: GetInsightSummaries -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: GetInsightSummaries)
{-# DEPRECATED gisGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of the group. Required if the GroupARN isn't provided.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisGroupName :: Lens.Lens' GetInsightSummaries (Lude.Maybe Lude.Text)
gisGroupName = Lens.lens (groupName :: GetInsightSummaries -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GetInsightSummaries)
{-# DEPRECATED gisGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The maximum number of results to display.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisMaxResults :: Lens.Lens' GetInsightSummaries (Lude.Maybe Lude.Natural)
gisMaxResults = Lens.lens (maxResults :: GetInsightSummaries -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetInsightSummaries)
{-# DEPRECATED gisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The beginning of the time frame in which the insights started. The start time can't be more than 30 days old.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisStartTime :: Lens.Lens' GetInsightSummaries Lude.Timestamp
gisStartTime = Lens.lens (startTime :: GetInsightSummaries -> Lude.Timestamp) (\s a -> s {startTime = a} :: GetInsightSummaries)
{-# DEPRECATED gisStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time frame in which the insights ended. The end time can't be more than 30 days old.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisEndTime :: Lens.Lens' GetInsightSummaries Lude.Timestamp
gisEndTime = Lens.lens (endTime :: GetInsightSummaries -> Lude.Timestamp) (\s a -> s {endTime = a} :: GetInsightSummaries)
{-# DEPRECATED gisEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.AWSRequest GetInsightSummaries where
  type Rs GetInsightSummaries = GetInsightSummariesResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInsightSummariesResponse'
            Lude.<$> (x Lude..?> "InsightSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInsightSummaries where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetInsightSummaries where
  toJSON GetInsightSummaries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("States" Lude..=) Lude.<$> states,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("GroupARN" Lude..=) Lude.<$> groupARN,
            ("GroupName" Lude..=) Lude.<$> groupName,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("StartTime" Lude..= startTime),
            Lude.Just ("EndTime" Lude..= endTime)
          ]
      )

instance Lude.ToPath GetInsightSummaries where
  toPath = Lude.const "/InsightSummaries"

instance Lude.ToQuery GetInsightSummaries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInsightSummariesResponse' smart constructor.
data GetInsightSummariesResponse = GetInsightSummariesResponse'
  { insightSummaries ::
      Lude.Maybe [InsightSummary],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsightSummariesResponse' with the minimum fields required to make a request.
--
-- * 'insightSummaries' - The summary of each insight within the group matching the provided filters. The summary contains the InsightID, start and end time, the root cause service, the root cause and client impact statistics, the top anomalous services, and the status of the insight.
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
mkGetInsightSummariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInsightSummariesResponse
mkGetInsightSummariesResponse pResponseStatus_ =
  GetInsightSummariesResponse'
    { insightSummaries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The summary of each insight within the group matching the provided filters. The summary contains the InsightID, start and end time, the root cause service, the root cause and client impact statistics, the top anomalous services, and the status of the insight.
--
-- /Note:/ Consider using 'insightSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsInsightSummaries :: Lens.Lens' GetInsightSummariesResponse (Lude.Maybe [InsightSummary])
gisrsInsightSummaries = Lens.lens (insightSummaries :: GetInsightSummariesResponse -> Lude.Maybe [InsightSummary]) (\s a -> s {insightSummaries = a} :: GetInsightSummariesResponse)
{-# DEPRECATED gisrsInsightSummaries "Use generic-lens or generic-optics with 'insightSummaries' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsNextToken :: Lens.Lens' GetInsightSummariesResponse (Lude.Maybe Lude.Text)
gisrsNextToken = Lens.lens (nextToken :: GetInsightSummariesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInsightSummariesResponse)
{-# DEPRECATED gisrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrsResponseStatus :: Lens.Lens' GetInsightSummariesResponse Lude.Int
gisrsResponseStatus = Lens.lens (responseStatus :: GetInsightSummariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInsightSummariesResponse)
{-# DEPRECATED gisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
