{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsightEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- X-Ray reevaluates insights periodically until they're resolved, and records each intermediate state as an event. You can review an insight's events in the Impact Timeline on the Inspect page in the X-Ray console.
module Network.AWS.XRay.GetInsightEvents
  ( -- * Creating a request
    GetInsightEvents (..),
    mkGetInsightEvents,

    -- ** Request lenses
    gieNextToken,
    gieMaxResults,
    gieInsightId,

    -- * Destructuring the response
    GetInsightEventsResponse (..),
    mkGetInsightEventsResponse,

    -- ** Response lenses
    giersInsightEvents,
    giersNextToken,
    giersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetInsightEvents' smart constructor.
data GetInsightEvents = GetInsightEvents'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    insightId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsightEvents' with the minimum fields required to make a request.
--
-- * 'insightId' - The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
-- * 'maxResults' - Used to retrieve at most the specified value of events.
-- * 'nextToken' - Specify the pagination token returned by a previous request to retrieve the next page of events.
mkGetInsightEvents ::
  -- | 'insightId'
  Lude.Text ->
  GetInsightEvents
mkGetInsightEvents pInsightId_ =
  GetInsightEvents'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      insightId = pInsightId_
    }

-- | Specify the pagination token returned by a previous request to retrieve the next page of events.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gieNextToken :: Lens.Lens' GetInsightEvents (Lude.Maybe Lude.Text)
gieNextToken = Lens.lens (nextToken :: GetInsightEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInsightEvents)
{-# DEPRECATED gieNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Used to retrieve at most the specified value of events.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gieMaxResults :: Lens.Lens' GetInsightEvents (Lude.Maybe Lude.Natural)
gieMaxResults = Lens.lens (maxResults :: GetInsightEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetInsightEvents)
{-# DEPRECATED gieMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gieInsightId :: Lens.Lens' GetInsightEvents Lude.Text
gieInsightId = Lens.lens (insightId :: GetInsightEvents -> Lude.Text) (\s a -> s {insightId = a} :: GetInsightEvents)
{-# DEPRECATED gieInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

instance Lude.AWSRequest GetInsightEvents where
  type Rs GetInsightEvents = GetInsightEventsResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInsightEventsResponse'
            Lude.<$> (x Lude..?> "InsightEvents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInsightEvents where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetInsightEvents where
  toJSON GetInsightEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("InsightId" Lude..= insightId)
          ]
      )

instance Lude.ToPath GetInsightEvents where
  toPath = Lude.const "/InsightEvents"

instance Lude.ToQuery GetInsightEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInsightEventsResponse' smart constructor.
data GetInsightEventsResponse = GetInsightEventsResponse'
  { insightEvents ::
      Lude.Maybe [InsightEvent],
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

-- | Creates a value of 'GetInsightEventsResponse' with the minimum fields required to make a request.
--
-- * 'insightEvents' - A detailed description of the event. This includes the time of the event, client and root cause impact statistics, and the top anomalous service at the time of the event.
-- * 'nextToken' - Use this token to retrieve the next page of insight events.
-- * 'responseStatus' - The response status code.
mkGetInsightEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInsightEventsResponse
mkGetInsightEventsResponse pResponseStatus_ =
  GetInsightEventsResponse'
    { insightEvents = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A detailed description of the event. This includes the time of the event, client and root cause impact statistics, and the top anomalous service at the time of the event.
--
-- /Note:/ Consider using 'insightEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giersInsightEvents :: Lens.Lens' GetInsightEventsResponse (Lude.Maybe [InsightEvent])
giersInsightEvents = Lens.lens (insightEvents :: GetInsightEventsResponse -> Lude.Maybe [InsightEvent]) (\s a -> s {insightEvents = a} :: GetInsightEventsResponse)
{-# DEPRECATED giersInsightEvents "Use generic-lens or generic-optics with 'insightEvents' instead." #-}

-- | Use this token to retrieve the next page of insight events.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giersNextToken :: Lens.Lens' GetInsightEventsResponse (Lude.Maybe Lude.Text)
giersNextToken = Lens.lens (nextToken :: GetInsightEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetInsightEventsResponse)
{-# DEPRECATED giersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giersResponseStatus :: Lens.Lens' GetInsightEventsResponse Lude.Int
giersResponseStatus = Lens.lens (responseStatus :: GetInsightEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInsightEventsResponse)
{-# DEPRECATED giersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
