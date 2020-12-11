{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsight
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summary information of an insight. This includes impact to clients and root cause services, the top anomalous services, the category, the state of the insight, and the start and end time of the insight.
module Network.AWS.XRay.GetInsight
  ( -- * Creating a request
    GetInsight (..),
    mkGetInsight,

    -- ** Request lenses
    giInsightId,

    -- * Destructuring the response
    GetInsightResponse (..),
    mkGetInsightResponse,

    -- ** Response lenses
    girsInsight,
    girsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetInsight' smart constructor.
newtype GetInsight = GetInsight' {insightId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsight' with the minimum fields required to make a request.
--
-- * 'insightId' - The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
mkGetInsight ::
  -- | 'insightId'
  Lude.Text ->
  GetInsight
mkGetInsight pInsightId_ = GetInsight' {insightId = pInsightId_}

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giInsightId :: Lens.Lens' GetInsight Lude.Text
giInsightId = Lens.lens (insightId :: GetInsight -> Lude.Text) (\s a -> s {insightId = a} :: GetInsight)
{-# DEPRECATED giInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

instance Lude.AWSRequest GetInsight where
  type Rs GetInsight = GetInsightResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInsightResponse'
            Lude.<$> (x Lude..?> "Insight") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInsight where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetInsight where
  toJSON GetInsight' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("InsightId" Lude..= insightId)])

instance Lude.ToPath GetInsight where
  toPath = Lude.const "/Insight"

instance Lude.ToQuery GetInsight where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInsightResponse' smart constructor.
data GetInsightResponse = GetInsightResponse'
  { insight ::
      Lude.Maybe Insight,
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

-- | Creates a value of 'GetInsightResponse' with the minimum fields required to make a request.
--
-- * 'insight' - The summary information of an insight.
-- * 'responseStatus' - The response status code.
mkGetInsightResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInsightResponse
mkGetInsightResponse pResponseStatus_ =
  GetInsightResponse'
    { insight = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The summary information of an insight.
--
-- /Note:/ Consider using 'insight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsInsight :: Lens.Lens' GetInsightResponse (Lude.Maybe Insight)
girsInsight = Lens.lens (insight :: GetInsightResponse -> Lude.Maybe Insight) (\s a -> s {insight = a} :: GetInsightResponse)
{-# DEPRECATED girsInsight "Use generic-lens or generic-optics with 'insight' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetInsightResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetInsightResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInsightResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
