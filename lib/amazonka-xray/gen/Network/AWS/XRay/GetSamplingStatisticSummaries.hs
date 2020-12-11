{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetSamplingStatisticSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about recent sampling results for all sampling rules.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingStatisticSummaries
  ( -- * Creating a request
    GetSamplingStatisticSummaries (..),
    mkGetSamplingStatisticSummaries,

    -- ** Request lenses
    gsssNextToken,

    -- * Destructuring the response
    GetSamplingStatisticSummariesResponse (..),
    mkGetSamplingStatisticSummariesResponse,

    -- ** Response lenses
    gsssrsSamplingStatisticSummaries,
    gsssrsNextToken,
    gsssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetSamplingStatisticSummaries' smart constructor.
newtype GetSamplingStatisticSummaries = GetSamplingStatisticSummaries'
  { nextToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSamplingStatisticSummaries' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
mkGetSamplingStatisticSummaries ::
  GetSamplingStatisticSummaries
mkGetSamplingStatisticSummaries =
  GetSamplingStatisticSummaries' {nextToken = Lude.Nothing}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssNextToken :: Lens.Lens' GetSamplingStatisticSummaries (Lude.Maybe Lude.Text)
gsssNextToken = Lens.lens (nextToken :: GetSamplingStatisticSummaries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSamplingStatisticSummaries)
{-# DEPRECATED gsssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager GetSamplingStatisticSummaries where
  page rq rs
    | Page.stop (rs Lens.^. gsssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gsssrsSamplingStatisticSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gsssNextToken Lens..~ rs Lens.^. gsssrsNextToken

instance Lude.AWSRequest GetSamplingStatisticSummaries where
  type
    Rs GetSamplingStatisticSummaries =
      GetSamplingStatisticSummariesResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSamplingStatisticSummariesResponse'
            Lude.<$> (x Lude..?> "SamplingStatisticSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSamplingStatisticSummaries where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetSamplingStatisticSummaries where
  toJSON GetSamplingStatisticSummaries' {..} =
    Lude.object
      (Lude.catMaybes [("NextToken" Lude..=) Lude.<$> nextToken])

instance Lude.ToPath GetSamplingStatisticSummaries where
  toPath = Lude.const "/SamplingStatisticSummaries"

instance Lude.ToQuery GetSamplingStatisticSummaries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSamplingStatisticSummariesResponse' smart constructor.
data GetSamplingStatisticSummariesResponse = GetSamplingStatisticSummariesResponse'
  { samplingStatisticSummaries ::
      Lude.Maybe
        [SamplingStatisticSummary],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSamplingStatisticSummariesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
-- * 'samplingStatisticSummaries' - Information about the number of requests instrumented for each sampling rule.
mkGetSamplingStatisticSummariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSamplingStatisticSummariesResponse
mkGetSamplingStatisticSummariesResponse pResponseStatus_ =
  GetSamplingStatisticSummariesResponse'
    { samplingStatisticSummaries =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the number of requests instrumented for each sampling rule.
--
-- /Note:/ Consider using 'samplingStatisticSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssrsSamplingStatisticSummaries :: Lens.Lens' GetSamplingStatisticSummariesResponse (Lude.Maybe [SamplingStatisticSummary])
gsssrsSamplingStatisticSummaries = Lens.lens (samplingStatisticSummaries :: GetSamplingStatisticSummariesResponse -> Lude.Maybe [SamplingStatisticSummary]) (\s a -> s {samplingStatisticSummaries = a} :: GetSamplingStatisticSummariesResponse)
{-# DEPRECATED gsssrsSamplingStatisticSummaries "Use generic-lens or generic-optics with 'samplingStatisticSummaries' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssrsNextToken :: Lens.Lens' GetSamplingStatisticSummariesResponse (Lude.Maybe Lude.Text)
gsssrsNextToken = Lens.lens (nextToken :: GetSamplingStatisticSummariesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSamplingStatisticSummariesResponse)
{-# DEPRECATED gsssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsssrsResponseStatus :: Lens.Lens' GetSamplingStatisticSummariesResponse Lude.Int
gsssrsResponseStatus = Lens.lens (responseStatus :: GetSamplingStatisticSummariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSamplingStatisticSummariesResponse)
{-# DEPRECATED gsssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
