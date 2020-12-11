{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchBaselines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the patch baselines in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchBaselines
  ( -- * Creating a request
    DescribePatchBaselines (..),
    mkDescribePatchBaselines,

    -- ** Request lenses
    dpbFilters,
    dpbNextToken,
    dpbMaxResults,

    -- * Destructuring the response
    DescribePatchBaselinesResponse (..),
    mkDescribePatchBaselinesResponse,

    -- ** Response lenses
    dpbsrsBaselineIdentities,
    dpbsrsNextToken,
    dpbsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribePatchBaselines' smart constructor.
data DescribePatchBaselines = DescribePatchBaselines'
  { filters ::
      Lude.Maybe [PatchOrchestratorFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePatchBaselines' with the minimum fields required to make a request.
--
-- * 'filters' - Each element in the array is a structure containing:
--
-- Key: (string, "NAME_PREFIX" or "OWNER")
-- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
-- * 'maxResults' - The maximum number of patch baselines to return (per page).
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribePatchBaselines ::
  DescribePatchBaselines
mkDescribePatchBaselines =
  DescribePatchBaselines'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Each element in the array is a structure containing:
--
-- Key: (string, "NAME_PREFIX" or "OWNER")
-- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbFilters :: Lens.Lens' DescribePatchBaselines (Lude.Maybe [PatchOrchestratorFilter])
dpbFilters = Lens.lens (filters :: DescribePatchBaselines -> Lude.Maybe [PatchOrchestratorFilter]) (\s a -> s {filters = a} :: DescribePatchBaselines)
{-# DEPRECATED dpbFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbNextToken :: Lens.Lens' DescribePatchBaselines (Lude.Maybe Lude.Text)
dpbNextToken = Lens.lens (nextToken :: DescribePatchBaselines -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePatchBaselines)
{-# DEPRECATED dpbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of patch baselines to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbMaxResults :: Lens.Lens' DescribePatchBaselines (Lude.Maybe Lude.Natural)
dpbMaxResults = Lens.lens (maxResults :: DescribePatchBaselines -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribePatchBaselines)
{-# DEPRECATED dpbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribePatchBaselines where
  page rq rs
    | Page.stop (rs Lens.^. dpbsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpbsrsBaselineIdentities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpbNextToken Lens..~ rs Lens.^. dpbsrsNextToken

instance Lude.AWSRequest DescribePatchBaselines where
  type Rs DescribePatchBaselines = DescribePatchBaselinesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePatchBaselinesResponse'
            Lude.<$> (x Lude..?> "BaselineIdentities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePatchBaselines where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribePatchBaselines" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePatchBaselines where
  toJSON DescribePatchBaselines' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribePatchBaselines where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePatchBaselines where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePatchBaselinesResponse' smart constructor.
data DescribePatchBaselinesResponse = DescribePatchBaselinesResponse'
  { baselineIdentities ::
      Lude.Maybe
        [PatchBaselineIdentity],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribePatchBaselinesResponse' with the minimum fields required to make a request.
--
-- * 'baselineIdentities' - An array of PatchBaselineIdentity elements.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribePatchBaselinesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePatchBaselinesResponse
mkDescribePatchBaselinesResponse pResponseStatus_ =
  DescribePatchBaselinesResponse'
    { baselineIdentities =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of PatchBaselineIdentity elements.
--
-- /Note:/ Consider using 'baselineIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbsrsBaselineIdentities :: Lens.Lens' DescribePatchBaselinesResponse (Lude.Maybe [PatchBaselineIdentity])
dpbsrsBaselineIdentities = Lens.lens (baselineIdentities :: DescribePatchBaselinesResponse -> Lude.Maybe [PatchBaselineIdentity]) (\s a -> s {baselineIdentities = a} :: DescribePatchBaselinesResponse)
{-# DEPRECATED dpbsrsBaselineIdentities "Use generic-lens or generic-optics with 'baselineIdentities' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbsrsNextToken :: Lens.Lens' DescribePatchBaselinesResponse (Lude.Maybe Lude.Text)
dpbsrsNextToken = Lens.lens (nextToken :: DescribePatchBaselinesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePatchBaselinesResponse)
{-# DEPRECATED dpbsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbsrsResponseStatus :: Lens.Lens' DescribePatchBaselinesResponse Lude.Int
dpbsrsResponseStatus = Lens.lens (responseStatus :: DescribePatchBaselinesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePatchBaselinesResponse)
{-# DEPRECATED dpbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
