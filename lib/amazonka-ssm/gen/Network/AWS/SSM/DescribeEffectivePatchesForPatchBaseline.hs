{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current effective patches (the patch and the approval state) for the specified patch baseline. Note that this API applies only to Windows patch baselines.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
  ( -- * Creating a request
    DescribeEffectivePatchesForPatchBaseline (..),
    mkDescribeEffectivePatchesForPatchBaseline,

    -- ** Request lenses
    depfpbNextToken,
    depfpbBaselineId,
    depfpbMaxResults,

    -- * Destructuring the response
    DescribeEffectivePatchesForPatchBaselineResponse (..),
    mkDescribeEffectivePatchesForPatchBaselineResponse,

    -- ** Response lenses
    depfpbrsEffectivePatches,
    depfpbrsNextToken,
    depfpbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeEffectivePatchesForPatchBaseline' smart constructor.
data DescribeEffectivePatchesForPatchBaseline = DescribeEffectivePatchesForPatchBaseline'
  { -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the patch baseline to retrieve the effective patches for.
    baselineId :: Lude.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEffectivePatchesForPatchBaseline' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'baselineId' - The ID of the patch baseline to retrieve the effective patches for.
-- * 'maxResults' - The maximum number of patches to return (per page).
mkDescribeEffectivePatchesForPatchBaseline ::
  -- | 'baselineId'
  Lude.Text ->
  DescribeEffectivePatchesForPatchBaseline
mkDescribeEffectivePatchesForPatchBaseline pBaselineId_ =
  DescribeEffectivePatchesForPatchBaseline'
    { nextToken =
        Lude.Nothing,
      baselineId = pBaselineId_,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbNextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Lude.Maybe Lude.Text)
depfpbNextToken = Lens.lens (nextToken :: DescribeEffectivePatchesForPatchBaseline -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEffectivePatchesForPatchBaseline)
{-# DEPRECATED depfpbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the patch baseline to retrieve the effective patches for.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbBaselineId :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline Lude.Text
depfpbBaselineId = Lens.lens (baselineId :: DescribeEffectivePatchesForPatchBaseline -> Lude.Text) (\s a -> s {baselineId = a} :: DescribeEffectivePatchesForPatchBaseline)
{-# DEPRECATED depfpbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbMaxResults :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Lude.Maybe Lude.Natural)
depfpbMaxResults = Lens.lens (maxResults :: DescribeEffectivePatchesForPatchBaseline -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeEffectivePatchesForPatchBaseline)
{-# DEPRECATED depfpbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeEffectivePatchesForPatchBaseline where
  page rq rs
    | Page.stop (rs Lens.^. depfpbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. depfpbrsEffectivePatches) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& depfpbNextToken Lens..~ rs Lens.^. depfpbrsNextToken

instance Lude.AWSRequest DescribeEffectivePatchesForPatchBaseline where
  type
    Rs DescribeEffectivePatchesForPatchBaseline =
      DescribeEffectivePatchesForPatchBaselineResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEffectivePatchesForPatchBaselineResponse'
            Lude.<$> (x Lude..?> "EffectivePatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEffectivePatchesForPatchBaseline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeEffectivePatchesForPatchBaseline" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEffectivePatchesForPatchBaseline where
  toJSON DescribeEffectivePatchesForPatchBaseline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("BaselineId" Lude..= baselineId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeEffectivePatchesForPatchBaseline where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEffectivePatchesForPatchBaseline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEffectivePatchesForPatchBaselineResponse' smart constructor.
data DescribeEffectivePatchesForPatchBaselineResponse = DescribeEffectivePatchesForPatchBaselineResponse'
  { -- | An array of patches and patch status.
    effectivePatches :: Lude.Maybe [EffectivePatch],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEffectivePatchesForPatchBaselineResponse' with the minimum fields required to make a request.
--
-- * 'effectivePatches' - An array of patches and patch status.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeEffectivePatchesForPatchBaselineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEffectivePatchesForPatchBaselineResponse
mkDescribeEffectivePatchesForPatchBaselineResponse pResponseStatus_ =
  DescribeEffectivePatchesForPatchBaselineResponse'
    { effectivePatches =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of patches and patch status.
--
-- /Note:/ Consider using 'effectivePatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbrsEffectivePatches :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Lude.Maybe [EffectivePatch])
depfpbrsEffectivePatches = Lens.lens (effectivePatches :: DescribeEffectivePatchesForPatchBaselineResponse -> Lude.Maybe [EffectivePatch]) (\s a -> s {effectivePatches = a} :: DescribeEffectivePatchesForPatchBaselineResponse)
{-# DEPRECATED depfpbrsEffectivePatches "Use generic-lens or generic-optics with 'effectivePatches' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbrsNextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Lude.Maybe Lude.Text)
depfpbrsNextToken = Lens.lens (nextToken :: DescribeEffectivePatchesForPatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEffectivePatchesForPatchBaselineResponse)
{-# DEPRECATED depfpbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbrsResponseStatus :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse Lude.Int
depfpbrsResponseStatus = Lens.lens (responseStatus :: DescribeEffectivePatchesForPatchBaselineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEffectivePatchesForPatchBaselineResponse)
{-# DEPRECATED depfpbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
