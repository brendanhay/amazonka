{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAvailablePatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patches eligible to be included in a patch baseline.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAvailablePatches
  ( -- * Creating a request
    DescribeAvailablePatches (..),
    mkDescribeAvailablePatches,

    -- ** Request lenses
    dapFilters,
    dapNextToken,
    dapMaxResults,

    -- * Destructuring the response
    DescribeAvailablePatchesResponse (..),
    mkDescribeAvailablePatchesResponse,

    -- ** Response lenses
    daprsPatches,
    daprsNextToken,
    daprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeAvailablePatches' smart constructor.
data DescribeAvailablePatches = DescribeAvailablePatches'
  { -- | Filters used to scope down the returned patches.
    filters :: Lude.Maybe [PatchOrchestratorFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailablePatches' with the minimum fields required to make a request.
--
-- * 'filters' - Filters used to scope down the returned patches.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of patches to return (per page).
mkDescribeAvailablePatches ::
  DescribeAvailablePatches
mkDescribeAvailablePatches =
  DescribeAvailablePatches'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filters used to scope down the returned patches.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapFilters :: Lens.Lens' DescribeAvailablePatches (Lude.Maybe [PatchOrchestratorFilter])
dapFilters = Lens.lens (filters :: DescribeAvailablePatches -> Lude.Maybe [PatchOrchestratorFilter]) (\s a -> s {filters = a} :: DescribeAvailablePatches)
{-# DEPRECATED dapFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapNextToken :: Lens.Lens' DescribeAvailablePatches (Lude.Maybe Lude.Text)
dapNextToken = Lens.lens (nextToken :: DescribeAvailablePatches -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAvailablePatches)
{-# DEPRECATED dapNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapMaxResults :: Lens.Lens' DescribeAvailablePatches (Lude.Maybe Lude.Natural)
dapMaxResults = Lens.lens (maxResults :: DescribeAvailablePatches -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAvailablePatches)
{-# DEPRECATED dapMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeAvailablePatches where
  page rq rs
    | Page.stop (rs Lens.^. daprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daprsPatches) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dapNextToken Lens..~ rs Lens.^. daprsNextToken

instance Lude.AWSRequest DescribeAvailablePatches where
  type Rs DescribeAvailablePatches = DescribeAvailablePatchesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAvailablePatchesResponse'
            Lude.<$> (x Lude..?> "Patches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAvailablePatches where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeAvailablePatches" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAvailablePatches where
  toJSON DescribeAvailablePatches' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeAvailablePatches where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAvailablePatches where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAvailablePatchesResponse' smart constructor.
data DescribeAvailablePatchesResponse = DescribeAvailablePatchesResponse'
  { -- | An array of patches. Each entry in the array is a patch structure.
    patches :: Lude.Maybe [Patch],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailablePatchesResponse' with the minimum fields required to make a request.
--
-- * 'patches' - An array of patches. Each entry in the array is a patch structure.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeAvailablePatchesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAvailablePatchesResponse
mkDescribeAvailablePatchesResponse pResponseStatus_ =
  DescribeAvailablePatchesResponse'
    { patches = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of patches. Each entry in the array is a patch structure.
--
-- /Note:/ Consider using 'patches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprsPatches :: Lens.Lens' DescribeAvailablePatchesResponse (Lude.Maybe [Patch])
daprsPatches = Lens.lens (patches :: DescribeAvailablePatchesResponse -> Lude.Maybe [Patch]) (\s a -> s {patches = a} :: DescribeAvailablePatchesResponse)
{-# DEPRECATED daprsPatches "Use generic-lens or generic-optics with 'patches' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprsNextToken :: Lens.Lens' DescribeAvailablePatchesResponse (Lude.Maybe Lude.Text)
daprsNextToken = Lens.lens (nextToken :: DescribeAvailablePatchesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAvailablePatchesResponse)
{-# DEPRECATED daprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprsResponseStatus :: Lens.Lens' DescribeAvailablePatchesResponse Lude.Int
daprsResponseStatus = Lens.lens (responseStatus :: DescribeAvailablePatchesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAvailablePatchesResponse)
{-# DEPRECATED daprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
