{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patch groups that have been registered with patch baselines.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchGroups
  ( -- * Creating a request
    DescribePatchGroups (..),
    mkDescribePatchGroups,

    -- ** Request lenses
    dpgFilters,
    dpgNextToken,
    dpgMaxResults,

    -- * Destructuring the response
    DescribePatchGroupsResponse (..),
    mkDescribePatchGroupsResponse,

    -- ** Response lenses
    dpgrsMappings,
    dpgrsNextToken,
    dpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribePatchGroups' smart constructor.
data DescribePatchGroups = DescribePatchGroups'
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

-- | Creates a value of 'DescribePatchGroups' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. Use a filter to return a more specific list of results.
--
-- For @DescribePatchGroups@ ,valid filter keys include the following:
--
--     * @NAME_PREFIX@ : The name of the patch group. Wildcards (*) are accepted.
--
--
--     * @OPERATING_SYSTEM@ : The supported operating system type to return results for. For valid operating system values, see 'GetDefaultPatchBaselineRequest$OperatingSystem' in 'CreatePatchBaseline' .
-- Examples:
--
--     * @--filters Key=NAME_PREFIX,Values=MyPatchGroup*@
--
--
--     * @--filters Key=OPERATING_SYSTEM,Values=AMAZON_LINUX_2@
--
--
--
--
-- * 'maxResults' - The maximum number of patch groups to return (per page).
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribePatchGroups ::
  DescribePatchGroups
mkDescribePatchGroups =
  DescribePatchGroups'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- For @DescribePatchGroups@ ,valid filter keys include the following:
--
--     * @NAME_PREFIX@ : The name of the patch group. Wildcards (*) are accepted.
--
--
--     * @OPERATING_SYSTEM@ : The supported operating system type to return results for. For valid operating system values, see 'GetDefaultPatchBaselineRequest$OperatingSystem' in 'CreatePatchBaseline' .
-- Examples:
--
--     * @--filters Key=NAME_PREFIX,Values=MyPatchGroup*@
--
--
--     * @--filters Key=OPERATING_SYSTEM,Values=AMAZON_LINUX_2@
--
--
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgFilters :: Lens.Lens' DescribePatchGroups (Lude.Maybe [PatchOrchestratorFilter])
dpgFilters = Lens.lens (filters :: DescribePatchGroups -> Lude.Maybe [PatchOrchestratorFilter]) (\s a -> s {filters = a} :: DescribePatchGroups)
{-# DEPRECATED dpgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgNextToken :: Lens.Lens' DescribePatchGroups (Lude.Maybe Lude.Text)
dpgNextToken = Lens.lens (nextToken :: DescribePatchGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePatchGroups)
{-# DEPRECATED dpgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of patch groups to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgMaxResults :: Lens.Lens' DescribePatchGroups (Lude.Maybe Lude.Natural)
dpgMaxResults = Lens.lens (maxResults :: DescribePatchGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribePatchGroups)
{-# DEPRECATED dpgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribePatchGroups where
  page rq rs
    | Page.stop (rs Lens.^. dpgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpgrsMappings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpgNextToken Lens..~ rs Lens.^. dpgrsNextToken

instance Lude.AWSRequest DescribePatchGroups where
  type Rs DescribePatchGroups = DescribePatchGroupsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePatchGroupsResponse'
            Lude.<$> (x Lude..?> "Mappings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePatchGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribePatchGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePatchGroups where
  toJSON DescribePatchGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribePatchGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePatchGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePatchGroupsResponse' smart constructor.
data DescribePatchGroupsResponse = DescribePatchGroupsResponse'
  { mappings ::
      Lude.Maybe
        [PatchGroupPatchBaselineMapping],
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

-- | Creates a value of 'DescribePatchGroupsResponse' with the minimum fields required to make a request.
--
-- * 'mappings' - Each entry in the array contains:
--
-- PatchGroup: string (between 1 and 256 characters, Regex: ^([\p{L}\p{Z}\p{N}_.:/=+\-@]*)$)
-- PatchBaselineIdentity: A PatchBaselineIdentity element.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribePatchGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePatchGroupsResponse
mkDescribePatchGroupsResponse pResponseStatus_ =
  DescribePatchGroupsResponse'
    { mappings = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Each entry in the array contains:
--
-- PatchGroup: string (between 1 and 256 characters, Regex: ^([\p{L}\p{Z}\p{N}_.:/=+\-@]*)$)
-- PatchBaselineIdentity: A PatchBaselineIdentity element.
--
-- /Note:/ Consider using 'mappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsMappings :: Lens.Lens' DescribePatchGroupsResponse (Lude.Maybe [PatchGroupPatchBaselineMapping])
dpgrsMappings = Lens.lens (mappings :: DescribePatchGroupsResponse -> Lude.Maybe [PatchGroupPatchBaselineMapping]) (\s a -> s {mappings = a} :: DescribePatchGroupsResponse)
{-# DEPRECATED dpgrsMappings "Use generic-lens or generic-optics with 'mappings' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsNextToken :: Lens.Lens' DescribePatchGroupsResponse (Lude.Maybe Lude.Text)
dpgrsNextToken = Lens.lens (nextToken :: DescribePatchGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePatchGroupsResponse)
{-# DEPRECATED dpgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsResponseStatus :: Lens.Lens' DescribePatchGroupsResponse Lude.Int
dpgrsResponseStatus = Lens.lens (responseStatus :: DescribePatchGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePatchGroupsResponse)
{-# DEPRECATED dpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
