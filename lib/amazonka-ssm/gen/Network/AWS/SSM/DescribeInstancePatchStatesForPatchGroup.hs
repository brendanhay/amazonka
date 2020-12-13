{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state for the instances in the specified patch group.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
  ( -- * Creating a request
    DescribeInstancePatchStatesForPatchGroup (..),
    mkDescribeInstancePatchStatesForPatchGroup,

    -- ** Request lenses
    dipsfpgFilters,
    dipsfpgNextToken,
    dipsfpgMaxResults,
    dipsfpgPatchGroup,

    -- * Destructuring the response
    DescribeInstancePatchStatesForPatchGroupResponse (..),
    mkDescribeInstancePatchStatesForPatchGroupResponse,

    -- ** Response lenses
    dipsfpgrsNextToken,
    dipsfpgrsInstancePatchStates,
    dipsfpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeInstancePatchStatesForPatchGroup' smart constructor.
data DescribeInstancePatchStatesForPatchGroup = DescribeInstancePatchStatesForPatchGroup'
  { -- | Each entry in the array is a structure containing:
    --
    -- Key (string between 1 and 200 characters)
    -- Values (array containing a single string)
    -- Type (string "Equal", "NotEqual", "LessThan", "GreaterThan")
    filters :: Lude.Maybe [InstancePatchStateFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The name of the patch group for which the patch state information should be retrieved.
    patchGroup :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancePatchStatesForPatchGroup' with the minimum fields required to make a request.
--
-- * 'filters' - Each entry in the array is a structure containing:
--
-- Key (string between 1 and 200 characters)
-- Values (array containing a single string)
-- Type (string "Equal", "NotEqual", "LessThan", "GreaterThan")
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of patches to return (per page).
-- * 'patchGroup' - The name of the patch group for which the patch state information should be retrieved.
mkDescribeInstancePatchStatesForPatchGroup ::
  -- | 'patchGroup'
  Lude.Text ->
  DescribeInstancePatchStatesForPatchGroup
mkDescribeInstancePatchStatesForPatchGroup pPatchGroup_ =
  DescribeInstancePatchStatesForPatchGroup'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      patchGroup = pPatchGroup_
    }

-- | Each entry in the array is a structure containing:
--
-- Key (string between 1 and 200 characters)
-- Values (array containing a single string)
-- Type (string "Equal", "NotEqual", "LessThan", "GreaterThan")
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgFilters :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Lude.Maybe [InstancePatchStateFilter])
dipsfpgFilters = Lens.lens (filters :: DescribeInstancePatchStatesForPatchGroup -> Lude.Maybe [InstancePatchStateFilter]) (\s a -> s {filters = a} :: DescribeInstancePatchStatesForPatchGroup)
{-# DEPRECATED dipsfpgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgNextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Lude.Maybe Lude.Text)
dipsfpgNextToken = Lens.lens (nextToken :: DescribeInstancePatchStatesForPatchGroup -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancePatchStatesForPatchGroup)
{-# DEPRECATED dipsfpgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgMaxResults :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Lude.Maybe Lude.Natural)
dipsfpgMaxResults = Lens.lens (maxResults :: DescribeInstancePatchStatesForPatchGroup -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstancePatchStatesForPatchGroup)
{-# DEPRECATED dipsfpgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the patch group for which the patch state information should be retrieved.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgPatchGroup :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup Lude.Text
dipsfpgPatchGroup = Lens.lens (patchGroup :: DescribeInstancePatchStatesForPatchGroup -> Lude.Text) (\s a -> s {patchGroup = a} :: DescribeInstancePatchStatesForPatchGroup)
{-# DEPRECATED dipsfpgPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Page.AWSPager DescribeInstancePatchStatesForPatchGroup where
  page rq rs
    | Page.stop (rs Lens.^. dipsfpgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dipsfpgrsInstancePatchStates) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dipsfpgNextToken Lens..~ rs Lens.^. dipsfpgrsNextToken

instance Lude.AWSRequest DescribeInstancePatchStatesForPatchGroup where
  type
    Rs DescribeInstancePatchStatesForPatchGroup =
      DescribeInstancePatchStatesForPatchGroupResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesForPatchGroupResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "InstancePatchStates")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstancePatchStatesForPatchGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeInstancePatchStatesForPatchGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInstancePatchStatesForPatchGroup where
  toJSON DescribeInstancePatchStatesForPatchGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("PatchGroup" Lude..= patchGroup)
          ]
      )

instance Lude.ToPath DescribeInstancePatchStatesForPatchGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstancePatchStatesForPatchGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInstancePatchStatesForPatchGroupResponse' smart constructor.
data DescribeInstancePatchStatesForPatchGroupResponse = DescribeInstancePatchStatesForPatchGroupResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The high-level patch state for the requested instances.
    instancePatchStates :: Lude.Maybe (Lude.NonEmpty InstancePatchState),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancePatchStatesForPatchGroupResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'instancePatchStates' - The high-level patch state for the requested instances.
-- * 'responseStatus' - The response status code.
mkDescribeInstancePatchStatesForPatchGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstancePatchStatesForPatchGroupResponse
mkDescribeInstancePatchStatesForPatchGroupResponse pResponseStatus_ =
  DescribeInstancePatchStatesForPatchGroupResponse'
    { nextToken =
        Lude.Nothing,
      instancePatchStates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgrsNextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Lude.Maybe Lude.Text)
dipsfpgrsNextToken = Lens.lens (nextToken :: DescribeInstancePatchStatesForPatchGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancePatchStatesForPatchGroupResponse)
{-# DEPRECATED dipsfpgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The high-level patch state for the requested instances.
--
-- /Note:/ Consider using 'instancePatchStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgrsInstancePatchStates :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Lude.Maybe (Lude.NonEmpty InstancePatchState))
dipsfpgrsInstancePatchStates = Lens.lens (instancePatchStates :: DescribeInstancePatchStatesForPatchGroupResponse -> Lude.Maybe (Lude.NonEmpty InstancePatchState)) (\s a -> s {instancePatchStates = a} :: DescribeInstancePatchStatesForPatchGroupResponse)
{-# DEPRECATED dipsfpgrsInstancePatchStates "Use generic-lens or generic-optics with 'instancePatchStates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgrsResponseStatus :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse Lude.Int
dipsfpgrsResponseStatus = Lens.lens (responseStatus :: DescribeInstancePatchStatesForPatchGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstancePatchStatesForPatchGroupResponse)
{-# DEPRECATED dipsfpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
