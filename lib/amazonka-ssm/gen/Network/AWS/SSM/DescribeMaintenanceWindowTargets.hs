{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets registered with the maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowTargets
  ( -- * Creating a request
    DescribeMaintenanceWindowTargets (..),
    mkDescribeMaintenanceWindowTargets,

    -- ** Request lenses
    dmwtFilters,
    dmwtNextToken,
    dmwtMaxResults,
    dmwtWindowId,

    -- * Destructuring the response
    DescribeMaintenanceWindowTargetsResponse (..),
    mkDescribeMaintenanceWindowTargetsResponse,

    -- ** Response lenses
    dmwtrsNextToken,
    dmwtrsTargets,
    dmwtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindowTargets' smart constructor.
data DescribeMaintenanceWindowTargets = DescribeMaintenanceWindowTargets'
  { filters ::
      Lude.Maybe
        [MaintenanceWindowFilter],
    nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    windowId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowTargets' with the minimum fields required to make a request.
--
-- * 'filters' - Optional filters that can be used to narrow down the scope of the returned window targets. The supported filter keys are Type, WindowTargetId and OwnerInformation.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'windowId' - The ID of the maintenance window whose targets should be retrieved.
mkDescribeMaintenanceWindowTargets ::
  -- | 'windowId'
  Lude.Text ->
  DescribeMaintenanceWindowTargets
mkDescribeMaintenanceWindowTargets pWindowId_ =
  DescribeMaintenanceWindowTargets'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      windowId = pWindowId_
    }

-- | Optional filters that can be used to narrow down the scope of the returned window targets. The supported filter keys are Type, WindowTargetId and OwnerInformation.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtFilters :: Lens.Lens' DescribeMaintenanceWindowTargets (Lude.Maybe [MaintenanceWindowFilter])
dmwtFilters = Lens.lens (filters :: DescribeMaintenanceWindowTargets -> Lude.Maybe [MaintenanceWindowFilter]) (\s a -> s {filters = a} :: DescribeMaintenanceWindowTargets)
{-# DEPRECATED dmwtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtNextToken :: Lens.Lens' DescribeMaintenanceWindowTargets (Lude.Maybe Lude.Text)
dmwtNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowTargets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowTargets)
{-# DEPRECATED dmwtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtMaxResults :: Lens.Lens' DescribeMaintenanceWindowTargets (Lude.Maybe Lude.Natural)
dmwtMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindowTargets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindowTargets)
{-# DEPRECATED dmwtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the maintenance window whose targets should be retrieved.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtWindowId :: Lens.Lens' DescribeMaintenanceWindowTargets Lude.Text
dmwtWindowId = Lens.lens (windowId :: DescribeMaintenanceWindowTargets -> Lude.Text) (\s a -> s {windowId = a} :: DescribeMaintenanceWindowTargets)
{-# DEPRECATED dmwtWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Page.AWSPager DescribeMaintenanceWindowTargets where
  page rq rs
    | Page.stop (rs Lens.^. dmwtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmwtrsTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmwtNextToken Lens..~ rs Lens.^. dmwtrsNextToken

instance Lude.AWSRequest DescribeMaintenanceWindowTargets where
  type
    Rs DescribeMaintenanceWindowTargets =
      DescribeMaintenanceWindowTargetsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTargetsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Targets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceWindowTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeMaintenanceWindowTargets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceWindowTargets where
  toJSON DescribeMaintenanceWindowTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("WindowId" Lude..= windowId)
          ]
      )

instance Lude.ToPath DescribeMaintenanceWindowTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceWindowTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowTargetsResponse' smart constructor.
data DescribeMaintenanceWindowTargetsResponse = DescribeMaintenanceWindowTargetsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    targets ::
      Lude.Maybe
        [MaintenanceWindowTarget],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowTargetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'targets' - Information about the targets in the maintenance window.
mkDescribeMaintenanceWindowTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowTargetsResponse
mkDescribeMaintenanceWindowTargetsResponse pResponseStatus_ =
  DescribeMaintenanceWindowTargetsResponse'
    { nextToken =
        Lude.Nothing,
      targets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrsNextToken :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Lude.Maybe Lude.Text)
dmwtrsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowTargetsResponse)
{-# DEPRECATED dmwtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the targets in the maintenance window.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrsTargets :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Lude.Maybe [MaintenanceWindowTarget])
dmwtrsTargets = Lens.lens (targets :: DescribeMaintenanceWindowTargetsResponse -> Lude.Maybe [MaintenanceWindowTarget]) (\s a -> s {targets = a} :: DescribeMaintenanceWindowTargetsResponse)
{-# DEPRECATED dmwtrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse Lude.Int
dmwtrsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowTargetsResponse)
{-# DEPRECATED dmwtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
