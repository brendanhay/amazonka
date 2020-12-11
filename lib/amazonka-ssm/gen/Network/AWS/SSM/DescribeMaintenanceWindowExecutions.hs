{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a maintenance window. This includes information about when the maintenance window was scheduled to be active, and information about tasks registered and run with the maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutions
  ( -- * Creating a request
    DescribeMaintenanceWindowExecutions (..),
    mkDescribeMaintenanceWindowExecutions,

    -- ** Request lenses
    dmweFilters,
    dmweNextToken,
    dmweMaxResults,
    dmweWindowId,

    -- * Destructuring the response
    DescribeMaintenanceWindowExecutionsResponse (..),
    mkDescribeMaintenanceWindowExecutionsResponse,

    -- ** Response lenses
    dmwersWindowExecutions,
    dmwersNextToken,
    dmwersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindowExecutions' smart constructor.
data DescribeMaintenanceWindowExecutions = DescribeMaintenanceWindowExecutions'
  { filters ::
      Lude.Maybe
        [MaintenanceWindowFilter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    windowId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowExecutions' with the minimum fields required to make a request.
--
-- * 'filters' - Each entry in the array is a structure containing:
--
-- Key (string, between 1 and 128 characters)
-- Values (array of strings, each string is between 1 and 256 characters)
-- The supported Keys are ExecutedBefore and ExecutedAfter with the value being a date/time string such as 2016-11-04T05:00:00Z.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'windowId' - The ID of the maintenance window whose executions should be retrieved.
mkDescribeMaintenanceWindowExecutions ::
  -- | 'windowId'
  Lude.Text ->
  DescribeMaintenanceWindowExecutions
mkDescribeMaintenanceWindowExecutions pWindowId_ =
  DescribeMaintenanceWindowExecutions'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      windowId = pWindowId_
    }

-- | Each entry in the array is a structure containing:
--
-- Key (string, between 1 and 128 characters)
-- Values (array of strings, each string is between 1 and 256 characters)
-- The supported Keys are ExecutedBefore and ExecutedAfter with the value being a date/time string such as 2016-11-04T05:00:00Z.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweFilters :: Lens.Lens' DescribeMaintenanceWindowExecutions (Lude.Maybe [MaintenanceWindowFilter])
dmweFilters = Lens.lens (filters :: DescribeMaintenanceWindowExecutions -> Lude.Maybe [MaintenanceWindowFilter]) (\s a -> s {filters = a} :: DescribeMaintenanceWindowExecutions)
{-# DEPRECATED dmweFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutions (Lude.Maybe Lude.Text)
dmweNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutions)
{-# DEPRECATED dmweNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweMaxResults :: Lens.Lens' DescribeMaintenanceWindowExecutions (Lude.Maybe Lude.Natural)
dmweMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindowExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutions)
{-# DEPRECATED dmweMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the maintenance window whose executions should be retrieved.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweWindowId :: Lens.Lens' DescribeMaintenanceWindowExecutions Lude.Text
dmweWindowId = Lens.lens (windowId :: DescribeMaintenanceWindowExecutions -> Lude.Text) (\s a -> s {windowId = a} :: DescribeMaintenanceWindowExecutions)
{-# DEPRECATED dmweWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Page.AWSPager DescribeMaintenanceWindowExecutions where
  page rq rs
    | Page.stop (rs Lens.^. dmwersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmwersWindowExecutions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmweNextToken Lens..~ rs Lens.^. dmwersNextToken

instance Lude.AWSRequest DescribeMaintenanceWindowExecutions where
  type
    Rs DescribeMaintenanceWindowExecutions =
      DescribeMaintenanceWindowExecutionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionsResponse'
            Lude.<$> (x Lude..?> "WindowExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceWindowExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceWindowExecutions where
  toJSON DescribeMaintenanceWindowExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("WindowId" Lude..= windowId)
          ]
      )

instance Lude.ToPath DescribeMaintenanceWindowExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceWindowExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionsResponse = DescribeMaintenanceWindowExecutionsResponse'
  { windowExecutions ::
      Lude.Maybe
        [MaintenanceWindowExecution],
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

-- | Creates a value of 'DescribeMaintenanceWindowExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'windowExecutions' - Information about the maintenance window executions.
mkDescribeMaintenanceWindowExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowExecutionsResponse
mkDescribeMaintenanceWindowExecutionsResponse pResponseStatus_ =
  DescribeMaintenanceWindowExecutionsResponse'
    { windowExecutions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the maintenance window executions.
--
-- /Note:/ Consider using 'windowExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwersWindowExecutions :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Lude.Maybe [MaintenanceWindowExecution])
dmwersWindowExecutions = Lens.lens (windowExecutions :: DescribeMaintenanceWindowExecutionsResponse -> Lude.Maybe [MaintenanceWindowExecution]) (\s a -> s {windowExecutions = a} :: DescribeMaintenanceWindowExecutionsResponse)
{-# DEPRECATED dmwersWindowExecutions "Use generic-lens or generic-optics with 'windowExecutions' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwersNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Lude.Maybe Lude.Text)
dmwersNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionsResponse)
{-# DEPRECATED dmwersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwersResponseStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse Lude.Int
dmwersResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowExecutionsResponse)
{-# DEPRECATED dmwersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
