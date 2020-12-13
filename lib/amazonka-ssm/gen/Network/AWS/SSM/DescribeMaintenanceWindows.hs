{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the maintenance windows in an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindows
  ( -- * Creating a request
    DescribeMaintenanceWindows (..),
    mkDescribeMaintenanceWindows,

    -- ** Request lenses
    dmwFilters,
    dmwNextToken,
    dmwMaxResults,

    -- * Destructuring the response
    DescribeMaintenanceWindowsResponse (..),
    mkDescribeMaintenanceWindowsResponse,

    -- ** Response lenses
    dmwrsWindowIdentities,
    dmwrsNextToken,
    dmwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeMaintenanceWindows' smart constructor.
data DescribeMaintenanceWindows = DescribeMaintenanceWindows'
  { -- | Optional filters used to narrow down the scope of the returned maintenance windows. Supported filter keys are __Name__ and __Enabled__ .
    filters :: Lude.Maybe [MaintenanceWindowFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindows' with the minimum fields required to make a request.
--
-- * 'filters' - Optional filters used to narrow down the scope of the returned maintenance windows. Supported filter keys are __Name__ and __Enabled__ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeMaintenanceWindows ::
  DescribeMaintenanceWindows
mkDescribeMaintenanceWindows =
  DescribeMaintenanceWindows'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optional filters used to narrow down the scope of the returned maintenance windows. Supported filter keys are __Name__ and __Enabled__ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwFilters :: Lens.Lens' DescribeMaintenanceWindows (Lude.Maybe [MaintenanceWindowFilter])
dmwFilters = Lens.lens (filters :: DescribeMaintenanceWindows -> Lude.Maybe [MaintenanceWindowFilter]) (\s a -> s {filters = a} :: DescribeMaintenanceWindows)
{-# DEPRECATED dmwFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwNextToken :: Lens.Lens' DescribeMaintenanceWindows (Lude.Maybe Lude.Text)
dmwNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindows -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindows)
{-# DEPRECATED dmwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwMaxResults :: Lens.Lens' DescribeMaintenanceWindows (Lude.Maybe Lude.Natural)
dmwMaxResults = Lens.lens (maxResults :: DescribeMaintenanceWindows -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeMaintenanceWindows)
{-# DEPRECATED dmwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeMaintenanceWindows where
  page rq rs
    | Page.stop (rs Lens.^. dmwrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmwrsWindowIdentities) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmwNextToken Lens..~ rs Lens.^. dmwrsNextToken

instance Lude.AWSRequest DescribeMaintenanceWindows where
  type
    Rs DescribeMaintenanceWindows =
      DescribeMaintenanceWindowsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsResponse'
            Lude.<$> (x Lude..?> "WindowIdentities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceWindows where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeMaintenanceWindows" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceWindows where
  toJSON DescribeMaintenanceWindows' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeMaintenanceWindows where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceWindows where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMaintenanceWindowsResponse' smart constructor.
data DescribeMaintenanceWindowsResponse = DescribeMaintenanceWindowsResponse'
  { -- | Information about the maintenance windows.
    windowIdentities :: Lude.Maybe [MaintenanceWindowIdentity],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceWindowsResponse' with the minimum fields required to make a request.
--
-- * 'windowIdentities' - Information about the maintenance windows.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeMaintenanceWindowsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceWindowsResponse
mkDescribeMaintenanceWindowsResponse pResponseStatus_ =
  DescribeMaintenanceWindowsResponse'
    { windowIdentities =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the maintenance windows.
--
-- /Note:/ Consider using 'windowIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrsWindowIdentities :: Lens.Lens' DescribeMaintenanceWindowsResponse (Lude.Maybe [MaintenanceWindowIdentity])
dmwrsWindowIdentities = Lens.lens (windowIdentities :: DescribeMaintenanceWindowsResponse -> Lude.Maybe [MaintenanceWindowIdentity]) (\s a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsResponse)
{-# DEPRECATED dmwrsWindowIdentities "Use generic-lens or generic-optics with 'windowIdentities' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrsNextToken :: Lens.Lens' DescribeMaintenanceWindowsResponse (Lude.Maybe Lude.Text)
dmwrsNextToken = Lens.lens (nextToken :: DescribeMaintenanceWindowsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMaintenanceWindowsResponse)
{-# DEPRECATED dmwrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowsResponse Lude.Int
dmwrsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceWindowsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceWindowsResponse)
{-# DEPRECATED dmwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
