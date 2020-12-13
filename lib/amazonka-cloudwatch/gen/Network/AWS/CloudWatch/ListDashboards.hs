{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.ListDashboards
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the dashboards for your account. If you include @DashboardNamePrefix@ , only those dashboards with names starting with the prefix are listed. Otherwise, all dashboards in your account are listed.
--
-- @ListDashboards@ returns up to 1000 results on one page. If there are more than 1000 dashboards, you can call @ListDashboards@ again and include the value you received for @NextToken@ in the first call, to receive the next 1000 results.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.ListDashboards
  ( -- * Creating a request
    ListDashboards (..),
    mkListDashboards,

    -- ** Request lenses
    ldDashboardNamePrefix,
    ldNextToken,

    -- * Destructuring the response
    ListDashboardsResponse (..),
    mkListDashboardsResponse,

    -- ** Response lenses
    ldrsDashboardEntries,
    ldrsNextToken,
    ldrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDashboards' smart constructor.
data ListDashboards = ListDashboards'
  { -- | If you specify this parameter, only the dashboards with names starting with the specified string are listed. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, ".", "-", and "_".
    dashboardNamePrefix :: Lude.Maybe Lude.Text,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDashboards' with the minimum fields required to make a request.
--
-- * 'dashboardNamePrefix' - If you specify this parameter, only the dashboards with names starting with the specified string are listed. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, ".", "-", and "_".
-- * 'nextToken' - The token returned by a previous call to indicate that there is more data available.
mkListDashboards ::
  ListDashboards
mkListDashboards =
  ListDashboards'
    { dashboardNamePrefix = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | If you specify this parameter, only the dashboards with names starting with the specified string are listed. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, ".", "-", and "_".
--
-- /Note:/ Consider using 'dashboardNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldDashboardNamePrefix :: Lens.Lens' ListDashboards (Lude.Maybe Lude.Text)
ldDashboardNamePrefix = Lens.lens (dashboardNamePrefix :: ListDashboards -> Lude.Maybe Lude.Text) (\s a -> s {dashboardNamePrefix = a} :: ListDashboards)
{-# DEPRECATED ldDashboardNamePrefix "Use generic-lens or generic-optics with 'dashboardNamePrefix' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDashboards (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDashboards -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDashboards)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListDashboards where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDashboardEntries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDashboards where
  type Rs ListDashboards = ListDashboardsResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "ListDashboardsResult"
      ( \s h x ->
          ListDashboardsResponse'
            Lude.<$> ( x Lude..@? "DashboardEntries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDashboards where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDashboards where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDashboards where
  toQuery ListDashboards' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListDashboards" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "DashboardNamePrefix" Lude.=: dashboardNamePrefix,
        "NextToken" Lude.=: nextToken
      ]

-- | /See:/ 'mkListDashboardsResponse' smart constructor.
data ListDashboardsResponse = ListDashboardsResponse'
  { -- | The list of matching dashboards.
    dashboardEntries :: Lude.Maybe [DashboardEntry],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDashboardsResponse' with the minimum fields required to make a request.
--
-- * 'dashboardEntries' - The list of matching dashboards.
-- * 'nextToken' - The token that marks the start of the next batch of returned results.
-- * 'responseStatus' - The response status code.
mkListDashboardsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDashboardsResponse
mkListDashboardsResponse pResponseStatus_ =
  ListDashboardsResponse'
    { dashboardEntries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of matching dashboards.
--
-- /Note:/ Consider using 'dashboardEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDashboardEntries :: Lens.Lens' ListDashboardsResponse (Lude.Maybe [DashboardEntry])
ldrsDashboardEntries = Lens.lens (dashboardEntries :: ListDashboardsResponse -> Lude.Maybe [DashboardEntry]) (\s a -> s {dashboardEntries = a} :: ListDashboardsResponse)
{-# DEPRECATED ldrsDashboardEntries "Use generic-lens or generic-optics with 'dashboardEntries' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDashboardsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDashboardsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDashboardsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDashboardsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDashboardsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDashboardsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
