{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteDashboards
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all dashboards that you specify. You can specify up to 100 dashboards to delete. If there is an error during this call, no dashboards are deleted.
module Network.AWS.CloudWatch.DeleteDashboards
  ( -- * Creating a request
    DeleteDashboards (..),
    mkDeleteDashboards,

    -- ** Request lenses
    ddDashboardNames,

    -- * Destructuring the response
    DeleteDashboardsResponse (..),
    mkDeleteDashboardsResponse,

    -- ** Response lenses
    ddrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDashboards' smart constructor.
newtype DeleteDashboards = DeleteDashboards'
  { dashboardNames ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDashboards' with the minimum fields required to make a request.
--
-- * 'dashboardNames' - The dashboards to be deleted. This parameter is required.
mkDeleteDashboards ::
  DeleteDashboards
mkDeleteDashboards =
  DeleteDashboards' {dashboardNames = Lude.mempty}

-- | The dashboards to be deleted. This parameter is required.
--
-- /Note:/ Consider using 'dashboardNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDashboardNames :: Lens.Lens' DeleteDashboards [Lude.Text]
ddDashboardNames = Lens.lens (dashboardNames :: DeleteDashboards -> [Lude.Text]) (\s a -> s {dashboardNames = a} :: DeleteDashboards)
{-# DEPRECATED ddDashboardNames "Use generic-lens or generic-optics with 'dashboardNames' instead." #-}

instance Lude.AWSRequest DeleteDashboards where
  type Rs DeleteDashboards = DeleteDashboardsResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DeleteDashboardsResult"
      ( \s h x ->
          DeleteDashboardsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDashboards where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDashboards where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDashboards where
  toQuery DeleteDashboards' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDashboards" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "DashboardNames" Lude.=: Lude.toQueryList "member" dashboardNames
      ]

-- | /See:/ 'mkDeleteDashboardsResponse' smart constructor.
newtype DeleteDashboardsResponse = DeleteDashboardsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDashboardsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDashboardsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDashboardsResponse
mkDeleteDashboardsResponse pResponseStatus_ =
  DeleteDashboardsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDashboardsResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDashboardsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDashboardsResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
