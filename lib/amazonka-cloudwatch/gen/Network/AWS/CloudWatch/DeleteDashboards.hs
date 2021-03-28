{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteDashboards (..)
    , mkDeleteDashboards
    -- ** Request lenses
    , ddDashboardNames

    -- * Destructuring the response
    , DeleteDashboardsResponse (..)
    , mkDeleteDashboardsResponse
    -- ** Response lenses
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDashboards' smart constructor.
newtype DeleteDashboards = DeleteDashboards'
  { dashboardNames :: [Types.DashboardName]
    -- ^ The dashboards to be deleted. This parameter is required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDashboards' value with any optional fields omitted.
mkDeleteDashboards
    :: DeleteDashboards
mkDeleteDashboards
  = DeleteDashboards'{dashboardNames = Core.mempty}

-- | The dashboards to be deleted. This parameter is required.
--
-- /Note:/ Consider using 'dashboardNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDashboardNames :: Lens.Lens' DeleteDashboards [Types.DashboardName]
ddDashboardNames = Lens.field @"dashboardNames"
{-# INLINEABLE ddDashboardNames #-}
{-# DEPRECATED dashboardNames "Use generic-lens or generic-optics with 'dashboardNames' instead"  #-}

instance Core.ToQuery DeleteDashboards where
        toQuery DeleteDashboards{..}
          = Core.toQueryPair "Action" ("DeleteDashboards" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "DashboardNames"
                (Core.toQueryList "member" dashboardNames)

instance Core.ToHeaders DeleteDashboards where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDashboards where
        type Rs DeleteDashboards = DeleteDashboardsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DeleteDashboardsResult"
              (\ s h x ->
                 DeleteDashboardsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDashboardsResponse' smart constructor.
newtype DeleteDashboardsResponse = DeleteDashboardsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDashboardsResponse' value with any optional fields omitted.
mkDeleteDashboardsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDashboardsResponse
mkDeleteDashboardsResponse responseStatus
  = DeleteDashboardsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDashboardsResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
