{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetDashboard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of the dashboard that you specify.
--
-- To copy an existing dashboard, use @GetDashboard@ , and then use the data returned within @DashboardBody@ as the template for the new dashboard when you call @PutDashboard@ to create the copy.
module Network.AWS.CloudWatch.GetDashboard
  ( -- * Creating a request
    GetDashboard (..),
    mkGetDashboard,

    -- ** Request lenses
    gdDashboardName,

    -- * Destructuring the response
    GetDashboardResponse (..),
    mkGetDashboardResponse,

    -- ** Response lenses
    gdrrsDashboardArn,
    gdrrsDashboardBody,
    gdrrsDashboardName,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDashboard' smart constructor.
newtype GetDashboard = GetDashboard'
  { -- | The name of the dashboard to be described.
    dashboardName :: Types.DashboardName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDashboard' value with any optional fields omitted.
mkGetDashboard ::
  -- | 'dashboardName'
  Types.DashboardName ->
  GetDashboard
mkGetDashboard dashboardName = GetDashboard' {dashboardName}

-- | The name of the dashboard to be described.
--
-- /Note:/ Consider using 'dashboardName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDashboardName :: Lens.Lens' GetDashboard Types.DashboardName
gdDashboardName = Lens.field @"dashboardName"
{-# DEPRECATED gdDashboardName "Use generic-lens or generic-optics with 'dashboardName' instead." #-}

instance Core.AWSRequest GetDashboard where
  type Rs GetDashboard = GetDashboardResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetDashboard")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "DashboardName" dashboardName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetDashboardResult"
      ( \s h x ->
          GetDashboardResponse'
            Core.<$> (x Core..@? "DashboardArn")
            Core.<*> (x Core..@? "DashboardBody")
            Core.<*> (x Core..@? "DashboardName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDashboardResponse' smart constructor.
data GetDashboardResponse = GetDashboardResponse'
  { -- | The Amazon Resource Name (ARN) of the dashboard.
    dashboardArn :: Core.Maybe Types.DashboardArn,
    -- | The detailed information about the dashboard, including what widgets are included and their location on the dashboard. For more information about the @DashboardBody@ syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax> .
    dashboardBody :: Core.Maybe Types.DashboardBody,
    -- | The name of the dashboard.
    dashboardName :: Core.Maybe Types.DashboardName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDashboardResponse' value with any optional fields omitted.
mkGetDashboardResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDashboardResponse
mkGetDashboardResponse responseStatus =
  GetDashboardResponse'
    { dashboardArn = Core.Nothing,
      dashboardBody = Core.Nothing,
      dashboardName = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the dashboard.
--
-- /Note:/ Consider using 'dashboardArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDashboardArn :: Lens.Lens' GetDashboardResponse (Core.Maybe Types.DashboardArn)
gdrrsDashboardArn = Lens.field @"dashboardArn"
{-# DEPRECATED gdrrsDashboardArn "Use generic-lens or generic-optics with 'dashboardArn' instead." #-}

-- | The detailed information about the dashboard, including what widgets are included and their location on the dashboard. For more information about the @DashboardBody@ syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax> .
--
-- /Note:/ Consider using 'dashboardBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDashboardBody :: Lens.Lens' GetDashboardResponse (Core.Maybe Types.DashboardBody)
gdrrsDashboardBody = Lens.field @"dashboardBody"
{-# DEPRECATED gdrrsDashboardBody "Use generic-lens or generic-optics with 'dashboardBody' instead." #-}

-- | The name of the dashboard.
--
-- /Note:/ Consider using 'dashboardName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDashboardName :: Lens.Lens' GetDashboardResponse (Core.Maybe Types.DashboardName)
gdrrsDashboardName = Lens.field @"dashboardName"
{-# DEPRECATED gdrrsDashboardName "Use generic-lens or generic-optics with 'dashboardName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDashboardResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
