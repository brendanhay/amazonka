{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of apps.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeApps
  ( -- * Creating a request
    DescribeApps (..),
    mkDescribeApps,

    -- ** Request lenses
    daAppIds,
    daStackId,

    -- * Destructuring the response
    DescribeAppsResponse (..),
    mkDescribeAppsResponse,

    -- ** Response lenses
    darrsApps,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeApps' smart constructor.
data DescribeApps = DescribeApps'
  { -- | An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
    appIds :: Core.Maybe [Types.String],
    -- | The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
    stackId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApps' value with any optional fields omitted.
mkDescribeApps ::
  DescribeApps
mkDescribeApps =
  DescribeApps' {appIds = Core.Nothing, stackId = Core.Nothing}

-- | An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
--
-- /Note:/ Consider using 'appIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppIds :: Lens.Lens' DescribeApps (Core.Maybe [Types.String])
daAppIds = Lens.field @"appIds"
{-# DEPRECATED daAppIds "Use generic-lens or generic-optics with 'appIds' instead." #-}

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStackId :: Lens.Lens' DescribeApps (Core.Maybe Types.String)
daStackId = Lens.field @"stackId"
{-# DEPRECATED daStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeApps where
  toJSON DescribeApps {..} =
    Core.object
      ( Core.catMaybes
          [ ("AppIds" Core..=) Core.<$> appIds,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribeApps where
  type Rs DescribeApps = DescribeAppsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeApps")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppsResponse'
            Core.<$> (x Core..:? "Apps") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeApps@ request.
--
-- /See:/ 'mkDescribeAppsResponse' smart constructor.
data DescribeAppsResponse = DescribeAppsResponse'
  { -- | An array of @App@ objects that describe the specified apps.
    apps :: Core.Maybe [Types.App],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAppsResponse' value with any optional fields omitted.
mkDescribeAppsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAppsResponse
mkDescribeAppsResponse responseStatus =
  DescribeAppsResponse' {apps = Core.Nothing, responseStatus}

-- | An array of @App@ objects that describe the specified apps.
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsApps :: Lens.Lens' DescribeAppsResponse (Core.Maybe [Types.App])
darrsApps = Lens.field @"apps"
{-# DEPRECATED darrsApps "Use generic-lens or generic-optics with 'apps' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAppsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
