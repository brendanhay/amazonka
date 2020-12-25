{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteLaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a launch template. Deleting a launch template deletes all of its versions.
module Network.AWS.EC2.DeleteLaunchTemplate
  ( -- * Creating a request
    DeleteLaunchTemplate (..),
    mkDeleteLaunchTemplate,

    -- ** Request lenses
    dltDryRun,
    dltLaunchTemplateId,
    dltLaunchTemplateName,

    -- * Destructuring the response
    DeleteLaunchTemplateResponse (..),
    mkDeleteLaunchTemplateResponse,

    -- ** Response lenses
    dltrrsLaunchTemplate,
    dltrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLaunchTemplate' smart constructor.
data DeleteLaunchTemplate = DeleteLaunchTemplate'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
    launchTemplateId :: Core.Maybe Types.LaunchTemplateId,
    -- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
    launchTemplateName :: Core.Maybe Types.LaunchTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchTemplate' value with any optional fields omitted.
mkDeleteLaunchTemplate ::
  DeleteLaunchTemplate
mkDeleteLaunchTemplate =
  DeleteLaunchTemplate'
    { dryRun = Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltDryRun :: Lens.Lens' DeleteLaunchTemplate (Core.Maybe Core.Bool)
dltDryRun = Lens.field @"dryRun"
{-# DEPRECATED dltDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplate (Core.Maybe Types.LaunchTemplateId)
dltLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED dltLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplate (Core.Maybe Types.LaunchTemplateName)
dltLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED dltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

instance Core.AWSRequest DeleteLaunchTemplate where
  type Rs DeleteLaunchTemplate = DeleteLaunchTemplateResponse
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
            ( Core.pure ("Action", "DeleteLaunchTemplate")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "LaunchTemplateId" Core.<$> launchTemplateId)
                Core.<> ( Core.toQueryValue "LaunchTemplateName"
                            Core.<$> launchTemplateName
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLaunchTemplateResponse'
            Core.<$> (x Core..@? "launchTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLaunchTemplateResponse' smart constructor.
data DeleteLaunchTemplateResponse = DeleteLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Core.Maybe Types.LaunchTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteLaunchTemplateResponse' value with any optional fields omitted.
mkDeleteLaunchTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLaunchTemplateResponse
mkDeleteLaunchTemplateResponse responseStatus =
  DeleteLaunchTemplateResponse'
    { launchTemplate = Core.Nothing,
      responseStatus
    }

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrrsLaunchTemplate :: Lens.Lens' DeleteLaunchTemplateResponse (Core.Maybe Types.LaunchTemplate)
dltrrsLaunchTemplate = Lens.field @"launchTemplate"
{-# DEPRECATED dltrrsLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltrrsResponseStatus :: Lens.Lens' DeleteLaunchTemplateResponse Core.Int
dltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
