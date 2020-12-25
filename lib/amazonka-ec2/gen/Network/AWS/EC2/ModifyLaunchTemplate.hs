{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyLaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a launch template. You can specify which version of the launch template to set as the default version. When launching an instance, the default version applies when a launch template version is not specified.
module Network.AWS.EC2.ModifyLaunchTemplate
  ( -- * Creating a request
    ModifyLaunchTemplate (..),
    mkModifyLaunchTemplate,

    -- ** Request lenses
    mltClientToken,
    mltDefaultVersion,
    mltDryRun,
    mltLaunchTemplateId,
    mltLaunchTemplateName,

    -- * Destructuring the response
    ModifyLaunchTemplateResponse (..),
    mkModifyLaunchTemplateResponse,

    -- ** Response lenses
    mltrrsLaunchTemplate,
    mltrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyLaunchTemplate' smart constructor.
data ModifyLaunchTemplate = ModifyLaunchTemplate'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | The version number of the launch template to set as the default version.
    defaultVersion :: Core.Maybe Types.DefaultVersion,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
    launchTemplateId :: Core.Maybe Types.LaunchTemplateId,
    -- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
    launchTemplateName :: Core.Maybe Types.LaunchTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLaunchTemplate' value with any optional fields omitted.
mkModifyLaunchTemplate ::
  ModifyLaunchTemplate
mkModifyLaunchTemplate =
  ModifyLaunchTemplate'
    { clientToken = Core.Nothing,
      defaultVersion = Core.Nothing,
      dryRun = Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltClientToken :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Types.ClientToken)
mltClientToken = Lens.field @"clientToken"
{-# DEPRECATED mltClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The version number of the launch template to set as the default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltDefaultVersion :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Types.DefaultVersion)
mltDefaultVersion = Lens.field @"defaultVersion"
{-# DEPRECATED mltDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltDryRun :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Bool)
mltDryRun = Lens.field @"dryRun"
{-# DEPRECATED mltDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltLaunchTemplateId :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Types.LaunchTemplateId)
mltLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED mltLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltLaunchTemplateName :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Types.LaunchTemplateName)
mltLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED mltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

instance Core.AWSRequest ModifyLaunchTemplate where
  type Rs ModifyLaunchTemplate = ModifyLaunchTemplateResponse
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
            ( Core.pure ("Action", "ModifyLaunchTemplate")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "SetDefaultVersion" Core.<$> defaultVersion)
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
          ModifyLaunchTemplateResponse'
            Core.<$> (x Core..@? "launchTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyLaunchTemplateResponse' smart constructor.
data ModifyLaunchTemplateResponse = ModifyLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Core.Maybe Types.LaunchTemplate,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyLaunchTemplateResponse' value with any optional fields omitted.
mkModifyLaunchTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyLaunchTemplateResponse
mkModifyLaunchTemplateResponse responseStatus =
  ModifyLaunchTemplateResponse'
    { launchTemplate = Core.Nothing,
      responseStatus
    }

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltrrsLaunchTemplate :: Lens.Lens' ModifyLaunchTemplateResponse (Core.Maybe Types.LaunchTemplate)
mltrrsLaunchTemplate = Lens.field @"launchTemplate"
{-# DEPRECATED mltrrsLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltrrsResponseStatus :: Lens.Lens' ModifyLaunchTemplateResponse Core.Int
mltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
