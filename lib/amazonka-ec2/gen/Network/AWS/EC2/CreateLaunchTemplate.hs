{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch template. A launch template contains the parameters to launch an instance. When you launch an instance using 'RunInstances' , you can specify a launch template instead of providing the launch parameters in the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateLaunchTemplate
  ( -- * Creating a request
    CreateLaunchTemplate (..),
    mkCreateLaunchTemplate,

    -- ** Request lenses
    cltLaunchTemplateName,
    cltLaunchTemplateData,
    cltClientToken,
    cltDryRun,
    cltTagSpecifications,
    cltVersionDescription,

    -- * Destructuring the response
    CreateLaunchTemplateResponse (..),
    mkCreateLaunchTemplateResponse,

    -- ** Response lenses
    cltrrsLaunchTemplate,
    cltrrsWarning,
    cltrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLaunchTemplate' smart constructor.
data CreateLaunchTemplate = CreateLaunchTemplate'
  { -- | A name for the launch template.
    launchTemplateName :: Types.LaunchTemplateName,
    -- | The information for the launch template.
    launchTemplateData :: Types.RequestLaunchTemplateData,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to apply to the launch template during creation.
    tagSpecifications :: Core.Maybe [Types.TagSpecification],
    -- | A description for the first version of the launch template.
    versionDescription :: Core.Maybe Types.VersionDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateLaunchTemplate' value with any optional fields omitted.
mkCreateLaunchTemplate ::
  -- | 'launchTemplateName'
  Types.LaunchTemplateName ->
  -- | 'launchTemplateData'
  Types.RequestLaunchTemplateData ->
  CreateLaunchTemplate
mkCreateLaunchTemplate launchTemplateName launchTemplateData =
  CreateLaunchTemplate'
    { launchTemplateName,
      launchTemplateData,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing,
      versionDescription = Core.Nothing
    }

-- | A name for the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltLaunchTemplateName :: Lens.Lens' CreateLaunchTemplate Types.LaunchTemplateName
cltLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED cltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The information for the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltLaunchTemplateData :: Lens.Lens' CreateLaunchTemplate Types.RequestLaunchTemplateData
cltLaunchTemplateData = Lens.field @"launchTemplateData"
{-# DEPRECATED cltLaunchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead." #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltClientToken :: Lens.Lens' CreateLaunchTemplate (Core.Maybe Types.String)
cltClientToken = Lens.field @"clientToken"
{-# DEPRECATED cltClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltDryRun :: Lens.Lens' CreateLaunchTemplate (Core.Maybe Core.Bool)
cltDryRun = Lens.field @"dryRun"
{-# DEPRECATED cltDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to apply to the launch template during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltTagSpecifications :: Lens.Lens' CreateLaunchTemplate (Core.Maybe [Types.TagSpecification])
cltTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cltTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | A description for the first version of the launch template.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltVersionDescription :: Lens.Lens' CreateLaunchTemplate (Core.Maybe Types.VersionDescription)
cltVersionDescription = Lens.field @"versionDescription"
{-# DEPRECATED cltVersionDescription "Use generic-lens or generic-optics with 'versionDescription' instead." #-}

instance Core.AWSRequest CreateLaunchTemplate where
  type Rs CreateLaunchTemplate = CreateLaunchTemplateResponse
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
            ( Core.pure ("Action", "CreateLaunchTemplate")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "LaunchTemplateName" launchTemplateName)
                Core.<> (Core.toQueryValue "LaunchTemplateData" launchTemplateData)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
                Core.<> ( Core.toQueryValue "VersionDescription"
                            Core.<$> versionDescription
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLaunchTemplateResponse'
            Core.<$> (x Core..@? "launchTemplate")
            Core.<*> (x Core..@? "warning")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateLaunchTemplateResponse' smart constructor.
data CreateLaunchTemplateResponse = CreateLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Core.Maybe Types.LaunchTemplate,
    -- | If the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
    warning :: Core.Maybe Types.ValidationWarning,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateLaunchTemplateResponse' value with any optional fields omitted.
mkCreateLaunchTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLaunchTemplateResponse
mkCreateLaunchTemplateResponse responseStatus =
  CreateLaunchTemplateResponse'
    { launchTemplate = Core.Nothing,
      warning = Core.Nothing,
      responseStatus
    }

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrrsLaunchTemplate :: Lens.Lens' CreateLaunchTemplateResponse (Core.Maybe Types.LaunchTemplate)
cltrrsLaunchTemplate = Lens.field @"launchTemplate"
{-# DEPRECATED cltrrsLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | If the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrrsWarning :: Lens.Lens' CreateLaunchTemplateResponse (Core.Maybe Types.ValidationWarning)
cltrrsWarning = Lens.field @"warning"
{-# DEPRECATED cltrrsWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrrsResponseStatus :: Lens.Lens' CreateLaunchTemplateResponse Core.Int
cltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
