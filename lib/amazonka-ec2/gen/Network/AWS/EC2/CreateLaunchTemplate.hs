{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateLaunchTemplate (..)
    , mkCreateLaunchTemplate
    -- ** Request lenses
    , cltLaunchTemplateName
    , cltLaunchTemplateData
    , cltClientToken
    , cltDryRun
    , cltTagSpecifications
    , cltVersionDescription

    -- * Destructuring the response
    , CreateLaunchTemplateResponse (..)
    , mkCreateLaunchTemplateResponse
    -- ** Response lenses
    , cltrrsLaunchTemplate
    , cltrrsWarning
    , cltrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLaunchTemplate' smart constructor.
data CreateLaunchTemplate = CreateLaunchTemplate'
  { launchTemplateName :: Types.LaunchTemplateName
    -- ^ A name for the launch template.
  , launchTemplateData :: Types.RequestLaunchTemplateData
    -- ^ The information for the launch template.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the launch template during creation.
  , versionDescription :: Core.Maybe Types.VersionDescription
    -- ^ A description for the first version of the launch template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLaunchTemplate' value with any optional fields omitted.
mkCreateLaunchTemplate
    :: Types.LaunchTemplateName -- ^ 'launchTemplateName'
    -> Types.RequestLaunchTemplateData -- ^ 'launchTemplateData'
    -> CreateLaunchTemplate
mkCreateLaunchTemplate launchTemplateName launchTemplateData
  = CreateLaunchTemplate'{launchTemplateName, launchTemplateData,
                          clientToken = Core.Nothing, dryRun = Core.Nothing,
                          tagSpecifications = Core.Nothing,
                          versionDescription = Core.Nothing}

-- | A name for the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltLaunchTemplateName :: Lens.Lens' CreateLaunchTemplate Types.LaunchTemplateName
cltLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE cltLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The information for the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltLaunchTemplateData :: Lens.Lens' CreateLaunchTemplate Types.RequestLaunchTemplateData
cltLaunchTemplateData = Lens.field @"launchTemplateData"
{-# INLINEABLE cltLaunchTemplateData #-}
{-# DEPRECATED launchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead"  #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltClientToken :: Lens.Lens' CreateLaunchTemplate (Core.Maybe Core.Text)
cltClientToken = Lens.field @"clientToken"
{-# INLINEABLE cltClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltDryRun :: Lens.Lens' CreateLaunchTemplate (Core.Maybe Core.Bool)
cltDryRun = Lens.field @"dryRun"
{-# INLINEABLE cltDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to apply to the launch template during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltTagSpecifications :: Lens.Lens' CreateLaunchTemplate (Core.Maybe [Types.TagSpecification])
cltTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cltTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | A description for the first version of the launch template.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltVersionDescription :: Lens.Lens' CreateLaunchTemplate (Core.Maybe Types.VersionDescription)
cltVersionDescription = Lens.field @"versionDescription"
{-# INLINEABLE cltVersionDescription #-}
{-# DEPRECATED versionDescription "Use generic-lens or generic-optics with 'versionDescription' instead"  #-}

instance Core.ToQuery CreateLaunchTemplate where
        toQuery CreateLaunchTemplate{..}
          = Core.toQueryPair "Action" ("CreateLaunchTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "LaunchTemplateName" launchTemplateName
              Core.<> Core.toQueryPair "LaunchTemplateData" launchTemplateData
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VersionDescription")
                versionDescription

instance Core.ToHeaders CreateLaunchTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLaunchTemplate where
        type Rs CreateLaunchTemplate = CreateLaunchTemplateResponse
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
          = Response.receiveXML
              (\ s h x ->
                 CreateLaunchTemplateResponse' Core.<$>
                   (x Core..@? "launchTemplate") Core.<*> x Core..@? "warning"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLaunchTemplateResponse' smart constructor.
data CreateLaunchTemplateResponse = CreateLaunchTemplateResponse'
  { launchTemplate :: Core.Maybe Types.LaunchTemplate
    -- ^ Information about the launch template.
  , warning :: Core.Maybe Types.ValidationWarning
    -- ^ If the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLaunchTemplateResponse' value with any optional fields omitted.
mkCreateLaunchTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLaunchTemplateResponse
mkCreateLaunchTemplateResponse responseStatus
  = CreateLaunchTemplateResponse'{launchTemplate = Core.Nothing,
                                  warning = Core.Nothing, responseStatus}

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrrsLaunchTemplate :: Lens.Lens' CreateLaunchTemplateResponse (Core.Maybe Types.LaunchTemplate)
cltrrsLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE cltrrsLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | If the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrrsWarning :: Lens.Lens' CreateLaunchTemplateResponse (Core.Maybe Types.ValidationWarning)
cltrrsWarning = Lens.field @"warning"
{-# INLINEABLE cltrrsWarning #-}
{-# DEPRECATED warning "Use generic-lens or generic-optics with 'warning' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltrrsResponseStatus :: Lens.Lens' CreateLaunchTemplateResponse Core.Int
cltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
