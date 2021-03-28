{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateLaunchTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version for a launch template. You can specify an existing version of launch template from which to base the new version.
--
-- Launch template versions are numbered in the order in which they are created. You cannot specify, change, or replace the numbering of launch template versions.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#manage-launch-template-versions Managing launch template versions> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateLaunchTemplateVersion
    (
    -- * Creating a request
      CreateLaunchTemplateVersion (..)
    , mkCreateLaunchTemplateVersion
    -- ** Request lenses
    , cltvLaunchTemplateData
    , cltvClientToken
    , cltvDryRun
    , cltvLaunchTemplateId
    , cltvLaunchTemplateName
    , cltvSourceVersion
    , cltvVersionDescription

    -- * Destructuring the response
    , CreateLaunchTemplateVersionResponse (..)
    , mkCreateLaunchTemplateVersionResponse
    -- ** Response lenses
    , cltvrrsLaunchTemplateVersion
    , cltvrrsWarning
    , cltvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLaunchTemplateVersion' smart constructor.
data CreateLaunchTemplateVersion = CreateLaunchTemplateVersion'
  { launchTemplateData :: Types.RequestLaunchTemplateData
    -- ^ The information for the launch template.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , launchTemplateId :: Core.Maybe Types.LaunchTemplateId
    -- ^ The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template. You must specify either the launch template ID or launch template name in the request.
  , sourceVersion :: Core.Maybe Core.Text
    -- ^ The version number of the launch template version on which to base the new version. The new version inherits the same launch parameters as the source version, except for parameters that you specify in @LaunchTemplateData@ . Snapshots applied to the block device mapping are ignored when creating a new version unless they are explicitly included.
  , versionDescription :: Core.Maybe Types.VersionDescription
    -- ^ A description for the version of the launch template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLaunchTemplateVersion' value with any optional fields omitted.
mkCreateLaunchTemplateVersion
    :: Types.RequestLaunchTemplateData -- ^ 'launchTemplateData'
    -> CreateLaunchTemplateVersion
mkCreateLaunchTemplateVersion launchTemplateData
  = CreateLaunchTemplateVersion'{launchTemplateData,
                                 clientToken = Core.Nothing, dryRun = Core.Nothing,
                                 launchTemplateId = Core.Nothing, launchTemplateName = Core.Nothing,
                                 sourceVersion = Core.Nothing, versionDescription = Core.Nothing}

-- | The information for the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvLaunchTemplateData :: Lens.Lens' CreateLaunchTemplateVersion Types.RequestLaunchTemplateData
cltvLaunchTemplateData = Lens.field @"launchTemplateData"
{-# INLINEABLE cltvLaunchTemplateData #-}
{-# DEPRECATED launchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead"  #-}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvClientToken :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Text)
cltvClientToken = Lens.field @"clientToken"
{-# INLINEABLE cltvClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvDryRun :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Bool)
cltvDryRun = Lens.field @"dryRun"
{-# INLINEABLE cltvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvLaunchTemplateId :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Types.LaunchTemplateId)
cltvLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE cltvLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvLaunchTemplateName :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Types.LaunchTemplateName)
cltvLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE cltvLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The version number of the launch template version on which to base the new version. The new version inherits the same launch parameters as the source version, except for parameters that you specify in @LaunchTemplateData@ . Snapshots applied to the block device mapping are ignored when creating a new version unless they are explicitly included.
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvSourceVersion :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Text)
cltvSourceVersion = Lens.field @"sourceVersion"
{-# INLINEABLE cltvSourceVersion #-}
{-# DEPRECATED sourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead"  #-}

-- | A description for the version of the launch template.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvVersionDescription :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Types.VersionDescription)
cltvVersionDescription = Lens.field @"versionDescription"
{-# INLINEABLE cltvVersionDescription #-}
{-# DEPRECATED versionDescription "Use generic-lens or generic-optics with 'versionDescription' instead"  #-}

instance Core.ToQuery CreateLaunchTemplateVersion where
        toQuery CreateLaunchTemplateVersion{..}
          = Core.toQueryPair "Action"
              ("CreateLaunchTemplateVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "LaunchTemplateData" launchTemplateData
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateId")
                launchTemplateId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateName")
                launchTemplateName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceVersion")
                sourceVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VersionDescription")
                versionDescription

instance Core.ToHeaders CreateLaunchTemplateVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLaunchTemplateVersion where
        type Rs CreateLaunchTemplateVersion =
             CreateLaunchTemplateVersionResponse
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
                 CreateLaunchTemplateVersionResponse' Core.<$>
                   (x Core..@? "launchTemplateVersion") Core.<*> x Core..@? "warning"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLaunchTemplateVersionResponse' smart constructor.
data CreateLaunchTemplateVersionResponse = CreateLaunchTemplateVersionResponse'
  { launchTemplateVersion :: Core.Maybe Types.LaunchTemplateVersion
    -- ^ Information about the launch template version.
  , warning :: Core.Maybe Types.ValidationWarning
    -- ^ If the new version of the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLaunchTemplateVersionResponse' value with any optional fields omitted.
mkCreateLaunchTemplateVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLaunchTemplateVersionResponse
mkCreateLaunchTemplateVersionResponse responseStatus
  = CreateLaunchTemplateVersionResponse'{launchTemplateVersion =
                                           Core.Nothing,
                                         warning = Core.Nothing, responseStatus}

-- | Information about the launch template version.
--
-- /Note:/ Consider using 'launchTemplateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvrrsLaunchTemplateVersion :: Lens.Lens' CreateLaunchTemplateVersionResponse (Core.Maybe Types.LaunchTemplateVersion)
cltvrrsLaunchTemplateVersion = Lens.field @"launchTemplateVersion"
{-# INLINEABLE cltvrrsLaunchTemplateVersion #-}
{-# DEPRECATED launchTemplateVersion "Use generic-lens or generic-optics with 'launchTemplateVersion' instead"  #-}

-- | If the new version of the launch template contains parameters or parameter combinations that are not valid, an error code and an error message are returned for each issue that's found.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvrrsWarning :: Lens.Lens' CreateLaunchTemplateVersionResponse (Core.Maybe Types.ValidationWarning)
cltvrrsWarning = Lens.field @"warning"
{-# INLINEABLE cltvrrsWarning #-}
{-# DEPRECATED warning "Use generic-lens or generic-optics with 'warning' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cltvrrsResponseStatus :: Lens.Lens' CreateLaunchTemplateVersionResponse Core.Int
cltvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cltvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
