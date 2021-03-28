{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyLaunchTemplate (..)
    , mkModifyLaunchTemplate
    -- ** Request lenses
    , mltClientToken
    , mltDefaultVersion
    , mltDryRun
    , mltLaunchTemplateId
    , mltLaunchTemplateName

    -- * Destructuring the response
    , ModifyLaunchTemplateResponse (..)
    , mkModifyLaunchTemplateResponse
    -- ** Response lenses
    , mltrrsLaunchTemplate
    , mltrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyLaunchTemplate' smart constructor.
data ModifyLaunchTemplate = ModifyLaunchTemplate'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
  , defaultVersion :: Core.Maybe Core.Text
    -- ^ The version number of the launch template to set as the default version.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , launchTemplateId :: Core.Maybe Types.LaunchTemplateId
    -- ^ The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template. You must specify either the launch template ID or launch template name in the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLaunchTemplate' value with any optional fields omitted.
mkModifyLaunchTemplate
    :: ModifyLaunchTemplate
mkModifyLaunchTemplate
  = ModifyLaunchTemplate'{clientToken = Core.Nothing,
                          defaultVersion = Core.Nothing, dryRun = Core.Nothing,
                          launchTemplateId = Core.Nothing, launchTemplateName = Core.Nothing}

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraint: Maximum 128 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltClientToken :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Text)
mltClientToken = Lens.field @"clientToken"
{-# INLINEABLE mltClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The version number of the launch template to set as the default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltDefaultVersion :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Text)
mltDefaultVersion = Lens.field @"defaultVersion"
{-# INLINEABLE mltDefaultVersion #-}
{-# DEPRECATED defaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltDryRun :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Bool)
mltDryRun = Lens.field @"dryRun"
{-# INLINEABLE mltDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltLaunchTemplateId :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Types.LaunchTemplateId)
mltLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE mltLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltLaunchTemplateName :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Types.LaunchTemplateName)
mltLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE mltLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

instance Core.ToQuery ModifyLaunchTemplate where
        toQuery ModifyLaunchTemplate{..}
          = Core.toQueryPair "Action" ("ModifyLaunchTemplate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SetDefaultVersion")
                defaultVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateId")
                launchTemplateId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateName")
                launchTemplateName

instance Core.ToHeaders ModifyLaunchTemplate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyLaunchTemplate where
        type Rs ModifyLaunchTemplate = ModifyLaunchTemplateResponse
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
                 ModifyLaunchTemplateResponse' Core.<$>
                   (x Core..@? "launchTemplate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyLaunchTemplateResponse' smart constructor.
data ModifyLaunchTemplateResponse = ModifyLaunchTemplateResponse'
  { launchTemplate :: Core.Maybe Types.LaunchTemplate
    -- ^ Information about the launch template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyLaunchTemplateResponse' value with any optional fields omitted.
mkModifyLaunchTemplateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyLaunchTemplateResponse
mkModifyLaunchTemplateResponse responseStatus
  = ModifyLaunchTemplateResponse'{launchTemplate = Core.Nothing,
                                  responseStatus}

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltrrsLaunchTemplate :: Lens.Lens' ModifyLaunchTemplateResponse (Core.Maybe Types.LaunchTemplate)
mltrrsLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE mltrrsLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mltrrsResponseStatus :: Lens.Lens' ModifyLaunchTemplateResponse Core.Int
mltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
