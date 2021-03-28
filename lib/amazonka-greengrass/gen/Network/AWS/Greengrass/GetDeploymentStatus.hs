{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a deployment.
module Network.AWS.Greengrass.GetDeploymentStatus
    (
    -- * Creating a request
      GetDeploymentStatus (..)
    , mkGetDeploymentStatus
    -- ** Request lenses
    , gdsGroupId
    , gdsDeploymentId

    -- * Destructuring the response
    , GetDeploymentStatusResponse (..)
    , mkGetDeploymentStatusResponse
    -- ** Response lenses
    , gdsrrsDeploymentStatus
    , gdsrrsDeploymentType
    , gdsrrsErrorDetails
    , gdsrrsErrorMessage
    , gdsrrsUpdatedAt
    , gdsrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDeploymentStatus' smart constructor.
data GetDeploymentStatus = GetDeploymentStatus'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  , deploymentId :: Core.Text
    -- ^ The ID of the deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeploymentStatus' value with any optional fields omitted.
mkGetDeploymentStatus
    :: Core.Text -- ^ 'groupId'
    -> Core.Text -- ^ 'deploymentId'
    -> GetDeploymentStatus
mkGetDeploymentStatus groupId deploymentId
  = GetDeploymentStatus'{groupId, deploymentId}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsGroupId :: Lens.Lens' GetDeploymentStatus Core.Text
gdsGroupId = Lens.field @"groupId"
{-# INLINEABLE gdsGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDeploymentId :: Lens.Lens' GetDeploymentStatus Core.Text
gdsDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE gdsDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

instance Core.ToQuery GetDeploymentStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDeploymentStatus where
        toHeaders GetDeploymentStatus{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetDeploymentStatus where
        type Rs GetDeploymentStatus = GetDeploymentStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<>
                             "/deployments/"
                             Core.<> Core.toText deploymentId
                             Core.<> "/status",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeploymentStatusResponse' Core.<$>
                   (x Core..:? "DeploymentStatus") Core.<*>
                     x Core..:? "DeploymentType"
                     Core.<*> x Core..:? "ErrorDetails"
                     Core.<*> x Core..:? "ErrorMessage"
                     Core.<*> x Core..:? "UpdatedAt"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDeploymentStatusResponse' smart constructor.
data GetDeploymentStatusResponse = GetDeploymentStatusResponse'
  { deploymentStatus :: Core.Maybe Core.Text
    -- ^ The status of the deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
  , deploymentType :: Core.Maybe Types.DeploymentType
    -- ^ The type of the deployment.
  , errorDetails :: Core.Maybe [Types.ErrorDetail]
    -- ^ Error details
  , errorMessage :: Core.Maybe Core.Text
    -- ^ Error message
  , updatedAt :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the deployment status was updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeploymentStatusResponse' value with any optional fields omitted.
mkGetDeploymentStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeploymentStatusResponse
mkGetDeploymentStatusResponse responseStatus
  = GetDeploymentStatusResponse'{deploymentStatus = Core.Nothing,
                                 deploymentType = Core.Nothing, errorDetails = Core.Nothing,
                                 errorMessage = Core.Nothing, updatedAt = Core.Nothing,
                                 responseStatus}

-- | The status of the deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDeploymentStatus :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
gdsrrsDeploymentStatus = Lens.field @"deploymentStatus"
{-# INLINEABLE gdsrrsDeploymentStatus #-}
{-# DEPRECATED deploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead"  #-}

-- | The type of the deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDeploymentType :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Types.DeploymentType)
gdsrrsDeploymentType = Lens.field @"deploymentType"
{-# INLINEABLE gdsrrsDeploymentType #-}
{-# DEPRECATED deploymentType "Use generic-lens or generic-optics with 'deploymentType' instead"  #-}

-- | Error details
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsErrorDetails :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe [Types.ErrorDetail])
gdsrrsErrorDetails = Lens.field @"errorDetails"
{-# INLINEABLE gdsrrsErrorDetails #-}
{-# DEPRECATED errorDetails "Use generic-lens or generic-optics with 'errorDetails' instead"  #-}

-- | Error message
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsErrorMessage :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
gdsrrsErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE gdsrrsErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The time, in milliseconds since the epoch, when the deployment status was updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsUpdatedAt :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
gdsrrsUpdatedAt = Lens.field @"updatedAt"
{-# INLINEABLE gdsrrsUpdatedAt #-}
{-# DEPRECATED updatedAt "Use generic-lens or generic-optics with 'updatedAt' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GetDeploymentStatusResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
