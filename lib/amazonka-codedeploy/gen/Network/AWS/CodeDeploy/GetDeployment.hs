{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment.
module Network.AWS.CodeDeploy.GetDeployment
    (
    -- * Creating a request
      GetDeployment (..)
    , mkGetDeployment
    -- ** Request lenses
    , gdDeploymentId

    -- * Destructuring the response
    , GetDeploymentResponse (..)
    , mkGetDeploymentResponse
    -- ** Response lenses
    , gdrrsDeploymentInfo
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetDeployment@ operation.
--
-- /See:/ 'mkGetDeployment' smart constructor.
newtype GetDeployment = GetDeployment'
  { deploymentId :: Types.DeploymentId
    -- ^ The unique ID of a deployment associated with the IAM user or AWS account. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeployment' value with any optional fields omitted.
mkGetDeployment
    :: Types.DeploymentId -- ^ 'deploymentId'
    -> GetDeployment
mkGetDeployment deploymentId = GetDeployment'{deploymentId}

-- | The unique ID of a deployment associated with the IAM user or AWS account. 
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDeploymentId :: Lens.Lens' GetDeployment Types.DeploymentId
gdDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE gdDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

instance Core.ToQuery GetDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDeployment where
        toHeaders GetDeployment{..}
          = Core.pure ("X-Amz-Target", "CodeDeploy_20141006.GetDeployment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDeployment where
        toJSON GetDeployment{..}
          = Core.object
              (Core.catMaybes [Core.Just ("deploymentId" Core..= deploymentId)])

instance Core.AWSRequest GetDeployment where
        type Rs GetDeployment = GetDeploymentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeploymentResponse' Core.<$>
                   (x Core..:? "deploymentInfo") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetDeployment@ operation.
--
-- /See:/ 'mkGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { deploymentInfo :: Core.Maybe Types.DeploymentInfo
    -- ^ Information about the deployment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDeploymentResponse' value with any optional fields omitted.
mkGetDeploymentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeploymentResponse
mkGetDeploymentResponse responseStatus
  = GetDeploymentResponse'{deploymentInfo = Core.Nothing,
                           responseStatus}

-- | Information about the deployment.
--
-- /Note:/ Consider using 'deploymentInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDeploymentInfo :: Lens.Lens' GetDeploymentResponse (Core.Maybe Types.DeploymentInfo)
gdrrsDeploymentInfo = Lens.field @"deploymentInfo"
{-# INLINEABLE gdrrsDeploymentInfo #-}
{-# DEPRECATED deploymentInfo "Use generic-lens or generic-optics with 'deploymentInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDeploymentResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
