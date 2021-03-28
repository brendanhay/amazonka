{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployments. The maximum number of deployments that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetDeployments
    (
    -- * Creating a request
      BatchGetDeployments (..)
    , mkBatchGetDeployments
    -- ** Request lenses
    , bgdDeploymentIds

    -- * Destructuring the response
    , BatchGetDeploymentsResponse (..)
    , mkBatchGetDeploymentsResponse
    -- ** Response lenses
    , bgdrrsDeploymentsInfo
    , bgdrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetDeployments@ operation. 
--
-- /See:/ 'mkBatchGetDeployments' smart constructor.
newtype BatchGetDeployments = BatchGetDeployments'
  { deploymentIds :: [Types.DeploymentId]
    -- ^ A list of deployment IDs, separated by spaces. The maximum number of deployment IDs you can specify is 25.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetDeployments' value with any optional fields omitted.
mkBatchGetDeployments
    :: BatchGetDeployments
mkBatchGetDeployments
  = BatchGetDeployments'{deploymentIds = Core.mempty}

-- | A list of deployment IDs, separated by spaces. The maximum number of deployment IDs you can specify is 25.
--
-- /Note:/ Consider using 'deploymentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdDeploymentIds :: Lens.Lens' BatchGetDeployments [Types.DeploymentId]
bgdDeploymentIds = Lens.field @"deploymentIds"
{-# INLINEABLE bgdDeploymentIds #-}
{-# DEPRECATED deploymentIds "Use generic-lens or generic-optics with 'deploymentIds' instead"  #-}

instance Core.ToQuery BatchGetDeployments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetDeployments where
        toHeaders BatchGetDeployments{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.BatchGetDeployments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetDeployments where
        toJSON BatchGetDeployments{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("deploymentIds" Core..= deploymentIds)])

instance Core.AWSRequest BatchGetDeployments where
        type Rs BatchGetDeployments = BatchGetDeploymentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetDeploymentsResponse' Core.<$>
                   (x Core..:? "deploymentsInfo") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @BatchGetDeployments@ operation. 
--
-- /See:/ 'mkBatchGetDeploymentsResponse' smart constructor.
data BatchGetDeploymentsResponse = BatchGetDeploymentsResponse'
  { deploymentsInfo :: Core.Maybe [Types.DeploymentInfo]
    -- ^ Information about the deployments. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetDeploymentsResponse' value with any optional fields omitted.
mkBatchGetDeploymentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetDeploymentsResponse
mkBatchGetDeploymentsResponse responseStatus
  = BatchGetDeploymentsResponse'{deploymentsInfo = Core.Nothing,
                                 responseStatus}

-- | Information about the deployments. 
--
-- /Note:/ Consider using 'deploymentsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdrrsDeploymentsInfo :: Lens.Lens' BatchGetDeploymentsResponse (Core.Maybe [Types.DeploymentInfo])
bgdrrsDeploymentsInfo = Lens.field @"deploymentsInfo"
{-# INLINEABLE bgdrrsDeploymentsInfo #-}
{-# DEPRECATED deploymentsInfo "Use generic-lens or generic-optics with 'deploymentsInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdrrsResponseStatus :: Lens.Lens' BatchGetDeploymentsResponse Core.Int
bgdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
