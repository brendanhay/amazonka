{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Deployment' resource. Deleting a deployment will only succeed if there are no 'Stage' resources associated with it.
module Network.AWS.ApiGateway.DeleteDeployment
    (
    -- * Creating a request
      DeleteDeployment (..)
    , mkDeleteDeployment
    -- ** Request lenses
    , ddRestApiId
    , ddDeploymentId

    -- * Destructuring the response
    , DeleteDeploymentResponse (..)
    , mkDeleteDeploymentResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to delete a 'Deployment' resource.
--
-- /See:/ 'mkDeleteDeployment' smart constructor.
data DeleteDeployment = DeleteDeployment'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , deploymentId :: Core.Text
    -- ^ [Required] The identifier of the 'Deployment' resource to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeployment' value with any optional fields omitted.
mkDeleteDeployment
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'deploymentId'
    -> DeleteDeployment
mkDeleteDeployment restApiId deploymentId
  = DeleteDeployment'{restApiId, deploymentId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRestApiId :: Lens.Lens' DeleteDeployment Core.Text
ddRestApiId = Lens.field @"restApiId"
{-# INLINEABLE ddRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'Deployment' resource to delete.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeploymentId :: Lens.Lens' DeleteDeployment Core.Text
ddDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE ddDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

instance Core.ToQuery DeleteDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDeployment where
        toHeaders DeleteDeployment{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteDeployment where
        type Rs DeleteDeployment = DeleteDeploymentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/deployments/"
                             Core.<> Core.toText deploymentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteDeploymentResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDeploymentResponse' smart constructor.
data DeleteDeploymentResponse = DeleteDeploymentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentResponse' value with any optional fields omitted.
mkDeleteDeploymentResponse
    :: DeleteDeploymentResponse
mkDeleteDeploymentResponse = DeleteDeploymentResponse'
