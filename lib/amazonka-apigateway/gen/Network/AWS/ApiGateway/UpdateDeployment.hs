{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Deployment' resource.
module Network.AWS.ApiGateway.UpdateDeployment
    (
    -- * Creating a request
      UpdateDeployment (..)
    , mkUpdateDeployment
    -- ** Request lenses
    , udRestApiId
    , udDeploymentId
    , udPatchOperations

     -- * Destructuring the response
    , Types.Deployment (..)
    , Types.mkDeployment
    -- ** Response lenses
    , Types.dApiSummary
    , Types.dCreatedDate
    , Types.dDescription
    , Types.dId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to change information about a 'Deployment' resource.
--
-- /See:/ 'mkUpdateDeployment' smart constructor.
data UpdateDeployment = UpdateDeployment'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , deploymentId :: Core.Text
    -- ^ The replacement identifier for the 'Deployment' resource to change information about.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeployment' value with any optional fields omitted.
mkUpdateDeployment
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'deploymentId'
    -> UpdateDeployment
mkUpdateDeployment restApiId deploymentId
  = UpdateDeployment'{restApiId, deploymentId,
                      patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udRestApiId :: Lens.Lens' UpdateDeployment Core.Text
udRestApiId = Lens.field @"restApiId"
{-# INLINEABLE udRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The replacement identifier for the 'Deployment' resource to change information about.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeploymentId :: Lens.Lens' UpdateDeployment Core.Text
udDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE udDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udPatchOperations :: Lens.Lens' UpdateDeployment (Core.Maybe [Types.PatchOperation])
udPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE udPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDeployment where
        toHeaders UpdateDeployment{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateDeployment where
        toJSON UpdateDeployment{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateDeployment where
        type Rs UpdateDeployment = Types.Deployment
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/deployments/"
                             Core.<> Core.toText deploymentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
