{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Resource' resource.
module Network.AWS.ApiGateway.UpdateResource
    (
    -- * Creating a request
      UpdateResource (..)
    , mkUpdateResource
    -- ** Request lenses
    , urRestApiId
    , urResourceId
    , urPatchOperations

     -- * Destructuring the response
    , Types.Resource (..)
    , Types.mkResource
    -- ** Response lenses
    , Types.rId
    , Types.rParentId
    , Types.rPath
    , Types.rPathPart
    , Types.rResourceMethods
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to change information about a 'Resource' resource.
--
-- /See:/ 'mkUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The identifier of the 'Resource' resource.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateResource' value with any optional fields omitted.
mkUpdateResource
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> UpdateResource
mkUpdateResource restApiId resourceId
  = UpdateResource'{restApiId, resourceId,
                    patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRestApiId :: Lens.Lens' UpdateResource Core.Text
urRestApiId = Lens.field @"restApiId"
{-# INLINEABLE urRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'Resource' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceId :: Lens.Lens' UpdateResource Core.Text
urResourceId = Lens.field @"resourceId"
{-# INLINEABLE urResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urPatchOperations :: Lens.Lens' UpdateResource (Core.Maybe [Types.PatchOperation])
urPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE urPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateResource where
        toHeaders UpdateResource{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateResource where
        toJSON UpdateResource{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateResource where
        type Rs UpdateResource = Types.Resource
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
