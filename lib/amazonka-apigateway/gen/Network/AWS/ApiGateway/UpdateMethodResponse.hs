{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateMethodResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'MethodResponse' resource.
module Network.AWS.ApiGateway.UpdateMethodResponse
    (
    -- * Creating a request
      UpdateMethodResponse (..)
    , mkUpdateMethodResponse
    -- ** Request lenses
    , umrRestApiId
    , umrResourceId
    , umrHttpMethod
    , umrStatusCode
    , umrPatchOperations

     -- * Destructuring the response
    , Types.MethodResponse (..)
    , Types.mkMethodResponse
    -- ** Response lenses
    , Types.mrResponseModels
    , Types.mrResponseParameters
    , Types.mrStatusCode
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an existing 'MethodResponse' resource.
--
-- /See:/ 'mkUpdateMethodResponse' smart constructor.
data UpdateMethodResponse = UpdateMethodResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The 'Resource' identifier for the 'MethodResponse' resource.
  , httpMethod :: Core.Text
    -- ^ [Required] The HTTP verb of the 'Method' resource.
  , statusCode :: Types.StatusCode
    -- ^ [Required] The status code for the 'MethodResponse' resource.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMethodResponse' value with any optional fields omitted.
mkUpdateMethodResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> UpdateMethodResponse
mkUpdateMethodResponse restApiId resourceId httpMethod statusCode
  = UpdateMethodResponse'{restApiId, resourceId, httpMethod,
                          statusCode, patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrRestApiId :: Lens.Lens' UpdateMethodResponse Core.Text
umrRestApiId = Lens.field @"restApiId"
{-# INLINEABLE umrRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The 'Resource' identifier for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrResourceId :: Lens.Lens' UpdateMethodResponse Core.Text
umrResourceId = Lens.field @"resourceId"
{-# INLINEABLE umrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrHttpMethod :: Lens.Lens' UpdateMethodResponse Core.Text
umrHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE umrHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] The status code for the 'MethodResponse' resource.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrStatusCode :: Lens.Lens' UpdateMethodResponse Types.StatusCode
umrStatusCode = Lens.field @"statusCode"
{-# INLINEABLE umrStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrPatchOperations :: Lens.Lens' UpdateMethodResponse (Core.Maybe [Types.PatchOperation])
umrPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE umrPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateMethodResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMethodResponse where
        toHeaders UpdateMethodResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateMethodResponse where
        toJSON UpdateMethodResponse{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateMethodResponse where
        type Rs UpdateMethodResponse = Types.MethodResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/responses/"
                             Core.<> Core.toText statusCode,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
