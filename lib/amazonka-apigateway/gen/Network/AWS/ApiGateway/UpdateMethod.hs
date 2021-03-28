{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'Method' resource.
module Network.AWS.ApiGateway.UpdateMethod
    (
    -- * Creating a request
      UpdateMethod (..)
    , mkUpdateMethod
    -- ** Request lenses
    , umfRestApiId
    , umfResourceId
    , umfHttpMethod
    , umfPatchOperations

     -- * Destructuring the response
    , Types.Method (..)
    , Types.mkMethod
    -- ** Response lenses
    , Types.mApiKeyRequired
    , Types.mAuthorizationScopes
    , Types.mAuthorizationType
    , Types.mAuthorizerId
    , Types.mHttpMethod
    , Types.mMethodIntegration
    , Types.mMethodResponses
    , Types.mOperationName
    , Types.mRequestModels
    , Types.mRequestParameters
    , Types.mRequestValidatorId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an existing 'Method' resource.
--
-- /See:/ 'mkUpdateMethod' smart constructor.
data UpdateMethod = UpdateMethod'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] The 'Resource' identifier for the 'Method' resource.
  , httpMethod :: Core.Text
    -- ^ [Required] The HTTP verb of the 'Method' resource.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMethod' value with any optional fields omitted.
mkUpdateMethod
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> UpdateMethod
mkUpdateMethod restApiId resourceId httpMethod
  = UpdateMethod'{restApiId, resourceId, httpMethod,
                  patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfRestApiId :: Lens.Lens' UpdateMethod Core.Text
umfRestApiId = Lens.field @"restApiId"
{-# INLINEABLE umfRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The 'Resource' identifier for the 'Method' resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfResourceId :: Lens.Lens' UpdateMethod Core.Text
umfResourceId = Lens.field @"resourceId"
{-# INLINEABLE umfResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] The HTTP verb of the 'Method' resource.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfHttpMethod :: Lens.Lens' UpdateMethod Core.Text
umfHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE umfHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umfPatchOperations :: Lens.Lens' UpdateMethod (Core.Maybe [Types.PatchOperation])
umfPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE umfPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateMethod where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMethod where
        toHeaders UpdateMethod{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateMethod where
        toJSON UpdateMethod{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateMethod where
        type Rs UpdateMethod = Types.Method
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
