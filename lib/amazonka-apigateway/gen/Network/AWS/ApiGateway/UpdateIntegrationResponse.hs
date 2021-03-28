{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateIntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration response.
module Network.AWS.ApiGateway.UpdateIntegrationResponse
    (
    -- * Creating a request
      UpdateIntegrationResponse (..)
    , mkUpdateIntegrationResponse
    -- ** Request lenses
    , uirRestApiId
    , uirResourceId
    , uirHttpMethod
    , uirStatusCode
    , uirPatchOperations

     -- * Destructuring the response
    , Types.IntegrationResponse (..)
    , Types.mkIntegrationResponse
    -- ** Response lenses
    , Types.irContentHandling
    , Types.irResponseParameters
    , Types.irResponseTemplates
    , Types.irSelectionPattern
    , Types.irStatusCode
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents an update integration response request.
--
-- /See:/ 'mkUpdateIntegrationResponse' smart constructor.
data UpdateIntegrationResponse = UpdateIntegrationResponse'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies an update integration response request's resource identifier.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies an update integration response request's HTTP method.
  , statusCode :: Types.StatusCode
    -- ^ [Required] Specifies an update integration response request's status code.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIntegrationResponse' value with any optional fields omitted.
mkUpdateIntegrationResponse
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.StatusCode -- ^ 'statusCode'
    -> UpdateIntegrationResponse
mkUpdateIntegrationResponse restApiId resourceId httpMethod
  statusCode
  = UpdateIntegrationResponse'{restApiId, resourceId, httpMethod,
                               statusCode, patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirRestApiId :: Lens.Lens' UpdateIntegrationResponse Core.Text
uirRestApiId = Lens.field @"restApiId"
{-# INLINEABLE uirRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies an update integration response request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirResourceId :: Lens.Lens' UpdateIntegrationResponse Core.Text
uirResourceId = Lens.field @"resourceId"
{-# INLINEABLE uirResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies an update integration response request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirHttpMethod :: Lens.Lens' UpdateIntegrationResponse Core.Text
uirHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE uirHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] Specifies an update integration response request's status code.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirStatusCode :: Lens.Lens' UpdateIntegrationResponse Types.StatusCode
uirStatusCode = Lens.field @"statusCode"
{-# INLINEABLE uirStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirPatchOperations :: Lens.Lens' UpdateIntegrationResponse (Core.Maybe [Types.PatchOperation])
uirPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uirPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateIntegrationResponse where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateIntegrationResponse where
        toHeaders UpdateIntegrationResponse{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateIntegrationResponse where
        toJSON UpdateIntegrationResponse{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateIntegrationResponse where
        type Rs UpdateIntegrationResponse = Types.IntegrationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/integration/responses/"
                             Core.<> Core.toText statusCode,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
