{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration.
module Network.AWS.ApiGateway.UpdateIntegration
    (
    -- * Creating a request
      UpdateIntegration (..)
    , mkUpdateIntegration
    -- ** Request lenses
    , uiRestApiId
    , uiResourceId
    , uiHttpMethod
    , uiPatchOperations

     -- * Destructuring the response
    , Types.Integration (..)
    , Types.mkIntegration
    -- ** Response lenses
    , Types.iCacheKeyParameters
    , Types.iCacheNamespace
    , Types.iConnectionId
    , Types.iConnectionType
    , Types.iContentHandling
    , Types.iCredentials
    , Types.iHttpMethod
    , Types.iIntegrationResponses
    , Types.iPassthroughBehavior
    , Types.iRequestParameters
    , Types.iRequestTemplates
    , Types.iTimeoutInMillis
    , Types.iTlsConfig
    , Types.iType
    , Types.iUri
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents an update integration request.
--
-- /See:/ 'mkUpdateIntegration' smart constructor.
data UpdateIntegration = UpdateIntegration'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Represents an update integration request's resource identifier.
  , httpMethod :: Core.Text
    -- ^ [Required] Represents an update integration request's HTTP method.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIntegration' value with any optional fields omitted.
mkUpdateIntegration
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> UpdateIntegration
mkUpdateIntegration restApiId resourceId httpMethod
  = UpdateIntegration'{restApiId, resourceId, httpMethod,
                       patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiRestApiId :: Lens.Lens' UpdateIntegration Core.Text
uiRestApiId = Lens.field @"restApiId"
{-# INLINEABLE uiRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Represents an update integration request's resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiResourceId :: Lens.Lens' UpdateIntegration Core.Text
uiResourceId = Lens.field @"resourceId"
{-# INLINEABLE uiResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Represents an update integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiHttpMethod :: Lens.Lens' UpdateIntegration Core.Text
uiHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE uiHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiPatchOperations :: Lens.Lens' UpdateIntegration (Core.Maybe [Types.PatchOperation])
uiPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uiPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateIntegration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateIntegration where
        toHeaders UpdateIntegration{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateIntegration where
        toJSON UpdateIntegration{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateIntegration where
        type Rs UpdateIntegration = Types.Integration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/integration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
