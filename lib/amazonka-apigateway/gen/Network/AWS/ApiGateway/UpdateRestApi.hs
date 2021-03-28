{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateRestApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the specified API.
module Network.AWS.ApiGateway.UpdateRestApi
    (
    -- * Creating a request
      UpdateRestApi (..)
    , mkUpdateRestApi
    -- ** Request lenses
    , uraRestApiId
    , uraPatchOperations

     -- * Destructuring the response
    , Types.RestApi (..)
    , Types.mkRestApi
    -- ** Response lenses
    , Types.raApiKeySource
    , Types.raBinaryMediaTypes
    , Types.raCreatedDate
    , Types.raDescription
    , Types.raDisableExecuteApiEndpoint
    , Types.raEndpointConfiguration
    , Types.raId
    , Types.raMinimumCompressionSize
    , Types.raName
    , Types.raPolicy
    , Types.raTags
    , Types.raVersion
    , Types.raWarnings
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an existing 'RestApi' resource in your collection.
--
-- /See:/ 'mkUpdateRestApi' smart constructor.
data UpdateRestApi = UpdateRestApi'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRestApi' value with any optional fields omitted.
mkUpdateRestApi
    :: Core.Text -- ^ 'restApiId'
    -> UpdateRestApi
mkUpdateRestApi restApiId
  = UpdateRestApi'{restApiId, patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraRestApiId :: Lens.Lens' UpdateRestApi Core.Text
uraRestApiId = Lens.field @"restApiId"
{-# INLINEABLE uraRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraPatchOperations :: Lens.Lens' UpdateRestApi (Core.Maybe [Types.PatchOperation])
uraPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uraPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateRestApi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRestApi where
        toHeaders UpdateRestApi{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateRestApi where
        toJSON UpdateRestApi{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateRestApi where
        type Rs UpdateRestApi = Types.RestApi
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/restapis/" Core.<> Core.toText restApiId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
