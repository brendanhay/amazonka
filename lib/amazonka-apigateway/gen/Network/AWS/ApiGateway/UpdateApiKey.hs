{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an 'ApiKey' resource.
module Network.AWS.ApiGateway.UpdateApiKey
    (
    -- * Creating a request
      UpdateApiKey (..)
    , mkUpdateApiKey
    -- ** Request lenses
    , uakApiKey
    , uakPatchOperations

     -- * Destructuring the response
    , Types.ApiKey (..)
    , Types.mkApiKey
    -- ** Response lenses
    , Types.akCreatedDate
    , Types.akCustomerId
    , Types.akDescription
    , Types.akEnabled
    , Types.akId
    , Types.akLastUpdatedDate
    , Types.akName
    , Types.akStageKeys
    , Types.akTags
    , Types.akValue
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about an 'ApiKey' resource.
--
-- /See:/ 'mkUpdateApiKey' smart constructor.
data UpdateApiKey = UpdateApiKey'
  { apiKey :: Core.Text
    -- ^ [Required] The identifier of the 'ApiKey' resource to be updated.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApiKey' value with any optional fields omitted.
mkUpdateApiKey
    :: Core.Text -- ^ 'apiKey'
    -> UpdateApiKey
mkUpdateApiKey apiKey
  = UpdateApiKey'{apiKey, patchOperations = Core.Nothing}

-- | [Required] The identifier of the 'ApiKey' resource to be updated.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakApiKey :: Lens.Lens' UpdateApiKey Core.Text
uakApiKey = Lens.field @"apiKey"
{-# INLINEABLE uakApiKey #-}
{-# DEPRECATED apiKey "Use generic-lens or generic-optics with 'apiKey' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakPatchOperations :: Lens.Lens' UpdateApiKey (Core.Maybe [Types.PatchOperation])
uakPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uakPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateApiKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApiKey where
        toHeaders UpdateApiKey{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateApiKey where
        toJSON UpdateApiKey{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateApiKey where
        type Rs UpdateApiKey = Types.ApiKey
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/apikeys/" Core.<> Core.toText apiKey,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
