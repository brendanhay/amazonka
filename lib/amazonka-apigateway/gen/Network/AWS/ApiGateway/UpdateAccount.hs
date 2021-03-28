{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the current 'Account' resource.
module Network.AWS.ApiGateway.UpdateAccount
    (
    -- * Creating a request
      UpdateAccount (..)
    , mkUpdateAccount
    -- ** Request lenses
    , uaPatchOperations

     -- * Destructuring the response
    , Types.Account (..)
    , Types.mkAccount
    -- ** Response lenses
    , Types.aApiKeyVersion
    , Types.aCloudwatchRoleArn
    , Types.aFeatures
    , Types.aThrottleSettings
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to change information about the current 'Account' resource.
--
-- /See:/ 'mkUpdateAccount' smart constructor.
newtype UpdateAccount = UpdateAccount'
  { patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAccount' value with any optional fields omitted.
mkUpdateAccount
    :: UpdateAccount
mkUpdateAccount = UpdateAccount'{patchOperations = Core.Nothing}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaPatchOperations :: Lens.Lens' UpdateAccount (Core.Maybe [Types.PatchOperation])
uaPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uaPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAccount where
        toHeaders UpdateAccount{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateAccount where
        toJSON UpdateAccount{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateAccount where
        type Rs UpdateAccount = Types.Account
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH, Core._rqPath = "/account",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
