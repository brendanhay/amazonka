{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a usage plan of a given plan Id.
module Network.AWS.ApiGateway.UpdateUsagePlan
    (
    -- * Creating a request
      UpdateUsagePlan (..)
    , mkUpdateUsagePlan
    -- ** Request lenses
    , uupUsagePlanId
    , uupPatchOperations

     -- * Destructuring the response
    , Types.UsagePlan (..)
    , Types.mkUsagePlan
    -- ** Response lenses
    , Types.upApiStages
    , Types.upDescription
    , Types.upId
    , Types.upName
    , Types.upProductCode
    , Types.upQuota
    , Types.upTags
    , Types.upThrottle
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The PATCH request to update a usage plan of a given plan Id.
--
-- /See:/ 'mkUpdateUsagePlan' smart constructor.
data UpdateUsagePlan = UpdateUsagePlan'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the to-be-updated usage plan.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUsagePlan' value with any optional fields omitted.
mkUpdateUsagePlan
    :: Core.Text -- ^ 'usagePlanId'
    -> UpdateUsagePlan
mkUpdateUsagePlan usagePlanId
  = UpdateUsagePlan'{usagePlanId, patchOperations = Core.Nothing}

-- | [Required] The Id of the to-be-updated usage plan.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUsagePlanId :: Lens.Lens' UpdateUsagePlan Core.Text
uupUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE uupUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupPatchOperations :: Lens.Lens' UpdateUsagePlan (Core.Maybe [Types.PatchOperation])
uupPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uupPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateUsagePlan where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUsagePlan where
        toHeaders UpdateUsagePlan{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateUsagePlan where
        toJSON UpdateUsagePlan{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateUsagePlan where
        type Rs UpdateUsagePlan = Types.UsagePlan
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/usageplans/" Core.<> Core.toText usagePlanId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
