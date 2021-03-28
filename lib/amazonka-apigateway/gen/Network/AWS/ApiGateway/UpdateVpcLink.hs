{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateVpcLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'VpcLink' of a specified identifier.
module Network.AWS.ApiGateway.UpdateVpcLink
    (
    -- * Creating a request
      UpdateVpcLink (..)
    , mkUpdateVpcLink
    -- ** Request lenses
    , uvlVpcLinkId
    , uvlPatchOperations

     -- * Destructuring the response
    , Types.VpcLink (..)
    , Types.mkVpcLink
    -- ** Response lenses
    , Types.vlDescription
    , Types.vlId
    , Types.vlName
    , Types.vlStatus
    , Types.vlStatusMessage
    , Types.vlTags
    , Types.vlTargetArns
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates an existing 'VpcLink' of a specified identifier.
--
-- /See:/ 'mkUpdateVpcLink' smart constructor.
data UpdateVpcLink = UpdateVpcLink'
  { vpcLinkId :: Core.Text
    -- ^ [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVpcLink' value with any optional fields omitted.
mkUpdateVpcLink
    :: Core.Text -- ^ 'vpcLinkId'
    -> UpdateVpcLink
mkUpdateVpcLink vpcLinkId
  = UpdateVpcLink'{vpcLinkId, patchOperations = Core.Nothing}

-- | [Required] The identifier of the 'VpcLink' . It is used in an 'Integration' to reference this 'VpcLink' .
--
-- /Note:/ Consider using 'vpcLinkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvlVpcLinkId :: Lens.Lens' UpdateVpcLink Core.Text
uvlVpcLinkId = Lens.field @"vpcLinkId"
{-# INLINEABLE uvlVpcLinkId #-}
{-# DEPRECATED vpcLinkId "Use generic-lens or generic-optics with 'vpcLinkId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvlPatchOperations :: Lens.Lens' UpdateVpcLink (Core.Maybe [Types.PatchOperation])
uvlPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uvlPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateVpcLink where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateVpcLink where
        toHeaders UpdateVpcLink{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateVpcLink where
        toJSON UpdateVpcLink{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateVpcLink where
        type Rs UpdateVpcLink = Types.VpcLink
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/vpclinks/" Core.<> Core.toText vpcLinkId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
