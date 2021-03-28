{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants a temporary extension to the remaining quota of a usage plan associated with a specified API key.
module Network.AWS.ApiGateway.UpdateUsage
    (
    -- * Creating a request
      UpdateUsage (..)
    , mkUpdateUsage
    -- ** Request lenses
    , uuUsagePlanId
    , uuKeyId
    , uuPatchOperations

     -- * Destructuring the response
    , Types.Usage (..)
    , Types.mkUsage
    -- ** Response lenses
    , Types.uEndDate
    , Types.uItems
    , Types.uPosition
    , Types.uStartDate
    , Types.uUsagePlanId
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The PATCH request to grant a temporary extension to the remaining quota of a usage plan associated with a specified API key.
--
-- /See:/ 'mkUpdateUsage' smart constructor.
data UpdateUsage = UpdateUsage'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the usage plan associated with the usage data.
  , keyId :: Core.Text
    -- ^ [Required] The identifier of the API key associated with the usage plan in which a temporary extension is granted to the remaining quota.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUsage' value with any optional fields omitted.
mkUpdateUsage
    :: Core.Text -- ^ 'usagePlanId'
    -> Core.Text -- ^ 'keyId'
    -> UpdateUsage
mkUpdateUsage usagePlanId keyId
  = UpdateUsage'{usagePlanId, keyId, patchOperations = Core.Nothing}

-- | [Required] The Id of the usage plan associated with the usage data.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuUsagePlanId :: Lens.Lens' UpdateUsage Core.Text
uuUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE uuUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

-- | [Required] The identifier of the API key associated with the usage plan in which a temporary extension is granted to the remaining quota.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuKeyId :: Lens.Lens' UpdateUsage Core.Text
uuKeyId = Lens.field @"keyId"
{-# INLINEABLE uuKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuPatchOperations :: Lens.Lens' UpdateUsage (Core.Maybe [Types.PatchOperation])
uuPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE uuPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateUsage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUsage where
        toHeaders UpdateUsage{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateUsage where
        toJSON UpdateUsage{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateUsage where
        type Rs UpdateUsage = Types.Usage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/usageplans/" Core.<> Core.toText usagePlanId Core.<> "/keys/"
                             Core.<> Core.toText keyId
                             Core.<> "/usage",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
