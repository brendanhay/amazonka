{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteUsagePlanKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan key and remove the underlying API key from the associated usage plan.
module Network.AWS.ApiGateway.DeleteUsagePlanKey
    (
    -- * Creating a request
      DeleteUsagePlanKey (..)
    , mkDeleteUsagePlanKey
    -- ** Request lenses
    , dupkUsagePlanId
    , dupkKeyId

    -- * Destructuring the response
    , DeleteUsagePlanKeyResponse (..)
    , mkDeleteUsagePlanKeyResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The DELETE request to delete a usage plan key and remove the underlying API key from the associated usage plan.
--
-- /See:/ 'mkDeleteUsagePlanKey' smart constructor.
data DeleteUsagePlanKey = DeleteUsagePlanKey'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-deleted 'UsagePlanKey' resource representing a plan customer.
  , keyId :: Core.Text
    -- ^ [Required] The Id of the 'UsagePlanKey' resource to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUsagePlanKey' value with any optional fields omitted.
mkDeleteUsagePlanKey
    :: Core.Text -- ^ 'usagePlanId'
    -> Core.Text -- ^ 'keyId'
    -> DeleteUsagePlanKey
mkDeleteUsagePlanKey usagePlanId keyId
  = DeleteUsagePlanKey'{usagePlanId, keyId}

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-deleted 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupkUsagePlanId :: Lens.Lens' DeleteUsagePlanKey Core.Text
dupkUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE dupkUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

-- | [Required] The Id of the 'UsagePlanKey' resource to be deleted.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupkKeyId :: Lens.Lens' DeleteUsagePlanKey Core.Text
dupkKeyId = Lens.field @"keyId"
{-# INLINEABLE dupkKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery DeleteUsagePlanKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUsagePlanKey where
        toHeaders DeleteUsagePlanKey{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteUsagePlanKey where
        type Rs DeleteUsagePlanKey = DeleteUsagePlanKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/usageplans/" Core.<> Core.toText usagePlanId Core.<> "/keys/"
                             Core.<> Core.toText keyId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteUsagePlanKeyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUsagePlanKeyResponse' smart constructor.
data DeleteUsagePlanKeyResponse = DeleteUsagePlanKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUsagePlanKeyResponse' value with any optional fields omitted.
mkDeleteUsagePlanKeyResponse
    :: DeleteUsagePlanKeyResponse
mkDeleteUsagePlanKeyResponse = DeleteUsagePlanKeyResponse'
