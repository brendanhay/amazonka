{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.PurchaseProvisionedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation purchases a provisioned capacity unit for an AWS account. 
module Network.AWS.Glacier.PurchaseProvisionedCapacity
    (
    -- * Creating a request
      PurchaseProvisionedCapacity (..)
    , mkPurchaseProvisionedCapacity
    -- ** Request lenses
    , ppcAccountId

    -- * Destructuring the response
    , PurchaseProvisionedCapacityResponse (..)
    , mkPurchaseProvisionedCapacityResponse
    -- ** Response lenses
    , ppcrrsCapacityId
    , ppcrrsResponseStatus
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPurchaseProvisionedCapacity' smart constructor.
newtype PurchaseProvisionedCapacity = PurchaseProvisionedCapacity'
  { accountId :: Core.Text
    -- ^ The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseProvisionedCapacity' value with any optional fields omitted.
mkPurchaseProvisionedCapacity
    :: Core.Text -- ^ 'accountId'
    -> PurchaseProvisionedCapacity
mkPurchaseProvisionedCapacity accountId
  = PurchaseProvisionedCapacity'{accountId}

-- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcAccountId :: Lens.Lens' PurchaseProvisionedCapacity Core.Text
ppcAccountId = Lens.field @"accountId"
{-# INLINEABLE ppcAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

instance Core.ToQuery PurchaseProvisionedCapacity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PurchaseProvisionedCapacity where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON PurchaseProvisionedCapacity where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest PurchaseProvisionedCapacity where
        type Rs PurchaseProvisionedCapacity =
             PurchaseProvisionedCapacityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/provisioned-capacity",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PurchaseProvisionedCapacityResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-capacity-id" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPurchaseProvisionedCapacityResponse' smart constructor.
data PurchaseProvisionedCapacityResponse = PurchaseProvisionedCapacityResponse'
  { capacityId :: Core.Maybe Core.Text
    -- ^ The ID that identifies the provisioned capacity unit.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseProvisionedCapacityResponse' value with any optional fields omitted.
mkPurchaseProvisionedCapacityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PurchaseProvisionedCapacityResponse
mkPurchaseProvisionedCapacityResponse responseStatus
  = PurchaseProvisionedCapacityResponse'{capacityId = Core.Nothing,
                                         responseStatus}

-- | The ID that identifies the provisioned capacity unit.
--
-- /Note:/ Consider using 'capacityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcrrsCapacityId :: Lens.Lens' PurchaseProvisionedCapacityResponse (Core.Maybe Core.Text)
ppcrrsCapacityId = Lens.field @"capacityId"
{-# INLINEABLE ppcrrsCapacityId #-}
{-# DEPRECATED capacityId "Use generic-lens or generic-optics with 'capacityId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcrrsResponseStatus :: Lens.Lens' PurchaseProvisionedCapacityResponse Core.Int
ppcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ppcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
