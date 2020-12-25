{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PurchaseProvisionedCapacity (..),
    mkPurchaseProvisionedCapacity,

    -- ** Request lenses
    ppcAccountId,

    -- * Destructuring the response
    PurchaseProvisionedCapacityResponse (..),
    mkPurchaseProvisionedCapacityResponse,

    -- ** Response lenses
    ppcrrsCapacityId,
    ppcrrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPurchaseProvisionedCapacity' smart constructor.
newtype PurchaseProvisionedCapacity = PurchaseProvisionedCapacity'
  { -- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
    accountId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseProvisionedCapacity' value with any optional fields omitted.
mkPurchaseProvisionedCapacity ::
  -- | 'accountId'
  Types.String ->
  PurchaseProvisionedCapacity
mkPurchaseProvisionedCapacity accountId =
  PurchaseProvisionedCapacity' {accountId}

-- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcAccountId :: Lens.Lens' PurchaseProvisionedCapacity Types.String
ppcAccountId = Lens.field @"accountId"
{-# DEPRECATED ppcAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Core.FromJSON PurchaseProvisionedCapacity where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest PurchaseProvisionedCapacity where
  type
    Rs PurchaseProvisionedCapacity =
      PurchaseProvisionedCapacityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId)
                Core.<> ("/provisioned-capacity")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PurchaseProvisionedCapacityResponse'
            Core.<$> (Core.parseHeaderMaybe "x-amz-capacity-id" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPurchaseProvisionedCapacityResponse' smart constructor.
data PurchaseProvisionedCapacityResponse = PurchaseProvisionedCapacityResponse'
  { -- | The ID that identifies the provisioned capacity unit.
    capacityId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseProvisionedCapacityResponse' value with any optional fields omitted.
mkPurchaseProvisionedCapacityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PurchaseProvisionedCapacityResponse
mkPurchaseProvisionedCapacityResponse responseStatus =
  PurchaseProvisionedCapacityResponse'
    { capacityId = Core.Nothing,
      responseStatus
    }

-- | The ID that identifies the provisioned capacity unit.
--
-- /Note:/ Consider using 'capacityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcrrsCapacityId :: Lens.Lens' PurchaseProvisionedCapacityResponse (Core.Maybe Types.String)
ppcrrsCapacityId = Lens.field @"capacityId"
{-# DEPRECATED ppcrrsCapacityId "Use generic-lens or generic-optics with 'capacityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppcrrsResponseStatus :: Lens.Lens' PurchaseProvisionedCapacityResponse Core.Int
ppcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ppcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
