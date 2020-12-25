{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines whether a product code is associated with an instance. This action can only be used by the owner of the product code. It is useful when a product code owner must verify whether another user's instance is eligible for support.
module Network.AWS.EC2.ConfirmProductInstance
  ( -- * Creating a request
    ConfirmProductInstance (..),
    mkConfirmProductInstance,

    -- ** Request lenses
    cpiInstanceId,
    cpiProductCode,
    cpiDryRun,

    -- * Destructuring the response
    ConfirmProductInstanceResponse (..),
    mkConfirmProductInstanceResponse,

    -- ** Response lenses
    cpirrsOwnerId,
    cpirrsReturn,
    cpirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkConfirmProductInstance' smart constructor.
data ConfirmProductInstance = ConfirmProductInstance'
  { -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | The product code. This must be a product code that you own.
    productCode :: Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmProductInstance' value with any optional fields omitted.
mkConfirmProductInstance ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'productCode'
  Types.String ->
  ConfirmProductInstance
mkConfirmProductInstance instanceId productCode =
  ConfirmProductInstance'
    { instanceId,
      productCode,
      dryRun = Core.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiInstanceId :: Lens.Lens' ConfirmProductInstance Types.InstanceId
cpiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED cpiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The product code. This must be a product code that you own.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiProductCode :: Lens.Lens' ConfirmProductInstance Types.String
cpiProductCode = Lens.field @"productCode"
{-# DEPRECATED cpiProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpiDryRun :: Lens.Lens' ConfirmProductInstance (Core.Maybe Core.Bool)
cpiDryRun = Lens.field @"dryRun"
{-# DEPRECATED cpiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest ConfirmProductInstance where
  type Rs ConfirmProductInstance = ConfirmProductInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ConfirmProductInstance")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "ProductCode" productCode)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ConfirmProductInstanceResponse'
            Core.<$> (x Core..@? "ownerId")
            Core.<*> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkConfirmProductInstanceResponse' smart constructor.
data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse'
  { -- | The AWS account ID of the instance owner. This is only present if the product code is attached to the instance.
    ownerId :: Core.Maybe Types.String,
    -- | The return value of the request. Returns @true@ if the specified product code is owned by the requester and associated with the specified instance.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfirmProductInstanceResponse' value with any optional fields omitted.
mkConfirmProductInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ConfirmProductInstanceResponse
mkConfirmProductInstanceResponse responseStatus =
  ConfirmProductInstanceResponse'
    { ownerId = Core.Nothing,
      return = Core.Nothing,
      responseStatus
    }

-- | The AWS account ID of the instance owner. This is only present if the product code is attached to the instance.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirrsOwnerId :: Lens.Lens' ConfirmProductInstanceResponse (Core.Maybe Types.String)
cpirrsOwnerId = Lens.field @"ownerId"
{-# DEPRECATED cpirrsOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The return value of the request. Returns @true@ if the specified product code is owned by the requester and associated with the specified instance.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirrsReturn :: Lens.Lens' ConfirmProductInstanceResponse (Core.Maybe Core.Bool)
cpirrsReturn = Lens.field @"return"
{-# DEPRECATED cpirrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpirrsResponseStatus :: Lens.Lens' ConfirmProductInstanceResponse Core.Int
cpirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
