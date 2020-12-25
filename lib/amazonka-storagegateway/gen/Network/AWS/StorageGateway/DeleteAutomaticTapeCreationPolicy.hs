{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the automatic tape creation policy of a gateway. If you delete this policy, new virtual tapes must be created manually. Use the Amazon Resource Name (ARN) of the gateway in your request to remove the policy.
module Network.AWS.StorageGateway.DeleteAutomaticTapeCreationPolicy
  ( -- * Creating a request
    DeleteAutomaticTapeCreationPolicy (..),
    mkDeleteAutomaticTapeCreationPolicy,

    -- ** Request lenses
    datcpGatewayARN,

    -- * Destructuring the response
    DeleteAutomaticTapeCreationPolicyResponse (..),
    mkDeleteAutomaticTapeCreationPolicyResponse,

    -- ** Response lenses
    datcprrsGatewayARN,
    datcprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDeleteAutomaticTapeCreationPolicy' smart constructor.
newtype DeleteAutomaticTapeCreationPolicy = DeleteAutomaticTapeCreationPolicy'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAutomaticTapeCreationPolicy' value with any optional fields omitted.
mkDeleteAutomaticTapeCreationPolicy ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DeleteAutomaticTapeCreationPolicy
mkDeleteAutomaticTapeCreationPolicy gatewayARN =
  DeleteAutomaticTapeCreationPolicy' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datcpGatewayARN :: Lens.Lens' DeleteAutomaticTapeCreationPolicy Types.GatewayARN
datcpGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED datcpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON DeleteAutomaticTapeCreationPolicy where
  toJSON DeleteAutomaticTapeCreationPolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DeleteAutomaticTapeCreationPolicy where
  type
    Rs DeleteAutomaticTapeCreationPolicy =
      DeleteAutomaticTapeCreationPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.DeleteAutomaticTapeCreationPolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAutomaticTapeCreationPolicyResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAutomaticTapeCreationPolicyResponse' smart constructor.
data DeleteAutomaticTapeCreationPolicyResponse = DeleteAutomaticTapeCreationPolicyResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAutomaticTapeCreationPolicyResponse' value with any optional fields omitted.
mkDeleteAutomaticTapeCreationPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAutomaticTapeCreationPolicyResponse
mkDeleteAutomaticTapeCreationPolicyResponse responseStatus =
  DeleteAutomaticTapeCreationPolicyResponse'
    { gatewayARN =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datcprrsGatewayARN :: Lens.Lens' DeleteAutomaticTapeCreationPolicyResponse (Core.Maybe Types.GatewayARN)
datcprrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED datcprrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datcprrsResponseStatus :: Lens.Lens' DeleteAutomaticTapeCreationPolicyResponse Core.Int
datcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED datcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
