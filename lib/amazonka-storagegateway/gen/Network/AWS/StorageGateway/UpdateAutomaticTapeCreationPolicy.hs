{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the automatic tape creation policy of a gateway. Use this to update the policy with a new set of automatic tape creation rules. This is only supported for tape gateways.
--
-- By default, there is no automatic tape creation policy.
module Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
  ( -- * Creating a request
    UpdateAutomaticTapeCreationPolicy (..),
    mkUpdateAutomaticTapeCreationPolicy,

    -- ** Request lenses
    uatcpAutomaticTapeCreationRules,
    uatcpGatewayARN,

    -- * Destructuring the response
    UpdateAutomaticTapeCreationPolicyResponse (..),
    mkUpdateAutomaticTapeCreationPolicyResponse,

    -- ** Response lenses
    uatcprrsGatewayARN,
    uatcprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkUpdateAutomaticTapeCreationPolicy' smart constructor.
data UpdateAutomaticTapeCreationPolicy = UpdateAutomaticTapeCreationPolicy'
  { -- | An automatic tape creation policy consists of a list of automatic tape creation rules. The rules determine when and how to automatically create new tapes.
    automaticTapeCreationRules :: Core.NonEmpty Types.AutomaticTapeCreationRule,
    gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAutomaticTapeCreationPolicy' value with any optional fields omitted.
mkUpdateAutomaticTapeCreationPolicy ::
  -- | 'automaticTapeCreationRules'
  Core.NonEmpty Types.AutomaticTapeCreationRule ->
  -- | 'gatewayARN'
  Types.GatewayARN ->
  UpdateAutomaticTapeCreationPolicy
mkUpdateAutomaticTapeCreationPolicy
  automaticTapeCreationRules
  gatewayARN =
    UpdateAutomaticTapeCreationPolicy'
      { automaticTapeCreationRules,
        gatewayARN
      }

-- | An automatic tape creation policy consists of a list of automatic tape creation rules. The rules determine when and how to automatically create new tapes.
--
-- /Note:/ Consider using 'automaticTapeCreationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcpAutomaticTapeCreationRules :: Lens.Lens' UpdateAutomaticTapeCreationPolicy (Core.NonEmpty Types.AutomaticTapeCreationRule)
uatcpAutomaticTapeCreationRules = Lens.field @"automaticTapeCreationRules"
{-# DEPRECATED uatcpAutomaticTapeCreationRules "Use generic-lens or generic-optics with 'automaticTapeCreationRules' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcpGatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicy Types.GatewayARN
uatcpGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED uatcpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON UpdateAutomaticTapeCreationPolicy where
  toJSON UpdateAutomaticTapeCreationPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AutomaticTapeCreationRules" Core..= automaticTapeCreationRules),
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.AWSRequest UpdateAutomaticTapeCreationPolicy where
  type
    Rs UpdateAutomaticTapeCreationPolicy =
      UpdateAutomaticTapeCreationPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.UpdateAutomaticTapeCreationPolicy"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAutomaticTapeCreationPolicyResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAutomaticTapeCreationPolicyResponse' smart constructor.
data UpdateAutomaticTapeCreationPolicyResponse = UpdateAutomaticTapeCreationPolicyResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAutomaticTapeCreationPolicyResponse' value with any optional fields omitted.
mkUpdateAutomaticTapeCreationPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateAutomaticTapeCreationPolicyResponse
mkUpdateAutomaticTapeCreationPolicyResponse responseStatus =
  UpdateAutomaticTapeCreationPolicyResponse'
    { gatewayARN =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcprrsGatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse (Core.Maybe Types.GatewayARN)
uatcprrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED uatcprrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcprrsResponseStatus :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse Core.Int
uatcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uatcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
