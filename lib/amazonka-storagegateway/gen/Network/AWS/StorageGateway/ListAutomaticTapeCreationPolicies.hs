{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the automatic tape creation policies for a gateway. If there are no automatic tape creation policies for the gateway, it returns an empty list.
--
-- This operation is only supported for tape gateways.
module Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
  ( -- * Creating a request
    ListAutomaticTapeCreationPolicies (..),
    mkListAutomaticTapeCreationPolicies,

    -- ** Request lenses
    latcpGatewayARN,

    -- * Destructuring the response
    ListAutomaticTapeCreationPoliciesResponse (..),
    mkListAutomaticTapeCreationPoliciesResponse,

    -- ** Response lenses
    latcprrsAutomaticTapeCreationPolicyInfos,
    latcprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkListAutomaticTapeCreationPolicies' smart constructor.
newtype ListAutomaticTapeCreationPolicies = ListAutomaticTapeCreationPolicies'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListAutomaticTapeCreationPolicies' value with any optional fields omitted.
mkListAutomaticTapeCreationPolicies ::
  ListAutomaticTapeCreationPolicies
mkListAutomaticTapeCreationPolicies =
  ListAutomaticTapeCreationPolicies' {gatewayARN = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latcpGatewayARN :: Lens.Lens' ListAutomaticTapeCreationPolicies (Core.Maybe Types.GatewayARN)
latcpGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED latcpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON ListAutomaticTapeCreationPolicies where
  toJSON ListAutomaticTapeCreationPolicies {..} =
    Core.object
      (Core.catMaybes [("GatewayARN" Core..=) Core.<$> gatewayARN])

instance Core.AWSRequest ListAutomaticTapeCreationPolicies where
  type
    Rs ListAutomaticTapeCreationPolicies =
      ListAutomaticTapeCreationPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.ListAutomaticTapeCreationPolicies"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAutomaticTapeCreationPoliciesResponse'
            Core.<$> (x Core..:? "AutomaticTapeCreationPolicyInfos")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListAutomaticTapeCreationPoliciesResponse' smart constructor.
data ListAutomaticTapeCreationPoliciesResponse = ListAutomaticTapeCreationPoliciesResponse'
  { -- | Gets a listing of information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
    automaticTapeCreationPolicyInfos :: Core.Maybe [Types.AutomaticTapeCreationPolicyInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAutomaticTapeCreationPoliciesResponse' value with any optional fields omitted.
mkListAutomaticTapeCreationPoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAutomaticTapeCreationPoliciesResponse
mkListAutomaticTapeCreationPoliciesResponse responseStatus =
  ListAutomaticTapeCreationPoliciesResponse'
    { automaticTapeCreationPolicyInfos =
        Core.Nothing,
      responseStatus
    }

-- | Gets a listing of information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
--
-- /Note:/ Consider using 'automaticTapeCreationPolicyInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latcprrsAutomaticTapeCreationPolicyInfos :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse (Core.Maybe [Types.AutomaticTapeCreationPolicyInfo])
latcprrsAutomaticTapeCreationPolicyInfos = Lens.field @"automaticTapeCreationPolicyInfos"
{-# DEPRECATED latcprrsAutomaticTapeCreationPolicyInfos "Use generic-lens or generic-optics with 'automaticTapeCreationPolicyInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latcprrsResponseStatus :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse Core.Int
latcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED latcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
