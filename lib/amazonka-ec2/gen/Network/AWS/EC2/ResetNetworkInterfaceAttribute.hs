{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a network interface attribute. You can specify only one attribute at a time.
module Network.AWS.EC2.ResetNetworkInterfaceAttribute
  ( -- * Creating a request
    ResetNetworkInterfaceAttribute (..),
    mkResetNetworkInterfaceAttribute,

    -- ** Request lenses
    rniaNetworkInterfaceId,
    rniaDryRun,
    rniaSourceDestCheck,

    -- * Destructuring the response
    ResetNetworkInterfaceAttributeResponse (..),
    mkResetNetworkInterfaceAttributeResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ResetNetworkInterfaceAttribute.
--
-- /See:/ 'mkResetNetworkInterfaceAttribute' smart constructor.
data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute'
  { -- | The ID of the network interface.
    networkInterfaceId :: Types.NetworkInterfaceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The source/destination checking attribute. Resets the value to @true@ .
    sourceDestCheck :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetNetworkInterfaceAttribute' value with any optional fields omitted.
mkResetNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Types.NetworkInterfaceId ->
  ResetNetworkInterfaceAttribute
mkResetNetworkInterfaceAttribute networkInterfaceId =
  ResetNetworkInterfaceAttribute'
    { networkInterfaceId,
      dryRun = Core.Nothing,
      sourceDestCheck = Core.Nothing
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rniaNetworkInterfaceId :: Lens.Lens' ResetNetworkInterfaceAttribute Types.NetworkInterfaceId
rniaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED rniaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rniaDryRun :: Lens.Lens' ResetNetworkInterfaceAttribute (Core.Maybe Core.Bool)
rniaDryRun = Lens.field @"dryRun"
{-# DEPRECATED rniaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The source/destination checking attribute. Resets the value to @true@ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rniaSourceDestCheck :: Lens.Lens' ResetNetworkInterfaceAttribute (Core.Maybe Types.String)
rniaSourceDestCheck = Lens.field @"sourceDestCheck"
{-# DEPRECATED rniaSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

instance Core.AWSRequest ResetNetworkInterfaceAttribute where
  type
    Rs ResetNetworkInterfaceAttribute =
      ResetNetworkInterfaceAttributeResponse
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
            ( Core.pure ("Action", "ResetNetworkInterfaceAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "NetworkInterfaceId" networkInterfaceId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "SourceDestCheck" Core.<$> sourceDestCheck)
            )
      }
  response =
    Response.receiveNull ResetNetworkInterfaceAttributeResponse'

-- | /See:/ 'mkResetNetworkInterfaceAttributeResponse' smart constructor.
data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetNetworkInterfaceAttributeResponse' value with any optional fields omitted.
mkResetNetworkInterfaceAttributeResponse ::
  ResetNetworkInterfaceAttributeResponse
mkResetNetworkInterfaceAttributeResponse =
  ResetNetworkInterfaceAttributeResponse'
