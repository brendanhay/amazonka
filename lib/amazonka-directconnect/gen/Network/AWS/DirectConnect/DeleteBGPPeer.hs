{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteBGPPeer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified BGP peer on the specified virtual interface with the specified customer address and ASN.
--
-- You cannot delete the last BGP peer from a virtual interface.
module Network.AWS.DirectConnect.DeleteBGPPeer
  ( -- * Creating a request
    DeleteBGPPeer (..),
    mkDeleteBGPPeer,

    -- ** Request lenses
    dbgppAsn,
    dbgppBgpPeerId,
    dbgppCustomerAddress,
    dbgppVirtualInterfaceId,

    -- * Destructuring the response
    DeleteBGPPeerResponse (..),
    mkDeleteBGPPeerResponse,

    -- ** Response lenses
    dbgpprrsVirtualInterface,
    dbgpprrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBGPPeer' smart constructor.
data DeleteBGPPeer = DeleteBGPPeer'
  { -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    asn :: Core.Maybe Core.Int,
    -- | The ID of the BGP peer.
    bgpPeerId :: Core.Maybe Types.BgpPeerId,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Types.CustomerAddress,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBGPPeer' value with any optional fields omitted.
mkDeleteBGPPeer ::
  DeleteBGPPeer
mkDeleteBGPPeer =
  DeleteBGPPeer'
    { asn = Core.Nothing,
      bgpPeerId = Core.Nothing,
      customerAddress = Core.Nothing,
      virtualInterfaceId = Core.Nothing
    }

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgppAsn :: Lens.Lens' DeleteBGPPeer (Core.Maybe Core.Int)
dbgppAsn = Lens.field @"asn"
{-# DEPRECATED dbgppAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The ID of the BGP peer.
--
-- /Note:/ Consider using 'bgpPeerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgppBgpPeerId :: Lens.Lens' DeleteBGPPeer (Core.Maybe Types.BgpPeerId)
dbgppBgpPeerId = Lens.field @"bgpPeerId"
{-# DEPRECATED dbgppBgpPeerId "Use generic-lens or generic-optics with 'bgpPeerId' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgppCustomerAddress :: Lens.Lens' DeleteBGPPeer (Core.Maybe Types.CustomerAddress)
dbgppCustomerAddress = Lens.field @"customerAddress"
{-# DEPRECATED dbgppCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgppVirtualInterfaceId :: Lens.Lens' DeleteBGPPeer (Core.Maybe Types.VirtualInterfaceId)
dbgppVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# DEPRECATED dbgppVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Core.FromJSON DeleteBGPPeer where
  toJSON DeleteBGPPeer {..} =
    Core.object
      ( Core.catMaybes
          [ ("asn" Core..=) Core.<$> asn,
            ("bgpPeerId" Core..=) Core.<$> bgpPeerId,
            ("customerAddress" Core..=) Core.<$> customerAddress,
            ("virtualInterfaceId" Core..=) Core.<$> virtualInterfaceId
          ]
      )

instance Core.AWSRequest DeleteBGPPeer where
  type Rs DeleteBGPPeer = DeleteBGPPeerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.DeleteBGPPeer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBGPPeerResponse'
            Core.<$> (x Core..:? "virtualInterface")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteBGPPeerResponse' smart constructor.
data DeleteBGPPeerResponse = DeleteBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Core.Maybe Types.VirtualInterface,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBGPPeerResponse' value with any optional fields omitted.
mkDeleteBGPPeerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteBGPPeerResponse
mkDeleteBGPPeerResponse responseStatus =
  DeleteBGPPeerResponse'
    { virtualInterface = Core.Nothing,
      responseStatus
    }

-- | The virtual interface.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgpprrsVirtualInterface :: Lens.Lens' DeleteBGPPeerResponse (Core.Maybe Types.VirtualInterface)
dbgpprrsVirtualInterface = Lens.field @"virtualInterface"
{-# DEPRECATED dbgpprrsVirtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgpprrsResponseStatus :: Lens.Lens' DeleteBGPPeerResponse Core.Int
dbgpprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbgpprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
