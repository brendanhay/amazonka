{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateBGPPeer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a BGP peer on the specified virtual interface.
--
-- You must create a BGP peer for the corresponding address family (IPv4/IPv6) in order to access AWS resources that also use that address family.
-- If logical redundancy is not supported by the connection, interconnect, or LAG, the BGP peer cannot be in the same address family as an existing BGP peer on the virtual interface.
-- When creating a IPv6 BGP peer, omit the Amazon address and customer address. IPv6 addresses are automatically assigned from the Amazon pool of IPv6 addresses; you cannot specify custom IPv6 addresses.
-- For a public virtual interface, the Autonomous System Number (ASN) must be private or already whitelisted for the virtual interface.
module Network.AWS.DirectConnect.CreateBGPPeer
  ( -- * Creating a request
    CreateBGPPeer (..),
    mkCreateBGPPeer,

    -- ** Request lenses
    cbgppNewBGPPeer,
    cbgppVirtualInterfaceId,

    -- * Destructuring the response
    CreateBGPPeerResponse (..),
    mkCreateBGPPeerResponse,

    -- ** Response lenses
    cbgpprrsVirtualInterface,
    cbgpprrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateBGPPeer' smart constructor.
data CreateBGPPeer = CreateBGPPeer'
  { -- | Information about the BGP peer.
    newBGPPeer :: Core.Maybe Types.NewBGPPeer,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBGPPeer' value with any optional fields omitted.
mkCreateBGPPeer ::
  CreateBGPPeer
mkCreateBGPPeer =
  CreateBGPPeer'
    { newBGPPeer = Core.Nothing,
      virtualInterfaceId = Core.Nothing
    }

-- | Information about the BGP peer.
--
-- /Note:/ Consider using 'newBGPPeer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgppNewBGPPeer :: Lens.Lens' CreateBGPPeer (Core.Maybe Types.NewBGPPeer)
cbgppNewBGPPeer = Lens.field @"newBGPPeer"
{-# DEPRECATED cbgppNewBGPPeer "Use generic-lens or generic-optics with 'newBGPPeer' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgppVirtualInterfaceId :: Lens.Lens' CreateBGPPeer (Core.Maybe Types.VirtualInterfaceId)
cbgppVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# DEPRECATED cbgppVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Core.FromJSON CreateBGPPeer where
  toJSON CreateBGPPeer {..} =
    Core.object
      ( Core.catMaybes
          [ ("newBGPPeer" Core..=) Core.<$> newBGPPeer,
            ("virtualInterfaceId" Core..=) Core.<$> virtualInterfaceId
          ]
      )

instance Core.AWSRequest CreateBGPPeer where
  type Rs CreateBGPPeer = CreateBGPPeerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.CreateBGPPeer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBGPPeerResponse'
            Core.<$> (x Core..:? "virtualInterface")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateBGPPeerResponse' smart constructor.
data CreateBGPPeerResponse = CreateBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Core.Maybe Types.VirtualInterface,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBGPPeerResponse' value with any optional fields omitted.
mkCreateBGPPeerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateBGPPeerResponse
mkCreateBGPPeerResponse responseStatus =
  CreateBGPPeerResponse'
    { virtualInterface = Core.Nothing,
      responseStatus
    }

-- | The virtual interface.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgpprrsVirtualInterface :: Lens.Lens' CreateBGPPeerResponse (Core.Maybe Types.VirtualInterface)
cbgpprrsVirtualInterface = Lens.field @"virtualInterface"
{-# DEPRECATED cbgpprrsVirtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbgpprrsResponseStatus :: Lens.Lens' CreateBGPPeerResponse Core.Int
cbgpprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cbgpprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
