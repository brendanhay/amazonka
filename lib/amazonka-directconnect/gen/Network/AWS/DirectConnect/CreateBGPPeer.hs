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
    cbpNewBGPPeer,
    cbpVirtualInterfaceId,

    -- * Destructuring the response
    CreateBGPPeerResponse (..),
    mkCreateBGPPeerResponse,

    -- ** Response lenses
    cbprsVirtualInterface,
    cbprsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBGPPeer' smart constructor.
data CreateBGPPeer = CreateBGPPeer'
  { -- | Information about the BGP peer.
    newBGPPeer :: Lude.Maybe NewBGPPeer,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBGPPeer' with the minimum fields required to make a request.
--
-- * 'newBGPPeer' - Information about the BGP peer.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkCreateBGPPeer ::
  CreateBGPPeer
mkCreateBGPPeer =
  CreateBGPPeer'
    { newBGPPeer = Lude.Nothing,
      virtualInterfaceId = Lude.Nothing
    }

-- | Information about the BGP peer.
--
-- /Note:/ Consider using 'newBGPPeer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpNewBGPPeer :: Lens.Lens' CreateBGPPeer (Lude.Maybe NewBGPPeer)
cbpNewBGPPeer = Lens.lens (newBGPPeer :: CreateBGPPeer -> Lude.Maybe NewBGPPeer) (\s a -> s {newBGPPeer = a} :: CreateBGPPeer)
{-# DEPRECATED cbpNewBGPPeer "Use generic-lens or generic-optics with 'newBGPPeer' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpVirtualInterfaceId :: Lens.Lens' CreateBGPPeer (Lude.Maybe Lude.Text)
cbpVirtualInterfaceId = Lens.lens (virtualInterfaceId :: CreateBGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceId = a} :: CreateBGPPeer)
{-# DEPRECATED cbpVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest CreateBGPPeer where
  type Rs CreateBGPPeer = CreateBGPPeerResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBGPPeerResponse'
            Lude.<$> (x Lude..?> "virtualInterface")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBGPPeer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.CreateBGPPeer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBGPPeer where
  toJSON CreateBGPPeer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("newBGPPeer" Lude..=) Lude.<$> newBGPPeer,
            ("virtualInterfaceId" Lude..=) Lude.<$> virtualInterfaceId
          ]
      )

instance Lude.ToPath CreateBGPPeer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBGPPeer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBGPPeerResponse' smart constructor.
data CreateBGPPeerResponse = CreateBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Lude.Maybe VirtualInterface,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBGPPeerResponse' with the minimum fields required to make a request.
--
-- * 'virtualInterface' - The virtual interface.
-- * 'responseStatus' - The response status code.
mkCreateBGPPeerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBGPPeerResponse
mkCreateBGPPeerResponse pResponseStatus_ =
  CreateBGPPeerResponse'
    { virtualInterface = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The virtual interface.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbprsVirtualInterface :: Lens.Lens' CreateBGPPeerResponse (Lude.Maybe VirtualInterface)
cbprsVirtualInterface = Lens.lens (virtualInterface :: CreateBGPPeerResponse -> Lude.Maybe VirtualInterface) (\s a -> s {virtualInterface = a} :: CreateBGPPeerResponse)
{-# DEPRECATED cbprsVirtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbprsResponseStatus :: Lens.Lens' CreateBGPPeerResponse Lude.Int
cbprsResponseStatus = Lens.lens (responseStatus :: CreateBGPPeerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBGPPeerResponse)
{-# DEPRECATED cbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
