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
    dbpCustomerAddress,
    dbpAsn,
    dbpBgpPeerId,
    dbpVirtualInterfaceId,

    -- * Destructuring the response
    DeleteBGPPeerResponse (..),
    mkDeleteBGPPeerResponse,

    -- ** Response lenses
    dbprsVirtualInterface,
    dbprsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBGPPeer' smart constructor.
data DeleteBGPPeer = DeleteBGPPeer'
  { -- | The IP address assigned to the customer interface.
    customerAddress :: Lude.Maybe Lude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    asn :: Lude.Maybe Lude.Int,
    -- | The ID of the BGP peer.
    bgpPeerId :: Lude.Maybe Lude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBGPPeer' with the minimum fields required to make a request.
--
-- * 'customerAddress' - The IP address assigned to the customer interface.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
-- * 'bgpPeerId' - The ID of the BGP peer.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkDeleteBGPPeer ::
  DeleteBGPPeer
mkDeleteBGPPeer =
  DeleteBGPPeer'
    { customerAddress = Lude.Nothing,
      asn = Lude.Nothing,
      bgpPeerId = Lude.Nothing,
      virtualInterfaceId = Lude.Nothing
    }

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpCustomerAddress :: Lens.Lens' DeleteBGPPeer (Lude.Maybe Lude.Text)
dbpCustomerAddress = Lens.lens (customerAddress :: DeleteBGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: DeleteBGPPeer)
{-# DEPRECATED dbpCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpAsn :: Lens.Lens' DeleteBGPPeer (Lude.Maybe Lude.Int)
dbpAsn = Lens.lens (asn :: DeleteBGPPeer -> Lude.Maybe Lude.Int) (\s a -> s {asn = a} :: DeleteBGPPeer)
{-# DEPRECATED dbpAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The ID of the BGP peer.
--
-- /Note:/ Consider using 'bgpPeerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpBgpPeerId :: Lens.Lens' DeleteBGPPeer (Lude.Maybe Lude.Text)
dbpBgpPeerId = Lens.lens (bgpPeerId :: DeleteBGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {bgpPeerId = a} :: DeleteBGPPeer)
{-# DEPRECATED dbpBgpPeerId "Use generic-lens or generic-optics with 'bgpPeerId' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpVirtualInterfaceId :: Lens.Lens' DeleteBGPPeer (Lude.Maybe Lude.Text)
dbpVirtualInterfaceId = Lens.lens (virtualInterfaceId :: DeleteBGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceId = a} :: DeleteBGPPeer)
{-# DEPRECATED dbpVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest DeleteBGPPeer where
  type Rs DeleteBGPPeer = DeleteBGPPeerResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBGPPeerResponse'
            Lude.<$> (x Lude..?> "virtualInterface")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBGPPeer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DeleteBGPPeer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBGPPeer where
  toJSON DeleteBGPPeer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customerAddress" Lude..=) Lude.<$> customerAddress,
            ("asn" Lude..=) Lude.<$> asn,
            ("bgpPeerId" Lude..=) Lude.<$> bgpPeerId,
            ("virtualInterfaceId" Lude..=) Lude.<$> virtualInterfaceId
          ]
      )

instance Lude.ToPath DeleteBGPPeer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBGPPeer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBGPPeerResponse' smart constructor.
data DeleteBGPPeerResponse = DeleteBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Lude.Maybe VirtualInterface,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBGPPeerResponse' with the minimum fields required to make a request.
--
-- * 'virtualInterface' - The virtual interface.
-- * 'responseStatus' - The response status code.
mkDeleteBGPPeerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBGPPeerResponse
mkDeleteBGPPeerResponse pResponseStatus_ =
  DeleteBGPPeerResponse'
    { virtualInterface = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The virtual interface.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbprsVirtualInterface :: Lens.Lens' DeleteBGPPeerResponse (Lude.Maybe VirtualInterface)
dbprsVirtualInterface = Lens.lens (virtualInterface :: DeleteBGPPeerResponse -> Lude.Maybe VirtualInterface) (\s a -> s {virtualInterface = a} :: DeleteBGPPeerResponse)
{-# DEPRECATED dbprsVirtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbprsResponseStatus :: Lens.Lens' DeleteBGPPeerResponse Lude.Int
dbprsResponseStatus = Lens.lens (responseStatus :: DeleteBGPPeerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBGPPeerResponse)
{-# DEPRECATED dbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
