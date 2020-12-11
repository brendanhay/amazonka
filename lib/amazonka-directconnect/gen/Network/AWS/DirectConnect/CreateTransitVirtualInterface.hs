{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit virtual interface. A transit virtual interface should be used to access one or more transit gateways associated with Direct Connect gateways. A transit virtual interface enables the connection of multiple VPCs attached to a transit gateway to a Direct Connect gateway.
--
-- /Important:/ If you associate your transit gateway with one or more Direct Connect gateways, the Autonomous System Number (ASN) used by the transit gateway and the Direct Connect gateway must be different. For example, if you use the default ASN 64512 for both your the transit gateway and Direct Connect gateway, the association request fails.
-- Setting the MTU of a virtual interface to 8500 (jumbo frames) can cause an update to the underlying physical connection if it wasn't updated to support jumbo frames. Updating the connection disrupts network connectivity for all virtual interfaces associated with the connection for up to 30 seconds. To check whether your connection supports jumbo frames, call 'DescribeConnections' . To check whether your virtual interface supports jumbo frames, call 'DescribeVirtualInterfaces' .
module Network.AWS.DirectConnect.CreateTransitVirtualInterface
  ( -- * Creating a request
    CreateTransitVirtualInterface (..),
    mkCreateTransitVirtualInterface,

    -- ** Request lenses
    ctviConnectionId,
    ctviNewTransitVirtualInterface,

    -- * Destructuring the response
    CreateTransitVirtualInterfaceResponse (..),
    mkCreateTransitVirtualInterfaceResponse,

    -- ** Response lenses
    ctvirsVirtualInterface,
    ctvirsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitVirtualInterface' smart constructor.
data CreateTransitVirtualInterface = CreateTransitVirtualInterface'
  { connectionId ::
      Lude.Text,
    newTransitVirtualInterface ::
      NewTransitVirtualInterface
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitVirtualInterface' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection.
-- * 'newTransitVirtualInterface' - Information about the transit virtual interface.
mkCreateTransitVirtualInterface ::
  -- | 'connectionId'
  Lude.Text ->
  -- | 'newTransitVirtualInterface'
  NewTransitVirtualInterface ->
  CreateTransitVirtualInterface
mkCreateTransitVirtualInterface
  pConnectionId_
  pNewTransitVirtualInterface_ =
    CreateTransitVirtualInterface'
      { connectionId = pConnectionId_,
        newTransitVirtualInterface = pNewTransitVirtualInterface_
      }

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctviConnectionId :: Lens.Lens' CreateTransitVirtualInterface Lude.Text
ctviConnectionId = Lens.lens (connectionId :: CreateTransitVirtualInterface -> Lude.Text) (\s a -> s {connectionId = a} :: CreateTransitVirtualInterface)
{-# DEPRECATED ctviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | Information about the transit virtual interface.
--
-- /Note:/ Consider using 'newTransitVirtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctviNewTransitVirtualInterface :: Lens.Lens' CreateTransitVirtualInterface NewTransitVirtualInterface
ctviNewTransitVirtualInterface = Lens.lens (newTransitVirtualInterface :: CreateTransitVirtualInterface -> NewTransitVirtualInterface) (\s a -> s {newTransitVirtualInterface = a} :: CreateTransitVirtualInterface)
{-# DEPRECATED ctviNewTransitVirtualInterface "Use generic-lens or generic-optics with 'newTransitVirtualInterface' instead." #-}

instance Lude.AWSRequest CreateTransitVirtualInterface where
  type
    Rs CreateTransitVirtualInterface =
      CreateTransitVirtualInterfaceResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTransitVirtualInterfaceResponse'
            Lude.<$> (x Lude..?> "virtualInterface")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.CreateTransitVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTransitVirtualInterface where
  toJSON CreateTransitVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just
              ("newTransitVirtualInterface" Lude..= newTransitVirtualInterface)
          ]
      )

instance Lude.ToPath CreateTransitVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitVirtualInterface where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTransitVirtualInterfaceResponse' smart constructor.
data CreateTransitVirtualInterfaceResponse = CreateTransitVirtualInterfaceResponse'
  { virtualInterface ::
      Lude.Maybe
        VirtualInterface,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'virtualInterface' - Undocumented field.
mkCreateTransitVirtualInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitVirtualInterfaceResponse
mkCreateTransitVirtualInterfaceResponse pResponseStatus_ =
  CreateTransitVirtualInterfaceResponse'
    { virtualInterface =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctvirsVirtualInterface :: Lens.Lens' CreateTransitVirtualInterfaceResponse (Lude.Maybe VirtualInterface)
ctvirsVirtualInterface = Lens.lens (virtualInterface :: CreateTransitVirtualInterfaceResponse -> Lude.Maybe VirtualInterface) (\s a -> s {virtualInterface = a} :: CreateTransitVirtualInterfaceResponse)
{-# DEPRECATED ctvirsVirtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctvirsResponseStatus :: Lens.Lens' CreateTransitVirtualInterfaceResponse Lude.Int
ctvirsResponseStatus = Lens.lens (responseStatus :: CreateTransitVirtualInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitVirtualInterfaceResponse)
{-# DEPRECATED ctvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
