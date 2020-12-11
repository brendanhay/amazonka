{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a private virtual interface created by another AWS account.
--
-- After the virtual interface owner makes this call, the virtual interface is created and attached to the specified virtual private gateway or Direct Connect gateway, and is made available to handle traffic.
module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
  ( -- * Creating a request
    ConfirmPrivateVirtualInterface (..),
    mkConfirmPrivateVirtualInterface,

    -- ** Request lenses
    cpviVirtualGatewayId,
    cpviDirectConnectGatewayId,
    cpviVirtualInterfaceId,

    -- * Destructuring the response
    ConfirmPrivateVirtualInterfaceResponse (..),
    mkConfirmPrivateVirtualInterfaceResponse,

    -- ** Response lenses
    cpvirsVirtualInterfaceState,
    cpvirsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkConfirmPrivateVirtualInterface' smart constructor.
data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface'
  { virtualGatewayId ::
      Lude.Maybe Lude.Text,
    directConnectGatewayId ::
      Lude.Maybe Lude.Text,
    virtualInterfaceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkConfirmPrivateVirtualInterface ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  ConfirmPrivateVirtualInterface
mkConfirmPrivateVirtualInterface pVirtualInterfaceId_ =
  ConfirmPrivateVirtualInterface'
    { virtualGatewayId = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      virtualInterfaceId = pVirtualInterfaceId_
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpviVirtualGatewayId :: Lens.Lens' ConfirmPrivateVirtualInterface (Lude.Maybe Lude.Text)
cpviVirtualGatewayId = Lens.lens (virtualGatewayId :: ConfirmPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: ConfirmPrivateVirtualInterface)
{-# DEPRECATED cpviVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpviDirectConnectGatewayId :: Lens.Lens' ConfirmPrivateVirtualInterface (Lude.Maybe Lude.Text)
cpviDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: ConfirmPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: ConfirmPrivateVirtualInterface)
{-# DEPRECATED cpviDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpviVirtualInterfaceId :: Lens.Lens' ConfirmPrivateVirtualInterface Lude.Text
cpviVirtualInterfaceId = Lens.lens (virtualInterfaceId :: ConfirmPrivateVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: ConfirmPrivateVirtualInterface)
{-# DEPRECATED cpviVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest ConfirmPrivateVirtualInterface where
  type
    Rs ConfirmPrivateVirtualInterface =
      ConfirmPrivateVirtualInterfaceResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ConfirmPrivateVirtualInterfaceResponse'
            Lude.<$> (x Lude..?> "virtualInterfaceState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmPrivateVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.ConfirmPrivateVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfirmPrivateVirtualInterface where
  toJSON ConfirmPrivateVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("virtualGatewayId" Lude..=) Lude.<$> virtualGatewayId,
            ("directConnectGatewayId" Lude..=) Lude.<$> directConnectGatewayId,
            Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId)
          ]
      )

instance Lude.ToPath ConfirmPrivateVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmPrivateVirtualInterface where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkConfirmPrivateVirtualInterfaceResponse' smart constructor.
data ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse'
  { virtualInterfaceState ::
      Lude.Maybe
        VirtualInterfaceState,
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

-- | Creates a value of 'ConfirmPrivateVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'virtualInterfaceState' - The state of the virtual interface. The following are the possible values:
--
--
--     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
--
--
--     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
--
--
--     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
--
--
--     * @available@ : A virtual interface that is able to forward traffic.
--
--
--     * @down@ : A virtual interface that is BGP down.
--
--
--     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.
--
--
--     * @deleted@ : A virtual interface that cannot forward traffic.
--
--
--     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.
--
--
--     * @unknown@ : The state of the virtual interface is not available.
mkConfirmPrivateVirtualInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmPrivateVirtualInterfaceResponse
mkConfirmPrivateVirtualInterfaceResponse pResponseStatus_ =
  ConfirmPrivateVirtualInterfaceResponse'
    { virtualInterfaceState =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the virtual interface. The following are the possible values:
--
--
--     * @confirming@ : The creation of the virtual interface is pending confirmation from the virtual interface owner. If the owner of the virtual interface is different from the owner of the connection on which it is provisioned, then the virtual interface will remain in this state until it is confirmed by the virtual interface owner.
--
--
--     * @verifying@ : This state only applies to public virtual interfaces. Each public virtual interface needs validation before the virtual interface can be created.
--
--
--     * @pending@ : A virtual interface is in this state from the time that it is created until the virtual interface is ready to forward traffic.
--
--
--     * @available@ : A virtual interface that is able to forward traffic.
--
--
--     * @down@ : A virtual interface that is BGP down.
--
--
--     * @deleting@ : A virtual interface is in this state immediately after calling 'DeleteVirtualInterface' until it can no longer forward traffic.
--
--
--     * @deleted@ : A virtual interface that cannot forward traffic.
--
--
--     * @rejected@ : The virtual interface owner has declined creation of the virtual interface. If a virtual interface in the @Confirming@ state is deleted by the virtual interface owner, the virtual interface enters the @Rejected@ state.
--
--
--     * @unknown@ : The state of the virtual interface is not available.
--
--
--
-- /Note:/ Consider using 'virtualInterfaceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvirsVirtualInterfaceState :: Lens.Lens' ConfirmPrivateVirtualInterfaceResponse (Lude.Maybe VirtualInterfaceState)
cpvirsVirtualInterfaceState = Lens.lens (virtualInterfaceState :: ConfirmPrivateVirtualInterfaceResponse -> Lude.Maybe VirtualInterfaceState) (\s a -> s {virtualInterfaceState = a} :: ConfirmPrivateVirtualInterfaceResponse)
{-# DEPRECATED cpvirsVirtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvirsResponseStatus :: Lens.Lens' ConfirmPrivateVirtualInterfaceResponse Lude.Int
cpvirsResponseStatus = Lens.lens (responseStatus :: ConfirmPrivateVirtualInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmPrivateVirtualInterfaceResponse)
{-# DEPRECATED cpvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
