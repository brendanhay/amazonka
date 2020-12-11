{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a transit virtual interface created by another AWS account.
--
-- After the owner of the transit virtual interface makes this call, the specified transit virtual interface is created and made available to handle traffic.
module Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
  ( -- * Creating a request
    ConfirmTransitVirtualInterface (..),
    mkConfirmTransitVirtualInterface,

    -- ** Request lenses
    ctviVirtualInterfaceId,
    ctviDirectConnectGatewayId,

    -- * Destructuring the response
    ConfirmTransitVirtualInterfaceResponse (..),
    mkConfirmTransitVirtualInterfaceResponse,

    -- ** Response lenses
    conrsVirtualInterfaceState,
    conrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkConfirmTransitVirtualInterface' smart constructor.
data ConfirmTransitVirtualInterface = ConfirmTransitVirtualInterface'
  { virtualInterfaceId ::
      Lude.Text,
    directConnectGatewayId ::
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

-- | Creates a value of 'ConfirmTransitVirtualInterface' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkConfirmTransitVirtualInterface ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  -- | 'directConnectGatewayId'
  Lude.Text ->
  ConfirmTransitVirtualInterface
mkConfirmTransitVirtualInterface
  pVirtualInterfaceId_
  pDirectConnectGatewayId_ =
    ConfirmTransitVirtualInterface'
      { virtualInterfaceId =
          pVirtualInterfaceId_,
        directConnectGatewayId = pDirectConnectGatewayId_
      }

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctviVirtualInterfaceId :: Lens.Lens' ConfirmTransitVirtualInterface Lude.Text
ctviVirtualInterfaceId = Lens.lens (virtualInterfaceId :: ConfirmTransitVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: ConfirmTransitVirtualInterface)
{-# DEPRECATED ctviVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctviDirectConnectGatewayId :: Lens.Lens' ConfirmTransitVirtualInterface Lude.Text
ctviDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: ConfirmTransitVirtualInterface -> Lude.Text) (\s a -> s {directConnectGatewayId = a} :: ConfirmTransitVirtualInterface)
{-# DEPRECATED ctviDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

instance Lude.AWSRequest ConfirmTransitVirtualInterface where
  type
    Rs ConfirmTransitVirtualInterface =
      ConfirmTransitVirtualInterfaceResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ConfirmTransitVirtualInterfaceResponse'
            Lude.<$> (x Lude..?> "virtualInterfaceState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmTransitVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.ConfirmTransitVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfirmTransitVirtualInterface where
  toJSON ConfirmTransitVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId),
            Lude.Just
              ("directConnectGatewayId" Lude..= directConnectGatewayId)
          ]
      )

instance Lude.ToPath ConfirmTransitVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmTransitVirtualInterface where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkConfirmTransitVirtualInterfaceResponse' smart constructor.
data ConfirmTransitVirtualInterfaceResponse = ConfirmTransitVirtualInterfaceResponse'
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

-- | Creates a value of 'ConfirmTransitVirtualInterfaceResponse' with the minimum fields required to make a request.
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
mkConfirmTransitVirtualInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmTransitVirtualInterfaceResponse
mkConfirmTransitVirtualInterfaceResponse pResponseStatus_ =
  ConfirmTransitVirtualInterfaceResponse'
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
conrsVirtualInterfaceState :: Lens.Lens' ConfirmTransitVirtualInterfaceResponse (Lude.Maybe VirtualInterfaceState)
conrsVirtualInterfaceState = Lens.lens (virtualInterfaceState :: ConfirmTransitVirtualInterfaceResponse -> Lude.Maybe VirtualInterfaceState) (\s a -> s {virtualInterfaceState = a} :: ConfirmTransitVirtualInterfaceResponse)
{-# DEPRECATED conrsVirtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
conrsResponseStatus :: Lens.Lens' ConfirmTransitVirtualInterfaceResponse Lude.Int
conrsResponseStatus = Lens.lens (responseStatus :: ConfirmTransitVirtualInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmTransitVirtualInterfaceResponse)
{-# DEPRECATED conrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
