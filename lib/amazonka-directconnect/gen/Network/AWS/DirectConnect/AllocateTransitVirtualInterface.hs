{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocateTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a transit virtual interface to be owned by the specified AWS account. Use this type of interface to connect a transit gateway to your Direct Connect gateway.
--
-- The owner of a connection provisions a transit virtual interface to be owned by the specified AWS account.
-- After you create a transit virtual interface, it must be confirmed by the owner using 'ConfirmTransitVirtualInterface' . Until this step has been completed, the transit virtual interface is in the @requested@ state and is not available to handle traffic.
module Network.AWS.DirectConnect.AllocateTransitVirtualInterface
  ( -- * Creating a request
    AllocateTransitVirtualInterface (..),
    mkAllocateTransitVirtualInterface,

    -- ** Request lenses
    atviConnectionId,
    atviOwnerAccount,
    atviNewTransitVirtualInterfaceAllocation,

    -- * Destructuring the response
    AllocateTransitVirtualInterfaceResponse (..),
    mkAllocateTransitVirtualInterfaceResponse,

    -- ** Response lenses
    atvirsVirtualInterface,
    atvirsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAllocateTransitVirtualInterface' smart constructor.
data AllocateTransitVirtualInterface = AllocateTransitVirtualInterface'
  { connectionId ::
      Lude.Text,
    ownerAccount :: Lude.Text,
    newTransitVirtualInterfaceAllocation ::
      NewTransitVirtualInterfaceAllocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateTransitVirtualInterface' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection on which the transit virtual interface is provisioned.
-- * 'newTransitVirtualInterfaceAllocation' - Information about the transit virtual interface.
-- * 'ownerAccount' - The ID of the AWS account that owns the transit virtual interface.
mkAllocateTransitVirtualInterface ::
  -- | 'connectionId'
  Lude.Text ->
  -- | 'ownerAccount'
  Lude.Text ->
  -- | 'newTransitVirtualInterfaceAllocation'
  NewTransitVirtualInterfaceAllocation ->
  AllocateTransitVirtualInterface
mkAllocateTransitVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewTransitVirtualInterfaceAllocation_ =
    AllocateTransitVirtualInterface'
      { connectionId = pConnectionId_,
        ownerAccount = pOwnerAccount_,
        newTransitVirtualInterfaceAllocation =
          pNewTransitVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the transit virtual interface is provisioned.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atviConnectionId :: Lens.Lens' AllocateTransitVirtualInterface Lude.Text
atviConnectionId = Lens.lens (connectionId :: AllocateTransitVirtualInterface -> Lude.Text) (\s a -> s {connectionId = a} :: AllocateTransitVirtualInterface)
{-# DEPRECATED atviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the AWS account that owns the transit virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atviOwnerAccount :: Lens.Lens' AllocateTransitVirtualInterface Lude.Text
atviOwnerAccount = Lens.lens (ownerAccount :: AllocateTransitVirtualInterface -> Lude.Text) (\s a -> s {ownerAccount = a} :: AllocateTransitVirtualInterface)
{-# DEPRECATED atviOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | Information about the transit virtual interface.
--
-- /Note:/ Consider using 'newTransitVirtualInterfaceAllocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atviNewTransitVirtualInterfaceAllocation :: Lens.Lens' AllocateTransitVirtualInterface NewTransitVirtualInterfaceAllocation
atviNewTransitVirtualInterfaceAllocation = Lens.lens (newTransitVirtualInterfaceAllocation :: AllocateTransitVirtualInterface -> NewTransitVirtualInterfaceAllocation) (\s a -> s {newTransitVirtualInterfaceAllocation = a} :: AllocateTransitVirtualInterface)
{-# DEPRECATED atviNewTransitVirtualInterfaceAllocation "Use generic-lens or generic-optics with 'newTransitVirtualInterfaceAllocation' instead." #-}

instance Lude.AWSRequest AllocateTransitVirtualInterface where
  type
    Rs AllocateTransitVirtualInterface =
      AllocateTransitVirtualInterfaceResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          AllocateTransitVirtualInterfaceResponse'
            Lude.<$> (x Lude..?> "virtualInterface")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AllocateTransitVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.AllocateTransitVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AllocateTransitVirtualInterface where
  toJSON AllocateTransitVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just ("ownerAccount" Lude..= ownerAccount),
            Lude.Just
              ( "newTransitVirtualInterfaceAllocation"
                  Lude..= newTransitVirtualInterfaceAllocation
              )
          ]
      )

instance Lude.ToPath AllocateTransitVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery AllocateTransitVirtualInterface where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAllocateTransitVirtualInterfaceResponse' smart constructor.
data AllocateTransitVirtualInterfaceResponse = AllocateTransitVirtualInterfaceResponse'
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

-- | Creates a value of 'AllocateTransitVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'virtualInterface' - Undocumented field.
mkAllocateTransitVirtualInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AllocateTransitVirtualInterfaceResponse
mkAllocateTransitVirtualInterfaceResponse pResponseStatus_ =
  AllocateTransitVirtualInterfaceResponse'
    { virtualInterface =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atvirsVirtualInterface :: Lens.Lens' AllocateTransitVirtualInterfaceResponse (Lude.Maybe VirtualInterface)
atvirsVirtualInterface = Lens.lens (virtualInterface :: AllocateTransitVirtualInterfaceResponse -> Lude.Maybe VirtualInterface) (\s a -> s {virtualInterface = a} :: AllocateTransitVirtualInterfaceResponse)
{-# DEPRECATED atvirsVirtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atvirsResponseStatus :: Lens.Lens' AllocateTransitVirtualInterfaceResponse Lude.Int
atvirsResponseStatus = Lens.lens (responseStatus :: AllocateTransitVirtualInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AllocateTransitVirtualInterfaceResponse)
{-# DEPRECATED atvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
