{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual interface.
module Network.AWS.DirectConnect.DeleteVirtualInterface
  ( -- * Creating a request
    DeleteVirtualInterface (..),
    mkDeleteVirtualInterface,

    -- ** Request lenses
    dvifVirtualInterfaceId,

    -- * Destructuring the response
    DeleteVirtualInterfaceResponse (..),
    mkDeleteVirtualInterfaceResponse,

    -- ** Response lenses
    dvirsVirtualInterfaceState,
    dvirsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVirtualInterface' smart constructor.
newtype DeleteVirtualInterface = DeleteVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVirtualInterface' with the minimum fields required to make a request.
--
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkDeleteVirtualInterface ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  DeleteVirtualInterface
mkDeleteVirtualInterface pVirtualInterfaceId_ =
  DeleteVirtualInterface'
    { virtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvifVirtualInterfaceId :: Lens.Lens' DeleteVirtualInterface Lude.Text
dvifVirtualInterfaceId = Lens.lens (virtualInterfaceId :: DeleteVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: DeleteVirtualInterface)
{-# DEPRECATED dvifVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest DeleteVirtualInterface where
  type Rs DeleteVirtualInterface = DeleteVirtualInterfaceResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteVirtualInterfaceResponse'
            Lude.<$> (x Lude..?> "virtualInterfaceState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DeleteVirtualInterface" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteVirtualInterface where
  toJSON DeleteVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId)]
      )

instance Lude.ToPath DeleteVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVirtualInterface where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVirtualInterfaceResponse' smart constructor.
data DeleteVirtualInterfaceResponse = DeleteVirtualInterfaceResponse'
  { -- | The state of the virtual interface. The following are the possible values:
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
    virtualInterfaceState :: Lude.Maybe VirtualInterfaceState,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVirtualInterfaceResponse' with the minimum fields required to make a request.
--
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
--
--
-- * 'responseStatus' - The response status code.
mkDeleteVirtualInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVirtualInterfaceResponse
mkDeleteVirtualInterfaceResponse pResponseStatus_ =
  DeleteVirtualInterfaceResponse'
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
dvirsVirtualInterfaceState :: Lens.Lens' DeleteVirtualInterfaceResponse (Lude.Maybe VirtualInterfaceState)
dvirsVirtualInterfaceState = Lens.lens (virtualInterfaceState :: DeleteVirtualInterfaceResponse -> Lude.Maybe VirtualInterfaceState) (\s a -> s {virtualInterfaceState = a} :: DeleteVirtualInterfaceResponse)
{-# DEPRECATED dvirsVirtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvirsResponseStatus :: Lens.Lens' DeleteVirtualInterfaceResponse Lude.Int
dvirsResponseStatus = Lens.lens (responseStatus :: DeleteVirtualInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVirtualInterfaceResponse)
{-# DEPRECATED dvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
