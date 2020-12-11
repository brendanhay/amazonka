{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a public virtual interface created by another AWS account.
--
-- After the virtual interface owner makes this call, the specified virtual interface is created and made available to handle traffic.
module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
  ( -- * Creating a request
    ConfirmPublicVirtualInterface (..),
    mkConfirmPublicVirtualInterface,

    -- ** Request lenses
    cVirtualInterfaceId,

    -- * Destructuring the response
    ConfirmPublicVirtualInterfaceResponse (..),
    mkConfirmPublicVirtualInterfaceResponse,

    -- ** Response lenses
    crsVirtualInterfaceState,
    crsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkConfirmPublicVirtualInterface' smart constructor.
newtype ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface'
  { virtualInterfaceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmPublicVirtualInterface' with the minimum fields required to make a request.
--
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkConfirmPublicVirtualInterface ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  ConfirmPublicVirtualInterface
mkConfirmPublicVirtualInterface pVirtualInterfaceId_ =
  ConfirmPublicVirtualInterface'
    { virtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVirtualInterfaceId :: Lens.Lens' ConfirmPublicVirtualInterface Lude.Text
cVirtualInterfaceId = Lens.lens (virtualInterfaceId :: ConfirmPublicVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: ConfirmPublicVirtualInterface)
{-# DEPRECATED cVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest ConfirmPublicVirtualInterface where
  type
    Rs ConfirmPublicVirtualInterface =
      ConfirmPublicVirtualInterfaceResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ConfirmPublicVirtualInterfaceResponse'
            Lude.<$> (x Lude..?> "virtualInterfaceState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmPublicVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.ConfirmPublicVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfirmPublicVirtualInterface where
  toJSON ConfirmPublicVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId)]
      )

instance Lude.ToPath ConfirmPublicVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmPublicVirtualInterface where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkConfirmPublicVirtualInterfaceResponse' smart constructor.
data ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse'
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

-- | Creates a value of 'ConfirmPublicVirtualInterfaceResponse' with the minimum fields required to make a request.
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
mkConfirmPublicVirtualInterfaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmPublicVirtualInterfaceResponse
mkConfirmPublicVirtualInterfaceResponse pResponseStatus_ =
  ConfirmPublicVirtualInterfaceResponse'
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
crsVirtualInterfaceState :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse (Lude.Maybe VirtualInterfaceState)
crsVirtualInterfaceState = Lens.lens (virtualInterfaceState :: ConfirmPublicVirtualInterfaceResponse -> Lude.Maybe VirtualInterfaceState) (\s a -> s {virtualInterfaceState = a} :: ConfirmPublicVirtualInterfaceResponse)
{-# DEPRECATED crsVirtualInterfaceState "Use generic-lens or generic-optics with 'virtualInterfaceState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: ConfirmPublicVirtualInterfaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmPublicVirtualInterfaceResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
