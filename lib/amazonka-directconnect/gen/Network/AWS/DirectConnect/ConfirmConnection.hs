{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms the creation of the specified hosted connection on an interconnect.
--
-- Upon creation, the hosted connection is initially in the @Ordering@ state, and remains in this state until the owner confirms creation of the hosted connection.
module Network.AWS.DirectConnect.ConfirmConnection
  ( -- * Creating a request
    ConfirmConnection (..),
    mkConfirmConnection,

    -- ** Request lenses
    ccConnectionId,

    -- * Destructuring the response
    ConfirmConnectionResponse (..),
    mkConfirmConnectionResponse,

    -- ** Response lenses
    ccrsConnectionState,
    ccrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkConfirmConnection' smart constructor.
newtype ConfirmConnection = ConfirmConnection'
  { connectionId ::
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

-- | Creates a value of 'ConfirmConnection' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the hosted connection.
mkConfirmConnection ::
  -- | 'connectionId'
  Lude.Text ->
  ConfirmConnection
mkConfirmConnection pConnectionId_ =
  ConfirmConnection' {connectionId = pConnectionId_}

-- | The ID of the hosted connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConnectionId :: Lens.Lens' ConfirmConnection Lude.Text
ccConnectionId = Lens.lens (connectionId :: ConfirmConnection -> Lude.Text) (\s a -> s {connectionId = a} :: ConfirmConnection)
{-# DEPRECATED ccConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest ConfirmConnection where
  type Rs ConfirmConnection = ConfirmConnectionResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ConfirmConnectionResponse'
            Lude.<$> (x Lude..?> "connectionState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.ConfirmConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfirmConnection where
  toJSON ConfirmConnection' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("connectionId" Lude..= connectionId)])

instance Lude.ToPath ConfirmConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery ConfirmConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkConfirmConnectionResponse' smart constructor.
data ConfirmConnectionResponse = ConfirmConnectionResponse'
  { connectionState ::
      Lude.Maybe ConnectionState,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmConnectionResponse' with the minimum fields required to make a request.
--
-- * 'connectionState' - The state of the connection. The following are the possible values:
--
--
--     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
--
--
--     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The connection has been approved and is being initialized.
--
--
--     * @available@ : The network link is up and the connection is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The connection is being deleted.
--
--
--     * @deleted@ : The connection has been deleted.
--
--
--     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.
--
--
--     * @unknown@ : The state of the connection is not available.
--
--
-- * 'responseStatus' - The response status code.
mkConfirmConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmConnectionResponse
mkConfirmConnectionResponse pResponseStatus_ =
  ConfirmConnectionResponse'
    { connectionState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the connection. The following are the possible values:
--
--
--     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
--
--
--     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The connection has been approved and is being initialized.
--
--
--     * @available@ : The network link is up and the connection is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The connection is being deleted.
--
--
--     * @deleted@ : The connection has been deleted.
--
--
--     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.
--
--
--     * @unknown@ : The state of the connection is not available.
--
--
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsConnectionState :: Lens.Lens' ConfirmConnectionResponse (Lude.Maybe ConnectionState)
ccrsConnectionState = Lens.lens (connectionState :: ConfirmConnectionResponse -> Lude.Maybe ConnectionState) (\s a -> s {connectionState = a} :: ConfirmConnectionResponse)
{-# DEPRECATED ccrsConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' ConfirmConnectionResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: ConfirmConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmConnectionResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
