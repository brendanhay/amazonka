{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified interconnect.
module Network.AWS.DirectConnect.DeleteInterconnect
  ( -- * Creating a request
    DeleteInterconnect (..),
    mkDeleteInterconnect,

    -- ** Request lenses
    dInterconnectId,

    -- * Destructuring the response
    DeleteInterconnectResponse (..),
    mkDeleteInterconnectResponse,

    -- ** Response lenses
    drsInterconnectState,
    drsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInterconnect' smart constructor.
newtype DeleteInterconnect = DeleteInterconnect'
  { interconnectId ::
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

-- | Creates a value of 'DeleteInterconnect' with the minimum fields required to make a request.
--
-- * 'interconnectId' - The ID of the interconnect.
mkDeleteInterconnect ::
  -- | 'interconnectId'
  Lude.Text ->
  DeleteInterconnect
mkDeleteInterconnect pInterconnectId_ =
  DeleteInterconnect' {interconnectId = pInterconnectId_}

-- | The ID of the interconnect.
--
-- /Note:/ Consider using 'interconnectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInterconnectId :: Lens.Lens' DeleteInterconnect Lude.Text
dInterconnectId = Lens.lens (interconnectId :: DeleteInterconnect -> Lude.Text) (\s a -> s {interconnectId = a} :: DeleteInterconnect)
{-# DEPRECATED dInterconnectId "Use generic-lens or generic-optics with 'interconnectId' instead." #-}

instance Lude.AWSRequest DeleteInterconnect where
  type Rs DeleteInterconnect = DeleteInterconnectResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteInterconnectResponse'
            Lude.<$> (x Lude..?> "interconnectState")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInterconnect where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DeleteInterconnect" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteInterconnect where
  toJSON DeleteInterconnect' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("interconnectId" Lude..= interconnectId)]
      )

instance Lude.ToPath DeleteInterconnect where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInterconnect where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteInterconnectResponse' smart constructor.
data DeleteInterconnectResponse = DeleteInterconnectResponse'
  { interconnectState ::
      Lude.Maybe InterconnectState,
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

-- | Creates a value of 'DeleteInterconnectResponse' with the minimum fields required to make a request.
--
-- * 'interconnectState' - The state of the interconnect. The following are the possible values:
--
--
--     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The interconnect is approved, and is being initialized.
--
--
--     * @available@ : The network link is up, and the interconnect is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The interconnect is being deleted.
--
--
--     * @deleted@ : The interconnect is deleted.
--
--
--     * @unknown@ : The state of the interconnect is not available.
--
--
-- * 'responseStatus' - The response status code.
mkDeleteInterconnectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInterconnectResponse
mkDeleteInterconnectResponse pResponseStatus_ =
  DeleteInterconnectResponse'
    { interconnectState = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the interconnect. The following are the possible values:
--
--
--     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The interconnect is approved, and is being initialized.
--
--
--     * @available@ : The network link is up, and the interconnect is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The interconnect is being deleted.
--
--
--     * @deleted@ : The interconnect is deleted.
--
--
--     * @unknown@ : The state of the interconnect is not available.
--
--
--
-- /Note:/ Consider using 'interconnectState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInterconnectState :: Lens.Lens' DeleteInterconnectResponse (Lude.Maybe InterconnectState)
drsInterconnectState = Lens.lens (interconnectState :: DeleteInterconnectResponse -> Lude.Maybe InterconnectState) (\s a -> s {interconnectState = a} :: DeleteInterconnectResponse)
{-# DEPRECATED drsInterconnectState "Use generic-lens or generic-optics with 'interconnectState' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteInterconnectResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteInterconnectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInterconnectResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
