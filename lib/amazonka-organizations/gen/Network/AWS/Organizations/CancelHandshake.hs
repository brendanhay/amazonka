{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CancelHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a handshake. Canceling a handshake sets the handshake state to @CANCELED@ .
--
-- This operation can be called only from the account that originated the handshake. The recipient of the handshake can't cancel it, but can use 'DeclineHandshake' instead. After a handshake is canceled, the recipient can no longer respond to that handshake.
-- After you cancel a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that, it's deleted.
module Network.AWS.Organizations.CancelHandshake
  ( -- * Creating a request
    CancelHandshake (..),
    mkCancelHandshake,

    -- ** Request lenses
    chHandshakeId,

    -- * Destructuring the response
    CancelHandshakeResponse (..),
    mkCancelHandshakeResponse,

    -- ** Response lenses
    chrsHandshake,
    chrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelHandshake' smart constructor.
newtype CancelHandshake = CancelHandshake'
  { handshakeId ::
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

-- | Creates a value of 'CancelHandshake' with the minimum fields required to make a request.
--
-- * 'handshakeId' - The unique identifier (ID) of the handshake that you want to cancel. You can get the ID from the 'ListHandshakesForOrganization' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
mkCancelHandshake ::
  -- | 'handshakeId'
  Lude.Text ->
  CancelHandshake
mkCancelHandshake pHandshakeId_ =
  CancelHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want to cancel. You can get the ID from the 'ListHandshakesForOrganization' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chHandshakeId :: Lens.Lens' CancelHandshake Lude.Text
chHandshakeId = Lens.lens (handshakeId :: CancelHandshake -> Lude.Text) (\s a -> s {handshakeId = a} :: CancelHandshake)
{-# DEPRECATED chHandshakeId "Use generic-lens or generic-optics with 'handshakeId' instead." #-}

instance Lude.AWSRequest CancelHandshake where
  type Rs CancelHandshake = CancelHandshakeResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelHandshakeResponse'
            Lude.<$> (x Lude..?> "Handshake") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelHandshake where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.CancelHandshake" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelHandshake where
  toJSON CancelHandshake' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("HandshakeId" Lude..= handshakeId)])

instance Lude.ToPath CancelHandshake where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelHandshake where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelHandshakeResponse' smart constructor.
data CancelHandshakeResponse = CancelHandshakeResponse'
  { handshake ::
      Lude.Maybe Handshake,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelHandshakeResponse' with the minimum fields required to make a request.
--
-- * 'handshake' - A structure that contains details about the handshake that you canceled.
-- * 'responseStatus' - The response status code.
mkCancelHandshakeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelHandshakeResponse
mkCancelHandshakeResponse pResponseStatus_ =
  CancelHandshakeResponse'
    { handshake = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the handshake that you canceled.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrsHandshake :: Lens.Lens' CancelHandshakeResponse (Lude.Maybe Handshake)
chrsHandshake = Lens.lens (handshake :: CancelHandshakeResponse -> Lude.Maybe Handshake) (\s a -> s {handshake = a} :: CancelHandshakeResponse)
{-# DEPRECATED chrsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrsResponseStatus :: Lens.Lens' CancelHandshakeResponse Lude.Int
chrsResponseStatus = Lens.lens (responseStatus :: CancelHandshakeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelHandshakeResponse)
{-# DEPRECATED chrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
