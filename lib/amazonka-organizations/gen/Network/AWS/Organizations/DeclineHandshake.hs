{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeclineHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Declines a handshake request. This sets the handshake state to @DECLINED@ and effectively deactivates the request.
--
-- This operation can be called only from the account that received the handshake. The originator of the handshake can use 'CancelHandshake' instead. The originator can't reactivate a declined request, but can reinitiate the process with a new handshake request.
-- After you decline a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that, it's deleted.
module Network.AWS.Organizations.DeclineHandshake
  ( -- * Creating a request
    DeclineHandshake (..),
    mkDeclineHandshake,

    -- ** Request lenses
    dhHandshakeId,

    -- * Destructuring the response
    DeclineHandshakeResponse (..),
    mkDeclineHandshakeResponse,

    -- ** Response lenses
    drsHandshake,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeclineHandshake' smart constructor.
newtype DeclineHandshake = DeclineHandshake'
  { -- | The unique identifier (ID) of the handshake that you want to decline. You can get the ID from the 'ListHandshakesForAccount' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
    handshakeId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeclineHandshake' with the minimum fields required to make a request.
--
-- * 'handshakeId' - The unique identifier (ID) of the handshake that you want to decline. You can get the ID from the 'ListHandshakesForAccount' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
mkDeclineHandshake ::
  -- | 'handshakeId'
  Lude.Text ->
  DeclineHandshake
mkDeclineHandshake pHandshakeId_ =
  DeclineHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want to decline. You can get the ID from the 'ListHandshakesForAccount' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHandshakeId :: Lens.Lens' DeclineHandshake Lude.Text
dhHandshakeId = Lens.lens (handshakeId :: DeclineHandshake -> Lude.Text) (\s a -> s {handshakeId = a} :: DeclineHandshake)
{-# DEPRECATED dhHandshakeId "Use generic-lens or generic-optics with 'handshakeId' instead." #-}

instance Lude.AWSRequest DeclineHandshake where
  type Rs DeclineHandshake = DeclineHandshakeResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeclineHandshakeResponse'
            Lude.<$> (x Lude..?> "Handshake") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeclineHandshake where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.DeclineHandshake" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeclineHandshake where
  toJSON DeclineHandshake' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("HandshakeId" Lude..= handshakeId)])

instance Lude.ToPath DeclineHandshake where
  toPath = Lude.const "/"

instance Lude.ToQuery DeclineHandshake where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeclineHandshakeResponse' smart constructor.
data DeclineHandshakeResponse = DeclineHandshakeResponse'
  { -- | A structure that contains details about the declined handshake. The state is updated to show the value @DECLINED@ .
    handshake :: Lude.Maybe Handshake,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeclineHandshakeResponse' with the minimum fields required to make a request.
--
-- * 'handshake' - A structure that contains details about the declined handshake. The state is updated to show the value @DECLINED@ .
-- * 'responseStatus' - The response status code.
mkDeclineHandshakeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeclineHandshakeResponse
mkDeclineHandshakeResponse pResponseStatus_ =
  DeclineHandshakeResponse'
    { handshake = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the declined handshake. The state is updated to show the value @DECLINED@ .
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsHandshake :: Lens.Lens' DeclineHandshakeResponse (Lude.Maybe Handshake)
drsHandshake = Lens.lens (handshake :: DeclineHandshakeResponse -> Lude.Maybe Handshake) (\s a -> s {handshake = a} :: DeclineHandshakeResponse)
{-# DEPRECATED drsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeclineHandshakeResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeclineHandshakeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeclineHandshakeResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
