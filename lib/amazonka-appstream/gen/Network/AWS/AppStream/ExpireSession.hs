{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.ExpireSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately stops the specified streaming session.
module Network.AWS.AppStream.ExpireSession
  ( -- * Creating a request
    ExpireSession (..),
    mkExpireSession,

    -- ** Request lenses
    esSessionId,

    -- * Destructuring the response
    ExpireSessionResponse (..),
    mkExpireSessionResponse,

    -- ** Response lenses
    esrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkExpireSession' smart constructor.
newtype ExpireSession = ExpireSession' {sessionId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExpireSession' with the minimum fields required to make a request.
--
-- * 'sessionId' - The identifier of the streaming session.
mkExpireSession ::
  -- | 'sessionId'
  Lude.Text ->
  ExpireSession
mkExpireSession pSessionId_ =
  ExpireSession' {sessionId = pSessionId_}

-- | The identifier of the streaming session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSessionId :: Lens.Lens' ExpireSession Lude.Text
esSessionId = Lens.lens (sessionId :: ExpireSession -> Lude.Text) (\s a -> s {sessionId = a} :: ExpireSession)
{-# DEPRECATED esSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

instance Lude.AWSRequest ExpireSession where
  type Rs ExpireSession = ExpireSessionResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ExpireSessionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExpireSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.ExpireSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ExpireSession where
  toJSON ExpireSession' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SessionId" Lude..= sessionId)])

instance Lude.ToPath ExpireSession where
  toPath = Lude.const "/"

instance Lude.ToQuery ExpireSession where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkExpireSessionResponse' smart constructor.
newtype ExpireSessionResponse = ExpireSessionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExpireSessionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkExpireSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExpireSessionResponse
mkExpireSessionResponse pResponseStatus_ =
  ExpireSessionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsResponseStatus :: Lens.Lens' ExpireSessionResponse Lude.Int
esrsResponseStatus = Lens.lens (responseStatus :: ExpireSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExpireSessionResponse)
{-# DEPRECATED esrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
