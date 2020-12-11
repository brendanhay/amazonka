{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.TerminateSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently ends a session and closes the data connection between the Session Manager client and SSM Agent on the instance. A terminated session cannot be resumed.
module Network.AWS.SSM.TerminateSession
  ( -- * Creating a request
    TerminateSession (..),
    mkTerminateSession,

    -- ** Request lenses
    tsSessionId,

    -- * Destructuring the response
    TerminateSessionResponse (..),
    mkTerminateSessionResponse,

    -- ** Response lenses
    tsrsSessionId,
    tsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkTerminateSession' smart constructor.
newtype TerminateSession = TerminateSession'
  { sessionId ::
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

-- | Creates a value of 'TerminateSession' with the minimum fields required to make a request.
--
-- * 'sessionId' - The ID of the session to terminate.
mkTerminateSession ::
  -- | 'sessionId'
  Lude.Text ->
  TerminateSession
mkTerminateSession pSessionId_ =
  TerminateSession' {sessionId = pSessionId_}

-- | The ID of the session to terminate.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSessionId :: Lens.Lens' TerminateSession Lude.Text
tsSessionId = Lens.lens (sessionId :: TerminateSession -> Lude.Text) (\s a -> s {sessionId = a} :: TerminateSession)
{-# DEPRECATED tsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

instance Lude.AWSRequest TerminateSession where
  type Rs TerminateSession = TerminateSessionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          TerminateSessionResponse'
            Lude.<$> (x Lude..?> "SessionId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.TerminateSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TerminateSession where
  toJSON TerminateSession' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SessionId" Lude..= sessionId)])

instance Lude.ToPath TerminateSession where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateSession where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTerminateSessionResponse' smart constructor.
data TerminateSessionResponse = TerminateSessionResponse'
  { sessionId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'TerminateSessionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sessionId' - The ID of the session that has been terminated.
mkTerminateSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateSessionResponse
mkTerminateSessionResponse pResponseStatus_ =
  TerminateSessionResponse'
    { sessionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the session that has been terminated.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsrsSessionId :: Lens.Lens' TerminateSessionResponse (Lude.Maybe Lude.Text)
tsrsSessionId = Lens.lens (sessionId :: TerminateSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: TerminateSessionResponse)
{-# DEPRECATED tsrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsrsResponseStatus :: Lens.Lens' TerminateSessionResponse Lude.Int
tsrsResponseStatus = Lens.lens (responseStatus :: TerminateSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateSessionResponse)
{-# DEPRECATED tsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
