{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ResumeSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reconnects a session to an instance after it has been disconnected. Connections can be resumed for disconnected sessions, but not terminated sessions.
module Network.AWS.SSM.ResumeSession
  ( -- * Creating a request
    ResumeSession (..),
    mkResumeSession,

    -- ** Request lenses
    rsSessionId,

    -- * Destructuring the response
    ResumeSessionResponse (..),
    mkResumeSessionResponse,

    -- ** Response lenses
    rsrsStreamURL,
    rsrsTokenValue,
    rsrsSessionId,
    rsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkResumeSession' smart constructor.
newtype ResumeSession = ResumeSession' {sessionId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResumeSession' with the minimum fields required to make a request.
--
-- * 'sessionId' - The ID of the disconnected session to resume.
mkResumeSession ::
  -- | 'sessionId'
  Lude.Text ->
  ResumeSession
mkResumeSession pSessionId_ =
  ResumeSession' {sessionId = pSessionId_}

-- | The ID of the disconnected session to resume.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSessionId :: Lens.Lens' ResumeSession Lude.Text
rsSessionId = Lens.lens (sessionId :: ResumeSession -> Lude.Text) (\s a -> s {sessionId = a} :: ResumeSession)
{-# DEPRECATED rsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

instance Lude.AWSRequest ResumeSession where
  type Rs ResumeSession = ResumeSessionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResumeSessionResponse'
            Lude.<$> (x Lude..?> "StreamUrl")
            Lude.<*> (x Lude..?> "TokenValue")
            Lude.<*> (x Lude..?> "SessionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResumeSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ResumeSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResumeSession where
  toJSON ResumeSession' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SessionId" Lude..= sessionId)])

instance Lude.ToPath ResumeSession where
  toPath = Lude.const "/"

instance Lude.ToQuery ResumeSession where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResumeSessionResponse' smart constructor.
data ResumeSessionResponse = ResumeSessionResponse'
  { streamURL ::
      Lude.Maybe Lude.Text,
    tokenValue :: Lude.Maybe Lude.Text,
    sessionId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ResumeSessionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sessionId' - The ID of the session.
-- * 'streamURL' - A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@ .
--
-- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
-- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
-- * 'tokenValue' - An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
mkResumeSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResumeSessionResponse
mkResumeSessionResponse pResponseStatus_ =
  ResumeSessionResponse'
    { streamURL = Lude.Nothing,
      tokenValue = Lude.Nothing,
      sessionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@ .
--
-- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
-- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
--
-- /Note:/ Consider using 'streamURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsStreamURL :: Lens.Lens' ResumeSessionResponse (Lude.Maybe Lude.Text)
rsrsStreamURL = Lens.lens (streamURL :: ResumeSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamURL = a} :: ResumeSessionResponse)
{-# DEPRECATED rsrsStreamURL "Use generic-lens or generic-optics with 'streamURL' instead." #-}

-- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
--
-- /Note:/ Consider using 'tokenValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsTokenValue :: Lens.Lens' ResumeSessionResponse (Lude.Maybe Lude.Text)
rsrsTokenValue = Lens.lens (tokenValue :: ResumeSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {tokenValue = a} :: ResumeSessionResponse)
{-# DEPRECATED rsrsTokenValue "Use generic-lens or generic-optics with 'tokenValue' instead." #-}

-- | The ID of the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsSessionId :: Lens.Lens' ResumeSessionResponse (Lude.Maybe Lude.Text)
rsrsSessionId = Lens.lens (sessionId :: ResumeSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: ResumeSessionResponse)
{-# DEPRECATED rsrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsResponseStatus :: Lens.Lens' ResumeSessionResponse Lude.Int
rsrsResponseStatus = Lens.lens (responseStatus :: ResumeSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResumeSessionResponse)
{-# DEPRECATED rsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
