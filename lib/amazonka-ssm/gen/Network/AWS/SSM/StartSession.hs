{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a connection to a target (for example, an instance) for a Session Manager session. Returns a URL and token that can be used to open a WebSocket connection for sending input and receiving outputs.
module Network.AWS.SSM.StartSession
  ( -- * Creating a request
    StartSession (..),
    mkStartSession,

    -- ** Request lenses
    ssDocumentName,
    ssParameters,
    ssTarget,

    -- * Destructuring the response
    StartSessionResponse (..),
    mkStartSessionResponse,

    -- ** Response lenses
    ssrsStreamURL,
    ssrsTokenValue,
    ssrsSessionId,
    ssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkStartSession' smart constructor.
data StartSession = StartSession'
  { -- | The name of the SSM document to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ . You can call the 'GetDocument' API to verify the document exists before attempting to start a session. If no document name is provided, a shell to the instance is launched by default.
    documentName :: Lude.Maybe Lude.Text,
    -- | Reserved for future use.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The instance to connect to for the session.
    target :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSession' with the minimum fields required to make a request.
--
-- * 'documentName' - The name of the SSM document to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ . You can call the 'GetDocument' API to verify the document exists before attempting to start a session. If no document name is provided, a shell to the instance is launched by default.
-- * 'parameters' - Reserved for future use.
-- * 'target' - The instance to connect to for the session.
mkStartSession ::
  -- | 'target'
  Lude.Text ->
  StartSession
mkStartSession pTarget_ =
  StartSession'
    { documentName = Lude.Nothing,
      parameters = Lude.Nothing,
      target = pTarget_
    }

-- | The name of the SSM document to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ . You can call the 'GetDocument' API to verify the document exists before attempting to start a session. If no document name is provided, a shell to the instance is launched by default.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDocumentName :: Lens.Lens' StartSession (Lude.Maybe Lude.Text)
ssDocumentName = Lens.lens (documentName :: StartSession -> Lude.Maybe Lude.Text) (\s a -> s {documentName = a} :: StartSession)
{-# DEPRECATED ssDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParameters :: Lens.Lens' StartSession (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
ssParameters = Lens.lens (parameters :: StartSession -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: StartSession)
{-# DEPRECATED ssParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The instance to connect to for the session.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTarget :: Lens.Lens' StartSession Lude.Text
ssTarget = Lens.lens (target :: StartSession -> Lude.Text) (\s a -> s {target = a} :: StartSession)
{-# DEPRECATED ssTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.AWSRequest StartSession where
  type Rs StartSession = StartSessionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartSessionResponse'
            Lude.<$> (x Lude..?> "StreamUrl")
            Lude.<*> (x Lude..?> "TokenValue")
            Lude.<*> (x Lude..?> "SessionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.StartSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartSession where
  toJSON StartSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DocumentName" Lude..=) Lude.<$> documentName,
            ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Target" Lude..= target)
          ]
      )

instance Lude.ToPath StartSession where
  toPath = Lude.const "/"

instance Lude.ToQuery StartSession where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartSessionResponse' smart constructor.
data StartSessionResponse = StartSessionResponse'
  { -- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@
    --
    -- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
    -- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
    streamURL :: Lude.Maybe Lude.Text,
    -- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
    tokenValue :: Lude.Maybe Lude.Text,
    -- | The ID of the session.
    sessionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSessionResponse' with the minimum fields required to make a request.
--
-- * 'streamURL' - A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@
--
-- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
-- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
-- * 'tokenValue' - An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
-- * 'sessionId' - The ID of the session.
-- * 'responseStatus' - The response status code.
mkStartSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartSessionResponse
mkStartSessionResponse pResponseStatus_ =
  StartSessionResponse'
    { streamURL = Lude.Nothing,
      tokenValue = Lude.Nothing,
      sessionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssmmessages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@
--
-- __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints> in the /AWS General Reference/ .
-- __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
--
-- /Note:/ Consider using 'streamURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsStreamURL :: Lens.Lens' StartSessionResponse (Lude.Maybe Lude.Text)
ssrsStreamURL = Lens.lens (streamURL :: StartSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamURL = a} :: StartSessionResponse)
{-# DEPRECATED ssrsStreamURL "Use generic-lens or generic-optics with 'streamURL' instead." #-}

-- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
--
-- /Note:/ Consider using 'tokenValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsTokenValue :: Lens.Lens' StartSessionResponse (Lude.Maybe Lude.Text)
ssrsTokenValue = Lens.lens (tokenValue :: StartSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {tokenValue = a} :: StartSessionResponse)
{-# DEPRECATED ssrsTokenValue "Use generic-lens or generic-optics with 'tokenValue' instead." #-}

-- | The ID of the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsSessionId :: Lens.Lens' StartSessionResponse (Lude.Maybe Lude.Text)
ssrsSessionId = Lens.lens (sessionId :: StartSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: StartSessionResponse)
{-# DEPRECATED ssrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsResponseStatus :: Lens.Lens' StartSessionResponse Lude.Int
ssrsResponseStatus = Lens.lens (responseStatus :: StartSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartSessionResponse)
{-# DEPRECATED ssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
