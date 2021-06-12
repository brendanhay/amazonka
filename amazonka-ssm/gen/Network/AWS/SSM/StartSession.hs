{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a connection to a target (for example, an instance) for a
-- Session Manager session. Returns a URL and token that can be used to
-- open a WebSocket connection for sending input and receiving outputs.
--
-- AWS CLI usage: @start-session@ is an interactive command that requires
-- the Session Manager plugin to be installed on the client machine making
-- the call. For information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-install-plugin.html Install the Session Manager plugin for the AWS CLI>
-- in the /AWS Systems Manager User Guide/.
--
-- AWS Tools for PowerShell usage: Start-SSMSession is not currently
-- supported by AWS Tools for PowerShell on Windows local machines.
module Network.AWS.SSM.StartSession
  ( -- * Creating a Request
    StartSession (..),
    newStartSession,

    -- * Request Lenses
    startSession_documentName,
    startSession_parameters,
    startSession_target,

    -- * Destructuring the Response
    StartSessionResponse (..),
    newStartSessionResponse,

    -- * Response Lenses
    startSessionResponse_sessionId,
    startSessionResponse_streamUrl,
    startSessionResponse_tokenValue,
    startSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newStartSession' smart constructor.
data StartSession = StartSession'
  { -- | The name of the SSM document to define the parameters and plugin
    -- settings for the session. For example, @SSM-SessionManagerRunShell@. You
    -- can call the GetDocument API to verify the document exists before
    -- attempting to start a session. If no document name is provided, a shell
    -- to the instance is launched by default.
    documentName :: Core.Maybe Core.Text,
    -- | Reserved for future use.
    parameters :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The instance to connect to for the session.
    target :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentName', 'startSession_documentName' - The name of the SSM document to define the parameters and plugin
-- settings for the session. For example, @SSM-SessionManagerRunShell@. You
-- can call the GetDocument API to verify the document exists before
-- attempting to start a session. If no document name is provided, a shell
-- to the instance is launched by default.
--
-- 'parameters', 'startSession_parameters' - Reserved for future use.
--
-- 'target', 'startSession_target' - The instance to connect to for the session.
newStartSession ::
  -- | 'target'
  Core.Text ->
  StartSession
newStartSession pTarget_ =
  StartSession'
    { documentName = Core.Nothing,
      parameters = Core.Nothing,
      target = pTarget_
    }

-- | The name of the SSM document to define the parameters and plugin
-- settings for the session. For example, @SSM-SessionManagerRunShell@. You
-- can call the GetDocument API to verify the document exists before
-- attempting to start a session. If no document name is provided, a shell
-- to the instance is launched by default.
startSession_documentName :: Lens.Lens' StartSession (Core.Maybe Core.Text)
startSession_documentName = Lens.lens (\StartSession' {documentName} -> documentName) (\s@StartSession' {} a -> s {documentName = a} :: StartSession)

-- | Reserved for future use.
startSession_parameters :: Lens.Lens' StartSession (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
startSession_parameters = Lens.lens (\StartSession' {parameters} -> parameters) (\s@StartSession' {} a -> s {parameters = a} :: StartSession) Core.. Lens.mapping Lens._Coerce

-- | The instance to connect to for the session.
startSession_target :: Lens.Lens' StartSession Core.Text
startSession_target = Lens.lens (\StartSession' {target} -> target) (\s@StartSession' {} a -> s {target = a} :: StartSession)

instance Core.AWSRequest StartSession where
  type AWSResponse StartSession = StartSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSessionResponse'
            Core.<$> (x Core..?> "SessionId")
            Core.<*> (x Core..?> "StreamUrl")
            Core.<*> (x Core..?> "TokenValue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartSession

instance Core.NFData StartSession

instance Core.ToHeaders StartSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.StartSession" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartSession where
  toJSON StartSession' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DocumentName" Core..=) Core.<$> documentName,
            ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("Target" Core..= target)
          ]
      )

instance Core.ToPath StartSession where
  toPath = Core.const "/"

instance Core.ToQuery StartSession where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartSessionResponse' smart constructor.
data StartSessionResponse = StartSessionResponse'
  { -- | The ID of the session.
    sessionId :: Core.Maybe Core.Text,
    -- | A URL back to SSM Agent on the instance that the Session Manager client
    -- uses to send commands and receive output from the instance. Format:
    -- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@
    --
    -- __region__ represents the Region identifier for an AWS Region supported
    -- by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio)
    -- Region. For a list of supported __region__ values, see the __Region__
    -- column in
    -- <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints>
    -- in the /AWS General Reference/.
    --
    -- __session-id__ represents the ID of a Session Manager session, such as
    -- @1a2b3c4dEXAMPLE@.
    streamUrl :: Core.Maybe Core.Text,
    -- | An encrypted token value containing session and caller information. Used
    -- to authenticate the connection to the instance.
    tokenValue :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'startSessionResponse_sessionId' - The ID of the session.
--
-- 'streamUrl', 'startSessionResponse_streamUrl' - A URL back to SSM Agent on the instance that the Session Manager client
-- uses to send commands and receive output from the instance. Format:
-- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@
--
-- __region__ represents the Region identifier for an AWS Region supported
-- by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio)
-- Region. For a list of supported __region__ values, see the __Region__
-- column in
-- <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints>
-- in the /AWS General Reference/.
--
-- __session-id__ represents the ID of a Session Manager session, such as
-- @1a2b3c4dEXAMPLE@.
--
-- 'tokenValue', 'startSessionResponse_tokenValue' - An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the instance.
--
-- 'httpStatus', 'startSessionResponse_httpStatus' - The response's http status code.
newStartSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartSessionResponse
newStartSessionResponse pHttpStatus_ =
  StartSessionResponse'
    { sessionId = Core.Nothing,
      streamUrl = Core.Nothing,
      tokenValue = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the session.
startSessionResponse_sessionId :: Lens.Lens' StartSessionResponse (Core.Maybe Core.Text)
startSessionResponse_sessionId = Lens.lens (\StartSessionResponse' {sessionId} -> sessionId) (\s@StartSessionResponse' {} a -> s {sessionId = a} :: StartSessionResponse)

-- | A URL back to SSM Agent on the instance that the Session Manager client
-- uses to send commands and receive output from the instance. Format:
-- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@
--
-- __region__ represents the Region identifier for an AWS Region supported
-- by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio)
-- Region. For a list of supported __region__ values, see the __Region__
-- column in
-- <http://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints>
-- in the /AWS General Reference/.
--
-- __session-id__ represents the ID of a Session Manager session, such as
-- @1a2b3c4dEXAMPLE@.
startSessionResponse_streamUrl :: Lens.Lens' StartSessionResponse (Core.Maybe Core.Text)
startSessionResponse_streamUrl = Lens.lens (\StartSessionResponse' {streamUrl} -> streamUrl) (\s@StartSessionResponse' {} a -> s {streamUrl = a} :: StartSessionResponse)

-- | An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the instance.
startSessionResponse_tokenValue :: Lens.Lens' StartSessionResponse (Core.Maybe Core.Text)
startSessionResponse_tokenValue = Lens.lens (\StartSessionResponse' {tokenValue} -> tokenValue) (\s@StartSessionResponse' {} a -> s {tokenValue = a} :: StartSessionResponse)

-- | The response's http status code.
startSessionResponse_httpStatus :: Lens.Lens' StartSessionResponse Core.Int
startSessionResponse_httpStatus = Lens.lens (\StartSessionResponse' {httpStatus} -> httpStatus) (\s@StartSessionResponse' {} a -> s {httpStatus = a} :: StartSessionResponse)

instance Core.NFData StartSessionResponse
