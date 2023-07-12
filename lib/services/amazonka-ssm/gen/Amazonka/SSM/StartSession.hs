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
-- Module      : Amazonka.SSM.StartSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a connection to a target (for example, a managed node) for a
-- Session Manager session. Returns a URL and token that can be used to
-- open a WebSocket connection for sending input and receiving outputs.
--
-- Amazon Web Services CLI usage: @start-session@ is an interactive command
-- that requires the Session Manager plugin to be installed on the client
-- machine making the call. For information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-install-plugin.html Install the Session Manager plugin for the Amazon Web Services CLI>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- Amazon Web Services Tools for PowerShell usage: Start-SSMSession isn\'t
-- currently supported by Amazon Web Services Tools for PowerShell on
-- Windows local machines.
module Amazonka.SSM.StartSession
  ( -- * Creating a Request
    StartSession (..),
    newStartSession,

    -- * Request Lenses
    startSession_documentName,
    startSession_parameters,
    startSession_reason,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newStartSession' smart constructor.
data StartSession = StartSession'
  { -- | The name of the SSM document you want to use to define the type of
    -- session, input parameters, or preferences for the session. For example,
    -- @SSM-SessionManagerRunShell@. You can call the GetDocument API to verify
    -- the document exists before attempting to start a session. If no document
    -- name is provided, a shell to the managed node is launched by default.
    -- For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-sessions-start.html Start a session>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    documentName :: Prelude.Maybe Prelude.Text,
    -- | The values you want to specify for the parameters defined in the Session
    -- document.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The reason for connecting to the instance. This value is included in the
    -- details for the Amazon CloudWatch Events event created when you start
    -- the session.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The managed node to connect to for the session.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentName', 'startSession_documentName' - The name of the SSM document you want to use to define the type of
-- session, input parameters, or preferences for the session. For example,
-- @SSM-SessionManagerRunShell@. You can call the GetDocument API to verify
-- the document exists before attempting to start a session. If no document
-- name is provided, a shell to the managed node is launched by default.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-sessions-start.html Start a session>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'parameters', 'startSession_parameters' - The values you want to specify for the parameters defined in the Session
-- document.
--
-- 'reason', 'startSession_reason' - The reason for connecting to the instance. This value is included in the
-- details for the Amazon CloudWatch Events event created when you start
-- the session.
--
-- 'target', 'startSession_target' - The managed node to connect to for the session.
newStartSession ::
  -- | 'target'
  Prelude.Text ->
  StartSession
newStartSession pTarget_ =
  StartSession'
    { documentName = Prelude.Nothing,
      parameters = Prelude.Nothing,
      reason = Prelude.Nothing,
      target = pTarget_
    }

-- | The name of the SSM document you want to use to define the type of
-- session, input parameters, or preferences for the session. For example,
-- @SSM-SessionManagerRunShell@. You can call the GetDocument API to verify
-- the document exists before attempting to start a session. If no document
-- name is provided, a shell to the managed node is launched by default.
-- For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-sessions-start.html Start a session>
-- in the /Amazon Web Services Systems Manager User Guide/.
startSession_documentName :: Lens.Lens' StartSession (Prelude.Maybe Prelude.Text)
startSession_documentName = Lens.lens (\StartSession' {documentName} -> documentName) (\s@StartSession' {} a -> s {documentName = a} :: StartSession)

-- | The values you want to specify for the parameters defined in the Session
-- document.
startSession_parameters :: Lens.Lens' StartSession (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
startSession_parameters = Lens.lens (\StartSession' {parameters} -> parameters) (\s@StartSession' {} a -> s {parameters = a} :: StartSession) Prelude.. Lens.mapping Lens.coerced

-- | The reason for connecting to the instance. This value is included in the
-- details for the Amazon CloudWatch Events event created when you start
-- the session.
startSession_reason :: Lens.Lens' StartSession (Prelude.Maybe Prelude.Text)
startSession_reason = Lens.lens (\StartSession' {reason} -> reason) (\s@StartSession' {} a -> s {reason = a} :: StartSession)

-- | The managed node to connect to for the session.
startSession_target :: Lens.Lens' StartSession Prelude.Text
startSession_target = Lens.lens (\StartSession' {target} -> target) (\s@StartSession' {} a -> s {target = a} :: StartSession)

instance Core.AWSRequest StartSession where
  type AWSResponse StartSession = StartSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSessionResponse'
            Prelude.<$> (x Data..?> "SessionId")
            Prelude.<*> (x Data..?> "StreamUrl")
            Prelude.<*> (x Data..?> "TokenValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSession where
  hashWithSalt _salt StartSession' {..} =
    _salt
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` target

instance Prelude.NFData StartSession where
  rnf StartSession' {..} =
    Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf target

instance Data.ToHeaders StartSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.StartSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSession where
  toJSON StartSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentName" Data..=) Prelude.<$> documentName,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("Reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("Target" Data..= target)
          ]
      )

instance Data.ToPath StartSession where
  toPath = Prelude.const "/"

instance Data.ToQuery StartSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSessionResponse' smart constructor.
data StartSessionResponse = StartSessionResponse'
  { -- | The ID of the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | A URL back to SSM Agent on the managed node that the Session Manager
    -- client uses to send commands and receive output from the node. Format:
    -- @wss:\/\/ssmmessages.@__@region@__@.amazonaws.com\/v1\/data-channel\/@__@session-id@__@?stream=(input|output)@
    --
    -- __region__ represents the Region identifier for an Amazon Web Services
    -- Region supported by Amazon Web Services Systems Manager, such as
    -- @us-east-2@ for the US East (Ohio) Region. For a list of supported
    -- __region__ values, see the __Region__ column in
    -- <https://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints>
    -- in the /Amazon Web Services General Reference/.
    --
    -- __session-id__ represents the ID of a Session Manager session, such as
    -- @1a2b3c4dEXAMPLE@.
    streamUrl :: Prelude.Maybe Prelude.Text,
    -- | An encrypted token value containing session and caller information. This
    -- token is used to authenticate the connection to the managed node, and is
    -- valid only long enough to ensure the connection is successful. Never
    -- share your session\'s token.
    tokenValue :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'streamUrl', 'startSessionResponse_streamUrl' - A URL back to SSM Agent on the managed node that the Session Manager
-- client uses to send commands and receive output from the node. Format:
-- @wss:\/\/ssmmessages.@__@region@__@.amazonaws.com\/v1\/data-channel\/@__@session-id@__@?stream=(input|output)@
--
-- __region__ represents the Region identifier for an Amazon Web Services
-- Region supported by Amazon Web Services Systems Manager, such as
-- @us-east-2@ for the US East (Ohio) Region. For a list of supported
-- __region__ values, see the __Region__ column in
-- <https://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints>
-- in the /Amazon Web Services General Reference/.
--
-- __session-id__ represents the ID of a Session Manager session, such as
-- @1a2b3c4dEXAMPLE@.
--
-- 'tokenValue', 'startSessionResponse_tokenValue' - An encrypted token value containing session and caller information. This
-- token is used to authenticate the connection to the managed node, and is
-- valid only long enough to ensure the connection is successful. Never
-- share your session\'s token.
--
-- 'httpStatus', 'startSessionResponse_httpStatus' - The response's http status code.
newStartSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSessionResponse
newStartSessionResponse pHttpStatus_ =
  StartSessionResponse'
    { sessionId = Prelude.Nothing,
      streamUrl = Prelude.Nothing,
      tokenValue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the session.
startSessionResponse_sessionId :: Lens.Lens' StartSessionResponse (Prelude.Maybe Prelude.Text)
startSessionResponse_sessionId = Lens.lens (\StartSessionResponse' {sessionId} -> sessionId) (\s@StartSessionResponse' {} a -> s {sessionId = a} :: StartSessionResponse)

-- | A URL back to SSM Agent on the managed node that the Session Manager
-- client uses to send commands and receive output from the node. Format:
-- @wss:\/\/ssmmessages.@__@region@__@.amazonaws.com\/v1\/data-channel\/@__@session-id@__@?stream=(input|output)@
--
-- __region__ represents the Region identifier for an Amazon Web Services
-- Region supported by Amazon Web Services Systems Manager, such as
-- @us-east-2@ for the US East (Ohio) Region. For a list of supported
-- __region__ values, see the __Region__ column in
-- <https://docs.aws.amazon.com/general/latest/gr/ssm.html#ssm_region Systems Manager service endpoints>
-- in the /Amazon Web Services General Reference/.
--
-- __session-id__ represents the ID of a Session Manager session, such as
-- @1a2b3c4dEXAMPLE@.
startSessionResponse_streamUrl :: Lens.Lens' StartSessionResponse (Prelude.Maybe Prelude.Text)
startSessionResponse_streamUrl = Lens.lens (\StartSessionResponse' {streamUrl} -> streamUrl) (\s@StartSessionResponse' {} a -> s {streamUrl = a} :: StartSessionResponse)

-- | An encrypted token value containing session and caller information. This
-- token is used to authenticate the connection to the managed node, and is
-- valid only long enough to ensure the connection is successful. Never
-- share your session\'s token.
startSessionResponse_tokenValue :: Lens.Lens' StartSessionResponse (Prelude.Maybe Prelude.Text)
startSessionResponse_tokenValue = Lens.lens (\StartSessionResponse' {tokenValue} -> tokenValue) (\s@StartSessionResponse' {} a -> s {tokenValue = a} :: StartSessionResponse)

-- | The response's http status code.
startSessionResponse_httpStatus :: Lens.Lens' StartSessionResponse Prelude.Int
startSessionResponse_httpStatus = Lens.lens (\StartSessionResponse' {httpStatus} -> httpStatus) (\s@StartSessionResponse' {} a -> s {httpStatus = a} :: StartSessionResponse)

instance Prelude.NFData StartSessionResponse where
  rnf StartSessionResponse' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf streamUrl
      `Prelude.seq` Prelude.rnf tokenValue
      `Prelude.seq` Prelude.rnf httpStatus
