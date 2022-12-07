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
-- Module      : Amazonka.SSM.ResumeSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reconnects a session to a managed node after it has been disconnected.
-- Connections can be resumed for disconnected sessions, but not terminated
-- sessions.
--
-- This command is primarily for use by client machines to automatically
-- reconnect during intermittent network issues. It isn\'t intended for any
-- other use.
module Amazonka.SSM.ResumeSession
  ( -- * Creating a Request
    ResumeSession (..),
    newResumeSession,

    -- * Request Lenses
    resumeSession_sessionId,

    -- * Destructuring the Response
    ResumeSessionResponse (..),
    newResumeSessionResponse,

    -- * Response Lenses
    resumeSessionResponse_tokenValue,
    resumeSessionResponse_streamUrl,
    resumeSessionResponse_sessionId,
    resumeSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newResumeSession' smart constructor.
data ResumeSession = ResumeSession'
  { -- | The ID of the disconnected session to resume.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'resumeSession_sessionId' - The ID of the disconnected session to resume.
newResumeSession ::
  -- | 'sessionId'
  Prelude.Text ->
  ResumeSession
newResumeSession pSessionId_ =
  ResumeSession' {sessionId = pSessionId_}

-- | The ID of the disconnected session to resume.
resumeSession_sessionId :: Lens.Lens' ResumeSession Prelude.Text
resumeSession_sessionId = Lens.lens (\ResumeSession' {sessionId} -> sessionId) (\s@ResumeSession' {} a -> s {sessionId = a} :: ResumeSession)

instance Core.AWSRequest ResumeSession where
  type
    AWSResponse ResumeSession =
      ResumeSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeSessionResponse'
            Prelude.<$> (x Data..?> "TokenValue")
            Prelude.<*> (x Data..?> "StreamUrl")
            Prelude.<*> (x Data..?> "SessionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResumeSession where
  hashWithSalt _salt ResumeSession' {..} =
    _salt `Prelude.hashWithSalt` sessionId

instance Prelude.NFData ResumeSession where
  rnf ResumeSession' {..} = Prelude.rnf sessionId

instance Data.ToHeaders ResumeSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.ResumeSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResumeSession where
  toJSON ResumeSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SessionId" Data..= sessionId)]
      )

instance Data.ToPath ResumeSession where
  toPath = Prelude.const "/"

instance Data.ToQuery ResumeSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeSessionResponse' smart constructor.
data ResumeSessionResponse = ResumeSessionResponse'
  { -- | An encrypted token value containing session and caller information. Used
    -- to authenticate the connection to the managed node.
    tokenValue :: Prelude.Maybe Prelude.Text,
    -- | A URL back to SSM Agent on the managed node that the Session Manager
    -- client uses to send commands and receive output from the managed node.
    -- Format:
    -- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@.
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
    -- | The ID of the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenValue', 'resumeSessionResponse_tokenValue' - An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the managed node.
--
-- 'streamUrl', 'resumeSessionResponse_streamUrl' - A URL back to SSM Agent on the managed node that the Session Manager
-- client uses to send commands and receive output from the managed node.
-- Format:
-- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@.
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
-- 'sessionId', 'resumeSessionResponse_sessionId' - The ID of the session.
--
-- 'httpStatus', 'resumeSessionResponse_httpStatus' - The response's http status code.
newResumeSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResumeSessionResponse
newResumeSessionResponse pHttpStatus_ =
  ResumeSessionResponse'
    { tokenValue =
        Prelude.Nothing,
      streamUrl = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the managed node.
resumeSessionResponse_tokenValue :: Lens.Lens' ResumeSessionResponse (Prelude.Maybe Prelude.Text)
resumeSessionResponse_tokenValue = Lens.lens (\ResumeSessionResponse' {tokenValue} -> tokenValue) (\s@ResumeSessionResponse' {} a -> s {tokenValue = a} :: ResumeSessionResponse)

-- | A URL back to SSM Agent on the managed node that the Session Manager
-- client uses to send commands and receive output from the managed node.
-- Format:
-- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@.
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
resumeSessionResponse_streamUrl :: Lens.Lens' ResumeSessionResponse (Prelude.Maybe Prelude.Text)
resumeSessionResponse_streamUrl = Lens.lens (\ResumeSessionResponse' {streamUrl} -> streamUrl) (\s@ResumeSessionResponse' {} a -> s {streamUrl = a} :: ResumeSessionResponse)

-- | The ID of the session.
resumeSessionResponse_sessionId :: Lens.Lens' ResumeSessionResponse (Prelude.Maybe Prelude.Text)
resumeSessionResponse_sessionId = Lens.lens (\ResumeSessionResponse' {sessionId} -> sessionId) (\s@ResumeSessionResponse' {} a -> s {sessionId = a} :: ResumeSessionResponse)

-- | The response's http status code.
resumeSessionResponse_httpStatus :: Lens.Lens' ResumeSessionResponse Prelude.Int
resumeSessionResponse_httpStatus = Lens.lens (\ResumeSessionResponse' {httpStatus} -> httpStatus) (\s@ResumeSessionResponse' {} a -> s {httpStatus = a} :: ResumeSessionResponse)

instance Prelude.NFData ResumeSessionResponse where
  rnf ResumeSessionResponse' {..} =
    Prelude.rnf tokenValue
      `Prelude.seq` Prelude.rnf streamUrl
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf httpStatus
