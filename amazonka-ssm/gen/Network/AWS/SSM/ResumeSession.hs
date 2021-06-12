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
-- Module      : Network.AWS.SSM.ResumeSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reconnects a session to an instance after it has been disconnected.
-- Connections can be resumed for disconnected sessions, but not terminated
-- sessions.
--
-- This command is primarily for use by client machines to automatically
-- reconnect during intermittent network issues. It is not intended for any
-- other use.
module Network.AWS.SSM.ResumeSession
  ( -- * Creating a Request
    ResumeSession (..),
    newResumeSession,

    -- * Request Lenses
    resumeSession_sessionId,

    -- * Destructuring the Response
    ResumeSessionResponse (..),
    newResumeSessionResponse,

    -- * Response Lenses
    resumeSessionResponse_sessionId,
    resumeSessionResponse_streamUrl,
    resumeSessionResponse_tokenValue,
    resumeSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newResumeSession' smart constructor.
data ResumeSession = ResumeSession'
  { -- | The ID of the disconnected session to resume.
    sessionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ResumeSession
newResumeSession pSessionId_ =
  ResumeSession' {sessionId = pSessionId_}

-- | The ID of the disconnected session to resume.
resumeSession_sessionId :: Lens.Lens' ResumeSession Core.Text
resumeSession_sessionId = Lens.lens (\ResumeSession' {sessionId} -> sessionId) (\s@ResumeSession' {} a -> s {sessionId = a} :: ResumeSession)

instance Core.AWSRequest ResumeSession where
  type
    AWSResponse ResumeSession =
      ResumeSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeSessionResponse'
            Core.<$> (x Core..?> "SessionId")
            Core.<*> (x Core..?> "StreamUrl")
            Core.<*> (x Core..?> "TokenValue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResumeSession

instance Core.NFData ResumeSession

instance Core.ToHeaders ResumeSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ResumeSession" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResumeSession where
  toJSON ResumeSession' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SessionId" Core..= sessionId)]
      )

instance Core.ToPath ResumeSession where
  toPath = Core.const "/"

instance Core.ToQuery ResumeSession where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newResumeSessionResponse' smart constructor.
data ResumeSessionResponse = ResumeSessionResponse'
  { -- | The ID of the session.
    sessionId :: Core.Maybe Core.Text,
    -- | A URL back to SSM Agent on the instance that the Session Manager client
    -- uses to send commands and receive output from the instance. Format:
    -- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@.
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
-- Create a value of 'ResumeSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionId', 'resumeSessionResponse_sessionId' - The ID of the session.
--
-- 'streamUrl', 'resumeSessionResponse_streamUrl' - A URL back to SSM Agent on the instance that the Session Manager client
-- uses to send commands and receive output from the instance. Format:
-- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@.
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
-- 'tokenValue', 'resumeSessionResponse_tokenValue' - An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the instance.
--
-- 'httpStatus', 'resumeSessionResponse_httpStatus' - The response's http status code.
newResumeSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ResumeSessionResponse
newResumeSessionResponse pHttpStatus_ =
  ResumeSessionResponse'
    { sessionId = Core.Nothing,
      streamUrl = Core.Nothing,
      tokenValue = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the session.
resumeSessionResponse_sessionId :: Lens.Lens' ResumeSessionResponse (Core.Maybe Core.Text)
resumeSessionResponse_sessionId = Lens.lens (\ResumeSessionResponse' {sessionId} -> sessionId) (\s@ResumeSessionResponse' {} a -> s {sessionId = a} :: ResumeSessionResponse)

-- | A URL back to SSM Agent on the instance that the Session Manager client
-- uses to send commands and receive output from the instance. Format:
-- @wss:\/\/ssmmessages.region.amazonaws.com\/v1\/data-channel\/session-id?stream=(input|output)@.
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
resumeSessionResponse_streamUrl :: Lens.Lens' ResumeSessionResponse (Core.Maybe Core.Text)
resumeSessionResponse_streamUrl = Lens.lens (\ResumeSessionResponse' {streamUrl} -> streamUrl) (\s@ResumeSessionResponse' {} a -> s {streamUrl = a} :: ResumeSessionResponse)

-- | An encrypted token value containing session and caller information. Used
-- to authenticate the connection to the instance.
resumeSessionResponse_tokenValue :: Lens.Lens' ResumeSessionResponse (Core.Maybe Core.Text)
resumeSessionResponse_tokenValue = Lens.lens (\ResumeSessionResponse' {tokenValue} -> tokenValue) (\s@ResumeSessionResponse' {} a -> s {tokenValue = a} :: ResumeSessionResponse)

-- | The response's http status code.
resumeSessionResponse_httpStatus :: Lens.Lens' ResumeSessionResponse Core.Int
resumeSessionResponse_httpStatus = Lens.lens (\ResumeSessionResponse' {httpStatus} -> httpStatus) (\s@ResumeSessionResponse' {} a -> s {httpStatus = a} :: ResumeSessionResponse)

instance Core.NFData ResumeSessionResponse
