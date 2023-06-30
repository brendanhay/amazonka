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
-- Module      : Amazonka.Athena.StartSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a session for running calculations within a workgroup. The
-- session is ready when it reaches an @IDLE@ state.
module Amazonka.Athena.StartSession
  ( -- * Creating a Request
    StartSession (..),
    newStartSession,

    -- * Request Lenses
    startSession_clientRequestToken,
    startSession_description,
    startSession_notebookVersion,
    startSession_sessionIdleTimeoutInMinutes,
    startSession_workGroup,
    startSession_engineConfiguration,

    -- * Destructuring the Response
    StartSessionResponse (..),
    newStartSessionResponse,

    -- * Response Lenses
    startSessionResponse_sessionId,
    startSessionResponse_state,
    startSessionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSession' smart constructor.
data StartSession = StartSession'
  { -- | A unique case-sensitive string used to ensure the request to create the
    -- session is idempotent (executes only once). If another
    -- @StartSessionRequest@ is received, the same response is returned and
    -- another session is not created. If a parameter has changed, an error is
    -- returned.
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for users. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The session description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The notebook version. This value is required only when requesting that a
    -- notebook server be started for the session. The only valid notebook
    -- version is @Jupyter1.0@.
    notebookVersion :: Prelude.Maybe Prelude.Text,
    -- | The idle timeout in minutes for the session.
    sessionIdleTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The workgroup to which the session belongs.
    workGroup :: Prelude.Text,
    -- | Contains engine data processing unit (DPU) configuration settings and
    -- parameter mappings.
    engineConfiguration :: EngineConfiguration
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
-- 'clientRequestToken', 'startSession_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- session is idempotent (executes only once). If another
-- @StartSessionRequest@ is received, the same response is returned and
-- another session is not created. If a parameter has changed, an error is
-- returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'description', 'startSession_description' - The session description.
--
-- 'notebookVersion', 'startSession_notebookVersion' - The notebook version. This value is required only when requesting that a
-- notebook server be started for the session. The only valid notebook
-- version is @Jupyter1.0@.
--
-- 'sessionIdleTimeoutInMinutes', 'startSession_sessionIdleTimeoutInMinutes' - The idle timeout in minutes for the session.
--
-- 'workGroup', 'startSession_workGroup' - The workgroup to which the session belongs.
--
-- 'engineConfiguration', 'startSession_engineConfiguration' - Contains engine data processing unit (DPU) configuration settings and
-- parameter mappings.
newStartSession ::
  -- | 'workGroup'
  Prelude.Text ->
  -- | 'engineConfiguration'
  EngineConfiguration ->
  StartSession
newStartSession pWorkGroup_ pEngineConfiguration_ =
  StartSession'
    { clientRequestToken = Prelude.Nothing,
      description = Prelude.Nothing,
      notebookVersion = Prelude.Nothing,
      sessionIdleTimeoutInMinutes = Prelude.Nothing,
      workGroup = pWorkGroup_,
      engineConfiguration = pEngineConfiguration_
    }

-- | A unique case-sensitive string used to ensure the request to create the
-- session is idempotent (executes only once). If another
-- @StartSessionRequest@ is received, the same response is returned and
-- another session is not created. If a parameter has changed, an error is
-- returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
startSession_clientRequestToken :: Lens.Lens' StartSession (Prelude.Maybe Prelude.Text)
startSession_clientRequestToken = Lens.lens (\StartSession' {clientRequestToken} -> clientRequestToken) (\s@StartSession' {} a -> s {clientRequestToken = a} :: StartSession)

-- | The session description.
startSession_description :: Lens.Lens' StartSession (Prelude.Maybe Prelude.Text)
startSession_description = Lens.lens (\StartSession' {description} -> description) (\s@StartSession' {} a -> s {description = a} :: StartSession)

-- | The notebook version. This value is required only when requesting that a
-- notebook server be started for the session. The only valid notebook
-- version is @Jupyter1.0@.
startSession_notebookVersion :: Lens.Lens' StartSession (Prelude.Maybe Prelude.Text)
startSession_notebookVersion = Lens.lens (\StartSession' {notebookVersion} -> notebookVersion) (\s@StartSession' {} a -> s {notebookVersion = a} :: StartSession)

-- | The idle timeout in minutes for the session.
startSession_sessionIdleTimeoutInMinutes :: Lens.Lens' StartSession (Prelude.Maybe Prelude.Natural)
startSession_sessionIdleTimeoutInMinutes = Lens.lens (\StartSession' {sessionIdleTimeoutInMinutes} -> sessionIdleTimeoutInMinutes) (\s@StartSession' {} a -> s {sessionIdleTimeoutInMinutes = a} :: StartSession)

-- | The workgroup to which the session belongs.
startSession_workGroup :: Lens.Lens' StartSession Prelude.Text
startSession_workGroup = Lens.lens (\StartSession' {workGroup} -> workGroup) (\s@StartSession' {} a -> s {workGroup = a} :: StartSession)

-- | Contains engine data processing unit (DPU) configuration settings and
-- parameter mappings.
startSession_engineConfiguration :: Lens.Lens' StartSession EngineConfiguration
startSession_engineConfiguration = Lens.lens (\StartSession' {engineConfiguration} -> engineConfiguration) (\s@StartSession' {} a -> s {engineConfiguration = a} :: StartSession)

instance Core.AWSRequest StartSession where
  type AWSResponse StartSession = StartSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSessionResponse'
            Prelude.<$> (x Data..?> "SessionId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSession where
  hashWithSalt _salt StartSession' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notebookVersion
      `Prelude.hashWithSalt` sessionIdleTimeoutInMinutes
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` engineConfiguration

instance Prelude.NFData StartSession where
  rnf StartSession' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notebookVersion
      `Prelude.seq` Prelude.rnf sessionIdleTimeoutInMinutes
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf engineConfiguration

instance Data.ToHeaders StartSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonAthena.StartSession" :: Prelude.ByteString),
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
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("NotebookVersion" Data..=)
              Prelude.<$> notebookVersion,
            ("SessionIdleTimeoutInMinutes" Data..=)
              Prelude.<$> sessionIdleTimeoutInMinutes,
            Prelude.Just ("WorkGroup" Data..= workGroup),
            Prelude.Just
              ("EngineConfiguration" Data..= engineConfiguration)
          ]
      )

instance Data.ToPath StartSession where
  toPath = Prelude.const "/"

instance Data.ToQuery StartSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSessionResponse' smart constructor.
data StartSessionResponse = StartSessionResponse'
  { -- | The session ID.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The state of the session. A description of each state follows.
    --
    -- @CREATING@ - The session is being started, including acquiring
    -- resources.
    --
    -- @CREATED@ - The session has been started.
    --
    -- @IDLE@ - The session is able to accept a calculation.
    --
    -- @BUSY@ - The session is processing another task and is unable to accept
    -- a calculation.
    --
    -- @TERMINATING@ - The session is in the process of shutting down.
    --
    -- @TERMINATED@ - The session and its resources are no longer running.
    --
    -- @DEGRADED@ - The session has no healthy coordinators.
    --
    -- @FAILED@ - Due to a failure, the session and its resources are no longer
    -- running.
    state :: Prelude.Maybe SessionState,
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
-- 'sessionId', 'startSessionResponse_sessionId' - The session ID.
--
-- 'state', 'startSessionResponse_state' - The state of the session. A description of each state follows.
--
-- @CREATING@ - The session is being started, including acquiring
-- resources.
--
-- @CREATED@ - The session has been started.
--
-- @IDLE@ - The session is able to accept a calculation.
--
-- @BUSY@ - The session is processing another task and is unable to accept
-- a calculation.
--
-- @TERMINATING@ - The session is in the process of shutting down.
--
-- @TERMINATED@ - The session and its resources are no longer running.
--
-- @DEGRADED@ - The session has no healthy coordinators.
--
-- @FAILED@ - Due to a failure, the session and its resources are no longer
-- running.
--
-- 'httpStatus', 'startSessionResponse_httpStatus' - The response's http status code.
newStartSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSessionResponse
newStartSessionResponse pHttpStatus_ =
  StartSessionResponse'
    { sessionId = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session ID.
startSessionResponse_sessionId :: Lens.Lens' StartSessionResponse (Prelude.Maybe Prelude.Text)
startSessionResponse_sessionId = Lens.lens (\StartSessionResponse' {sessionId} -> sessionId) (\s@StartSessionResponse' {} a -> s {sessionId = a} :: StartSessionResponse)

-- | The state of the session. A description of each state follows.
--
-- @CREATING@ - The session is being started, including acquiring
-- resources.
--
-- @CREATED@ - The session has been started.
--
-- @IDLE@ - The session is able to accept a calculation.
--
-- @BUSY@ - The session is processing another task and is unable to accept
-- a calculation.
--
-- @TERMINATING@ - The session is in the process of shutting down.
--
-- @TERMINATED@ - The session and its resources are no longer running.
--
-- @DEGRADED@ - The session has no healthy coordinators.
--
-- @FAILED@ - Due to a failure, the session and its resources are no longer
-- running.
startSessionResponse_state :: Lens.Lens' StartSessionResponse (Prelude.Maybe SessionState)
startSessionResponse_state = Lens.lens (\StartSessionResponse' {state} -> state) (\s@StartSessionResponse' {} a -> s {state = a} :: StartSessionResponse)

-- | The response's http status code.
startSessionResponse_httpStatus :: Lens.Lens' StartSessionResponse Prelude.Int
startSessionResponse_httpStatus = Lens.lens (\StartSessionResponse' {httpStatus} -> httpStatus) (\s@StartSessionResponse' {} a -> s {httpStatus = a} :: StartSessionResponse)

instance Prelude.NFData StartSessionResponse where
  rnf StartSessionResponse' {..} =
    Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
