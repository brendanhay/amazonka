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
-- Module      : Amazonka.VoiceId.EvaluateSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates a specified session based on audio data accumulated during a
-- streaming Amazon Connect Voice ID call.
module Amazonka.VoiceId.EvaluateSession
  ( -- * Creating a Request
    EvaluateSession (..),
    newEvaluateSession,

    -- * Request Lenses
    evaluateSession_domainId,
    evaluateSession_sessionNameOrId,

    -- * Destructuring the Response
    EvaluateSessionResponse (..),
    newEvaluateSessionResponse,

    -- * Response Lenses
    evaluateSessionResponse_authenticationResult,
    evaluateSessionResponse_domainId,
    evaluateSessionResponse_fraudDetectionResult,
    evaluateSessionResponse_sessionId,
    evaluateSessionResponse_sessionName,
    evaluateSessionResponse_streamingStatus,
    evaluateSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newEvaluateSession' smart constructor.
data EvaluateSession = EvaluateSession'
  { -- | The identifier of the domain where the session started.
    domainId :: Prelude.Text,
    -- | The session identifier, or name of the session, that you want to
    -- evaluate. In Voice ID integration, this is the Contact-Id.
    sessionNameOrId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'evaluateSession_domainId' - The identifier of the domain where the session started.
--
-- 'sessionNameOrId', 'evaluateSession_sessionNameOrId' - The session identifier, or name of the session, that you want to
-- evaluate. In Voice ID integration, this is the Contact-Id.
newEvaluateSession ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'sessionNameOrId'
  Prelude.Text ->
  EvaluateSession
newEvaluateSession pDomainId_ pSessionNameOrId_ =
  EvaluateSession'
    { domainId = pDomainId_,
      sessionNameOrId = pSessionNameOrId_
    }

-- | The identifier of the domain where the session started.
evaluateSession_domainId :: Lens.Lens' EvaluateSession Prelude.Text
evaluateSession_domainId = Lens.lens (\EvaluateSession' {domainId} -> domainId) (\s@EvaluateSession' {} a -> s {domainId = a} :: EvaluateSession)

-- | The session identifier, or name of the session, that you want to
-- evaluate. In Voice ID integration, this is the Contact-Id.
evaluateSession_sessionNameOrId :: Lens.Lens' EvaluateSession Prelude.Text
evaluateSession_sessionNameOrId = Lens.lens (\EvaluateSession' {sessionNameOrId} -> sessionNameOrId) (\s@EvaluateSession' {} a -> s {sessionNameOrId = a} :: EvaluateSession)

instance Core.AWSRequest EvaluateSession where
  type
    AWSResponse EvaluateSession =
      EvaluateSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateSessionResponse'
            Prelude.<$> (x Data..?> "AuthenticationResult")
            Prelude.<*> (x Data..?> "DomainId")
            Prelude.<*> (x Data..?> "FraudDetectionResult")
            Prelude.<*> (x Data..?> "SessionId")
            Prelude.<*> (x Data..?> "SessionName")
            Prelude.<*> (x Data..?> "StreamingStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EvaluateSession where
  hashWithSalt _salt EvaluateSession' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` sessionNameOrId

instance Prelude.NFData EvaluateSession where
  rnf EvaluateSession' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf sessionNameOrId

instance Data.ToHeaders EvaluateSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.EvaluateSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EvaluateSession where
  toJSON EvaluateSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just
              ("SessionNameOrId" Data..= sessionNameOrId)
          ]
      )

instance Data.ToPath EvaluateSession where
  toPath = Prelude.const "/"

instance Data.ToQuery EvaluateSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEvaluateSessionResponse' smart constructor.
data EvaluateSessionResponse = EvaluateSessionResponse'
  { -- | Details resulting from the authentication process, such as
    -- authentication decision and authentication score.
    authenticationResult :: Prelude.Maybe AuthenticationResult,
    -- | The identifier of the domain containing the session.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | Details resulting from the fraud detection process, such as fraud
    -- detection decision and risk score.
    fraudDetectionResult :: Prelude.Maybe FraudDetectionResult,
    -- | The service-generated identifier of the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The client-provided name of the session.
    sessionName :: Prelude.Maybe Prelude.Text,
    -- | The current status of audio streaming for this session. This field is
    -- useful to infer next steps when the Authentication or Fraud Detection
    -- results are empty or the decision is @NOT_ENOUGH_SPEECH@. In this
    -- situation, if the @StreamingStatus@ is @ONGOING\/PENDING_CONFIGURATION@,
    -- it can mean that the client should call the API again later, after Voice
    -- ID has enough audio to produce a result. If the decision remains
    -- @NOT_ENOUGH_SPEECH@ even after @StreamingStatus@ is @ENDED@, it means
    -- that the previously streamed session did not have enough speech to
    -- perform evaluation, and a new streaming session is needed to try again.
    streamingStatus :: Prelude.Maybe StreamingStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationResult', 'evaluateSessionResponse_authenticationResult' - Details resulting from the authentication process, such as
-- authentication decision and authentication score.
--
-- 'domainId', 'evaluateSessionResponse_domainId' - The identifier of the domain containing the session.
--
-- 'fraudDetectionResult', 'evaluateSessionResponse_fraudDetectionResult' - Details resulting from the fraud detection process, such as fraud
-- detection decision and risk score.
--
-- 'sessionId', 'evaluateSessionResponse_sessionId' - The service-generated identifier of the session.
--
-- 'sessionName', 'evaluateSessionResponse_sessionName' - The client-provided name of the session.
--
-- 'streamingStatus', 'evaluateSessionResponse_streamingStatus' - The current status of audio streaming for this session. This field is
-- useful to infer next steps when the Authentication or Fraud Detection
-- results are empty or the decision is @NOT_ENOUGH_SPEECH@. In this
-- situation, if the @StreamingStatus@ is @ONGOING\/PENDING_CONFIGURATION@,
-- it can mean that the client should call the API again later, after Voice
-- ID has enough audio to produce a result. If the decision remains
-- @NOT_ENOUGH_SPEECH@ even after @StreamingStatus@ is @ENDED@, it means
-- that the previously streamed session did not have enough speech to
-- perform evaluation, and a new streaming session is needed to try again.
--
-- 'httpStatus', 'evaluateSessionResponse_httpStatus' - The response's http status code.
newEvaluateSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EvaluateSessionResponse
newEvaluateSessionResponse pHttpStatus_ =
  EvaluateSessionResponse'
    { authenticationResult =
        Prelude.Nothing,
      domainId = Prelude.Nothing,
      fraudDetectionResult = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      sessionName = Prelude.Nothing,
      streamingStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details resulting from the authentication process, such as
-- authentication decision and authentication score.
evaluateSessionResponse_authenticationResult :: Lens.Lens' EvaluateSessionResponse (Prelude.Maybe AuthenticationResult)
evaluateSessionResponse_authenticationResult = Lens.lens (\EvaluateSessionResponse' {authenticationResult} -> authenticationResult) (\s@EvaluateSessionResponse' {} a -> s {authenticationResult = a} :: EvaluateSessionResponse)

-- | The identifier of the domain containing the session.
evaluateSessionResponse_domainId :: Lens.Lens' EvaluateSessionResponse (Prelude.Maybe Prelude.Text)
evaluateSessionResponse_domainId = Lens.lens (\EvaluateSessionResponse' {domainId} -> domainId) (\s@EvaluateSessionResponse' {} a -> s {domainId = a} :: EvaluateSessionResponse)

-- | Details resulting from the fraud detection process, such as fraud
-- detection decision and risk score.
evaluateSessionResponse_fraudDetectionResult :: Lens.Lens' EvaluateSessionResponse (Prelude.Maybe FraudDetectionResult)
evaluateSessionResponse_fraudDetectionResult = Lens.lens (\EvaluateSessionResponse' {fraudDetectionResult} -> fraudDetectionResult) (\s@EvaluateSessionResponse' {} a -> s {fraudDetectionResult = a} :: EvaluateSessionResponse)

-- | The service-generated identifier of the session.
evaluateSessionResponse_sessionId :: Lens.Lens' EvaluateSessionResponse (Prelude.Maybe Prelude.Text)
evaluateSessionResponse_sessionId = Lens.lens (\EvaluateSessionResponse' {sessionId} -> sessionId) (\s@EvaluateSessionResponse' {} a -> s {sessionId = a} :: EvaluateSessionResponse)

-- | The client-provided name of the session.
evaluateSessionResponse_sessionName :: Lens.Lens' EvaluateSessionResponse (Prelude.Maybe Prelude.Text)
evaluateSessionResponse_sessionName = Lens.lens (\EvaluateSessionResponse' {sessionName} -> sessionName) (\s@EvaluateSessionResponse' {} a -> s {sessionName = a} :: EvaluateSessionResponse)

-- | The current status of audio streaming for this session. This field is
-- useful to infer next steps when the Authentication or Fraud Detection
-- results are empty or the decision is @NOT_ENOUGH_SPEECH@. In this
-- situation, if the @StreamingStatus@ is @ONGOING\/PENDING_CONFIGURATION@,
-- it can mean that the client should call the API again later, after Voice
-- ID has enough audio to produce a result. If the decision remains
-- @NOT_ENOUGH_SPEECH@ even after @StreamingStatus@ is @ENDED@, it means
-- that the previously streamed session did not have enough speech to
-- perform evaluation, and a new streaming session is needed to try again.
evaluateSessionResponse_streamingStatus :: Lens.Lens' EvaluateSessionResponse (Prelude.Maybe StreamingStatus)
evaluateSessionResponse_streamingStatus = Lens.lens (\EvaluateSessionResponse' {streamingStatus} -> streamingStatus) (\s@EvaluateSessionResponse' {} a -> s {streamingStatus = a} :: EvaluateSessionResponse)

-- | The response's http status code.
evaluateSessionResponse_httpStatus :: Lens.Lens' EvaluateSessionResponse Prelude.Int
evaluateSessionResponse_httpStatus = Lens.lens (\EvaluateSessionResponse' {httpStatus} -> httpStatus) (\s@EvaluateSessionResponse' {} a -> s {httpStatus = a} :: EvaluateSessionResponse)

instance Prelude.NFData EvaluateSessionResponse where
  rnf EvaluateSessionResponse' {..} =
    Prelude.rnf authenticationResult
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fraudDetectionResult
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf sessionName
      `Prelude.seq` Prelude.rnf streamingStatus
      `Prelude.seq` Prelude.rnf httpStatus
