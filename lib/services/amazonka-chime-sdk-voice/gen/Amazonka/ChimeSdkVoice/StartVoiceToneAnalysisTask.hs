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
-- Module      : Amazonka.ChimeSdkVoice.StartVoiceToneAnalysisTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a voice tone analysis task. For more information about voice tone
-- analysis, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/pstn-voice-analytics.html Using Amazon Chime SDK voice analytics>
-- in the /Amazon Chime SDK Developer Guide/.
--
-- Before starting any voice tone analysis tasks, you must provide all
-- notices and obtain all consents from the speaker as required under
-- applicable privacy and biometrics laws, and as required under the
-- <https://aws.amazon.com/service-terms/ AWS service terms> for the Amazon
-- Chime SDK.
module Amazonka.ChimeSdkVoice.StartVoiceToneAnalysisTask
  ( -- * Creating a Request
    StartVoiceToneAnalysisTask (..),
    newStartVoiceToneAnalysisTask,

    -- * Request Lenses
    startVoiceToneAnalysisTask_clientRequestToken,
    startVoiceToneAnalysisTask_voiceConnectorId,
    startVoiceToneAnalysisTask_transactionId,
    startVoiceToneAnalysisTask_languageCode,

    -- * Destructuring the Response
    StartVoiceToneAnalysisTaskResponse (..),
    newStartVoiceToneAnalysisTaskResponse,

    -- * Response Lenses
    startVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask,
    startVoiceToneAnalysisTaskResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartVoiceToneAnalysisTask' smart constructor.
data StartVoiceToneAnalysisTask = StartVoiceToneAnalysisTask'
  { -- | The unique identifier for the client request. Use a different token for
    -- different voice tone analysis tasks.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The transaction ID.
    transactionId :: Prelude.Text,
    -- | The language code.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVoiceToneAnalysisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startVoiceToneAnalysisTask_clientRequestToken' - The unique identifier for the client request. Use a different token for
-- different voice tone analysis tasks.
--
-- 'voiceConnectorId', 'startVoiceToneAnalysisTask_voiceConnectorId' - The Voice Connector ID.
--
-- 'transactionId', 'startVoiceToneAnalysisTask_transactionId' - The transaction ID.
--
-- 'languageCode', 'startVoiceToneAnalysisTask_languageCode' - The language code.
newStartVoiceToneAnalysisTask ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'transactionId'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartVoiceToneAnalysisTask
newStartVoiceToneAnalysisTask
  pVoiceConnectorId_
  pTransactionId_
  pLanguageCode_ =
    StartVoiceToneAnalysisTask'
      { clientRequestToken =
          Prelude.Nothing,
        voiceConnectorId = pVoiceConnectorId_,
        transactionId = pTransactionId_,
        languageCode = pLanguageCode_
      }

-- | The unique identifier for the client request. Use a different token for
-- different voice tone analysis tasks.
startVoiceToneAnalysisTask_clientRequestToken :: Lens.Lens' StartVoiceToneAnalysisTask (Prelude.Maybe Prelude.Text)
startVoiceToneAnalysisTask_clientRequestToken = Lens.lens (\StartVoiceToneAnalysisTask' {clientRequestToken} -> clientRequestToken) (\s@StartVoiceToneAnalysisTask' {} a -> s {clientRequestToken = a} :: StartVoiceToneAnalysisTask)

-- | The Voice Connector ID.
startVoiceToneAnalysisTask_voiceConnectorId :: Lens.Lens' StartVoiceToneAnalysisTask Prelude.Text
startVoiceToneAnalysisTask_voiceConnectorId = Lens.lens (\StartVoiceToneAnalysisTask' {voiceConnectorId} -> voiceConnectorId) (\s@StartVoiceToneAnalysisTask' {} a -> s {voiceConnectorId = a} :: StartVoiceToneAnalysisTask)

-- | The transaction ID.
startVoiceToneAnalysisTask_transactionId :: Lens.Lens' StartVoiceToneAnalysisTask Prelude.Text
startVoiceToneAnalysisTask_transactionId = Lens.lens (\StartVoiceToneAnalysisTask' {transactionId} -> transactionId) (\s@StartVoiceToneAnalysisTask' {} a -> s {transactionId = a} :: StartVoiceToneAnalysisTask)

-- | The language code.
startVoiceToneAnalysisTask_languageCode :: Lens.Lens' StartVoiceToneAnalysisTask LanguageCode
startVoiceToneAnalysisTask_languageCode = Lens.lens (\StartVoiceToneAnalysisTask' {languageCode} -> languageCode) (\s@StartVoiceToneAnalysisTask' {} a -> s {languageCode = a} :: StartVoiceToneAnalysisTask)

instance Core.AWSRequest StartVoiceToneAnalysisTask where
  type
    AWSResponse StartVoiceToneAnalysisTask =
      StartVoiceToneAnalysisTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartVoiceToneAnalysisTaskResponse'
            Prelude.<$> (x Data..?> "VoiceToneAnalysisTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartVoiceToneAnalysisTask where
  hashWithSalt _salt StartVoiceToneAnalysisTask' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData StartVoiceToneAnalysisTask where
  rnf StartVoiceToneAnalysisTask' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders StartVoiceToneAnalysisTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartVoiceToneAnalysisTask where
  toJSON StartVoiceToneAnalysisTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("TransactionId" Data..= transactionId),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath StartVoiceToneAnalysisTask where
  toPath StartVoiceToneAnalysisTask' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/voice-tone-analysis-tasks"
      ]

instance Data.ToQuery StartVoiceToneAnalysisTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartVoiceToneAnalysisTaskResponse' smart constructor.
data StartVoiceToneAnalysisTaskResponse = StartVoiceToneAnalysisTaskResponse'
  { -- | The details of the voice tone analysis task.
    voiceToneAnalysisTask :: Prelude.Maybe VoiceToneAnalysisTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartVoiceToneAnalysisTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceToneAnalysisTask', 'startVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask' - The details of the voice tone analysis task.
--
-- 'httpStatus', 'startVoiceToneAnalysisTaskResponse_httpStatus' - The response's http status code.
newStartVoiceToneAnalysisTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartVoiceToneAnalysisTaskResponse
newStartVoiceToneAnalysisTaskResponse pHttpStatus_ =
  StartVoiceToneAnalysisTaskResponse'
    { voiceToneAnalysisTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the voice tone analysis task.
startVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask :: Lens.Lens' StartVoiceToneAnalysisTaskResponse (Prelude.Maybe VoiceToneAnalysisTask)
startVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask = Lens.lens (\StartVoiceToneAnalysisTaskResponse' {voiceToneAnalysisTask} -> voiceToneAnalysisTask) (\s@StartVoiceToneAnalysisTaskResponse' {} a -> s {voiceToneAnalysisTask = a} :: StartVoiceToneAnalysisTaskResponse)

-- | The response's http status code.
startVoiceToneAnalysisTaskResponse_httpStatus :: Lens.Lens' StartVoiceToneAnalysisTaskResponse Prelude.Int
startVoiceToneAnalysisTaskResponse_httpStatus = Lens.lens (\StartVoiceToneAnalysisTaskResponse' {httpStatus} -> httpStatus) (\s@StartVoiceToneAnalysisTaskResponse' {} a -> s {httpStatus = a} :: StartVoiceToneAnalysisTaskResponse)

instance
  Prelude.NFData
    StartVoiceToneAnalysisTaskResponse
  where
  rnf StartVoiceToneAnalysisTaskResponse' {..} =
    Prelude.rnf voiceToneAnalysisTask
      `Prelude.seq` Prelude.rnf httpStatus
