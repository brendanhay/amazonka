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
-- Module      : Amazonka.ChimeSdkVoice.StartSpeakerSearchTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a speaker search task.
--
-- Before starting any speaker search tasks, you must provide all notices
-- and obtain all consents from the speaker as required under applicable
-- privacy and biometrics laws, and as required under the
-- <https://aws.amazon.com/service-terms/ AWS service terms> for the Amazon
-- Chime SDK.
module Amazonka.ChimeSdkVoice.StartSpeakerSearchTask
  ( -- * Creating a Request
    StartSpeakerSearchTask (..),
    newStartSpeakerSearchTask,

    -- * Request Lenses
    startSpeakerSearchTask_callLeg,
    startSpeakerSearchTask_clientRequestToken,
    startSpeakerSearchTask_voiceConnectorId,
    startSpeakerSearchTask_transactionId,
    startSpeakerSearchTask_voiceProfileDomainId,

    -- * Destructuring the Response
    StartSpeakerSearchTaskResponse (..),
    newStartSpeakerSearchTaskResponse,

    -- * Response Lenses
    startSpeakerSearchTaskResponse_speakerSearchTask,
    startSpeakerSearchTaskResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSpeakerSearchTask' smart constructor.
data StartSpeakerSearchTask = StartSpeakerSearchTask'
  { -- | Specifies which call leg to stream for speaker search.
    callLeg :: Prelude.Maybe CallLegType,
    -- | The unique identifier for the client request. Use a different token for
    -- different speaker search tasks.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The transaction ID of the call being analyzed.
    transactionId :: Prelude.Text,
    -- | The ID of the voice profile domain that will store the voice profile.
    voiceProfileDomainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSpeakerSearchTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callLeg', 'startSpeakerSearchTask_callLeg' - Specifies which call leg to stream for speaker search.
--
-- 'clientRequestToken', 'startSpeakerSearchTask_clientRequestToken' - The unique identifier for the client request. Use a different token for
-- different speaker search tasks.
--
-- 'voiceConnectorId', 'startSpeakerSearchTask_voiceConnectorId' - The Voice Connector ID.
--
-- 'transactionId', 'startSpeakerSearchTask_transactionId' - The transaction ID of the call being analyzed.
--
-- 'voiceProfileDomainId', 'startSpeakerSearchTask_voiceProfileDomainId' - The ID of the voice profile domain that will store the voice profile.
newStartSpeakerSearchTask ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'transactionId'
  Prelude.Text ->
  -- | 'voiceProfileDomainId'
  Prelude.Text ->
  StartSpeakerSearchTask
newStartSpeakerSearchTask
  pVoiceConnectorId_
  pTransactionId_
  pVoiceProfileDomainId_ =
    StartSpeakerSearchTask'
      { callLeg = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        voiceConnectorId = pVoiceConnectorId_,
        transactionId = pTransactionId_,
        voiceProfileDomainId = pVoiceProfileDomainId_
      }

-- | Specifies which call leg to stream for speaker search.
startSpeakerSearchTask_callLeg :: Lens.Lens' StartSpeakerSearchTask (Prelude.Maybe CallLegType)
startSpeakerSearchTask_callLeg = Lens.lens (\StartSpeakerSearchTask' {callLeg} -> callLeg) (\s@StartSpeakerSearchTask' {} a -> s {callLeg = a} :: StartSpeakerSearchTask)

-- | The unique identifier for the client request. Use a different token for
-- different speaker search tasks.
startSpeakerSearchTask_clientRequestToken :: Lens.Lens' StartSpeakerSearchTask (Prelude.Maybe Prelude.Text)
startSpeakerSearchTask_clientRequestToken = Lens.lens (\StartSpeakerSearchTask' {clientRequestToken} -> clientRequestToken) (\s@StartSpeakerSearchTask' {} a -> s {clientRequestToken = a} :: StartSpeakerSearchTask)

-- | The Voice Connector ID.
startSpeakerSearchTask_voiceConnectorId :: Lens.Lens' StartSpeakerSearchTask Prelude.Text
startSpeakerSearchTask_voiceConnectorId = Lens.lens (\StartSpeakerSearchTask' {voiceConnectorId} -> voiceConnectorId) (\s@StartSpeakerSearchTask' {} a -> s {voiceConnectorId = a} :: StartSpeakerSearchTask)

-- | The transaction ID of the call being analyzed.
startSpeakerSearchTask_transactionId :: Lens.Lens' StartSpeakerSearchTask Prelude.Text
startSpeakerSearchTask_transactionId = Lens.lens (\StartSpeakerSearchTask' {transactionId} -> transactionId) (\s@StartSpeakerSearchTask' {} a -> s {transactionId = a} :: StartSpeakerSearchTask)

-- | The ID of the voice profile domain that will store the voice profile.
startSpeakerSearchTask_voiceProfileDomainId :: Lens.Lens' StartSpeakerSearchTask Prelude.Text
startSpeakerSearchTask_voiceProfileDomainId = Lens.lens (\StartSpeakerSearchTask' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@StartSpeakerSearchTask' {} a -> s {voiceProfileDomainId = a} :: StartSpeakerSearchTask)

instance Core.AWSRequest StartSpeakerSearchTask where
  type
    AWSResponse StartSpeakerSearchTask =
      StartSpeakerSearchTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSpeakerSearchTaskResponse'
            Prelude.<$> (x Data..?> "SpeakerSearchTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartSpeakerSearchTask where
  hashWithSalt _salt StartSpeakerSearchTask' {..} =
    _salt
      `Prelude.hashWithSalt` callLeg
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` voiceProfileDomainId

instance Prelude.NFData StartSpeakerSearchTask where
  rnf StartSpeakerSearchTask' {..} =
    Prelude.rnf callLeg
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf voiceProfileDomainId

instance Data.ToHeaders StartSpeakerSearchTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartSpeakerSearchTask where
  toJSON StartSpeakerSearchTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CallLeg" Data..=) Prelude.<$> callLeg,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("TransactionId" Data..= transactionId),
            Prelude.Just
              ( "VoiceProfileDomainId"
                  Data..= voiceProfileDomainId
              )
          ]
      )

instance Data.ToPath StartSpeakerSearchTask where
  toPath StartSpeakerSearchTask' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/speaker-search-tasks"
      ]

instance Data.ToQuery StartSpeakerSearchTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSpeakerSearchTaskResponse' smart constructor.
data StartSpeakerSearchTaskResponse = StartSpeakerSearchTaskResponse'
  { -- | The details of the speaker search task.
    speakerSearchTask :: Prelude.Maybe SpeakerSearchTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSpeakerSearchTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speakerSearchTask', 'startSpeakerSearchTaskResponse_speakerSearchTask' - The details of the speaker search task.
--
-- 'httpStatus', 'startSpeakerSearchTaskResponse_httpStatus' - The response's http status code.
newStartSpeakerSearchTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSpeakerSearchTaskResponse
newStartSpeakerSearchTaskResponse pHttpStatus_ =
  StartSpeakerSearchTaskResponse'
    { speakerSearchTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the speaker search task.
startSpeakerSearchTaskResponse_speakerSearchTask :: Lens.Lens' StartSpeakerSearchTaskResponse (Prelude.Maybe SpeakerSearchTask)
startSpeakerSearchTaskResponse_speakerSearchTask = Lens.lens (\StartSpeakerSearchTaskResponse' {speakerSearchTask} -> speakerSearchTask) (\s@StartSpeakerSearchTaskResponse' {} a -> s {speakerSearchTask = a} :: StartSpeakerSearchTaskResponse)

-- | The response's http status code.
startSpeakerSearchTaskResponse_httpStatus :: Lens.Lens' StartSpeakerSearchTaskResponse Prelude.Int
startSpeakerSearchTaskResponse_httpStatus = Lens.lens (\StartSpeakerSearchTaskResponse' {httpStatus} -> httpStatus) (\s@StartSpeakerSearchTaskResponse' {} a -> s {httpStatus = a} :: StartSpeakerSearchTaskResponse)

instance
  Prelude.NFData
    StartSpeakerSearchTaskResponse
  where
  rnf StartSpeakerSearchTaskResponse' {..} =
    Prelude.rnf speakerSearchTask
      `Prelude.seq` Prelude.rnf httpStatus
