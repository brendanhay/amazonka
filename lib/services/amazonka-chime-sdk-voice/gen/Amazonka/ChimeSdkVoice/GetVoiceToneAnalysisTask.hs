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
-- Module      : Amazonka.ChimeSdkVoice.GetVoiceToneAnalysisTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a voice tone analysis task.
module Amazonka.ChimeSdkVoice.GetVoiceToneAnalysisTask
  ( -- * Creating a Request
    GetVoiceToneAnalysisTask (..),
    newGetVoiceToneAnalysisTask,

    -- * Request Lenses
    getVoiceToneAnalysisTask_voiceConnectorId,
    getVoiceToneAnalysisTask_voiceToneAnalysisTaskId,
    getVoiceToneAnalysisTask_isCaller,

    -- * Destructuring the Response
    GetVoiceToneAnalysisTaskResponse (..),
    newGetVoiceToneAnalysisTaskResponse,

    -- * Response Lenses
    getVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask,
    getVoiceToneAnalysisTaskResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceToneAnalysisTask' smart constructor.
data GetVoiceToneAnalysisTask = GetVoiceToneAnalysisTask'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The ID of the voice tone anlysis task.
    voiceToneAnalysisTaskId :: Prelude.Text,
    -- | Specifies whether the voice being analyzed is the caller (originator) or
    -- the callee (responder).
    isCaller :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceToneAnalysisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getVoiceToneAnalysisTask_voiceConnectorId' - The Voice Connector ID.
--
-- 'voiceToneAnalysisTaskId', 'getVoiceToneAnalysisTask_voiceToneAnalysisTaskId' - The ID of the voice tone anlysis task.
--
-- 'isCaller', 'getVoiceToneAnalysisTask_isCaller' - Specifies whether the voice being analyzed is the caller (originator) or
-- the callee (responder).
newGetVoiceToneAnalysisTask ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'voiceToneAnalysisTaskId'
  Prelude.Text ->
  -- | 'isCaller'
  Prelude.Bool ->
  GetVoiceToneAnalysisTask
newGetVoiceToneAnalysisTask
  pVoiceConnectorId_
  pVoiceToneAnalysisTaskId_
  pIsCaller_ =
    GetVoiceToneAnalysisTask'
      { voiceConnectorId =
          pVoiceConnectorId_,
        voiceToneAnalysisTaskId =
          pVoiceToneAnalysisTaskId_,
        isCaller = pIsCaller_
      }

-- | The Voice Connector ID.
getVoiceToneAnalysisTask_voiceConnectorId :: Lens.Lens' GetVoiceToneAnalysisTask Prelude.Text
getVoiceToneAnalysisTask_voiceConnectorId = Lens.lens (\GetVoiceToneAnalysisTask' {voiceConnectorId} -> voiceConnectorId) (\s@GetVoiceToneAnalysisTask' {} a -> s {voiceConnectorId = a} :: GetVoiceToneAnalysisTask)

-- | The ID of the voice tone anlysis task.
getVoiceToneAnalysisTask_voiceToneAnalysisTaskId :: Lens.Lens' GetVoiceToneAnalysisTask Prelude.Text
getVoiceToneAnalysisTask_voiceToneAnalysisTaskId = Lens.lens (\GetVoiceToneAnalysisTask' {voiceToneAnalysisTaskId} -> voiceToneAnalysisTaskId) (\s@GetVoiceToneAnalysisTask' {} a -> s {voiceToneAnalysisTaskId = a} :: GetVoiceToneAnalysisTask)

-- | Specifies whether the voice being analyzed is the caller (originator) or
-- the callee (responder).
getVoiceToneAnalysisTask_isCaller :: Lens.Lens' GetVoiceToneAnalysisTask Prelude.Bool
getVoiceToneAnalysisTask_isCaller = Lens.lens (\GetVoiceToneAnalysisTask' {isCaller} -> isCaller) (\s@GetVoiceToneAnalysisTask' {} a -> s {isCaller = a} :: GetVoiceToneAnalysisTask)

instance Core.AWSRequest GetVoiceToneAnalysisTask where
  type
    AWSResponse GetVoiceToneAnalysisTask =
      GetVoiceToneAnalysisTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceToneAnalysisTaskResponse'
            Prelude.<$> (x Data..?> "VoiceToneAnalysisTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVoiceToneAnalysisTask where
  hashWithSalt _salt GetVoiceToneAnalysisTask' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` voiceToneAnalysisTaskId
      `Prelude.hashWithSalt` isCaller

instance Prelude.NFData GetVoiceToneAnalysisTask where
  rnf GetVoiceToneAnalysisTask' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf voiceToneAnalysisTaskId
      `Prelude.seq` Prelude.rnf isCaller

instance Data.ToHeaders GetVoiceToneAnalysisTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVoiceToneAnalysisTask where
  toPath GetVoiceToneAnalysisTask' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/voice-tone-analysis-tasks/",
        Data.toBS voiceToneAnalysisTaskId
      ]

instance Data.ToQuery GetVoiceToneAnalysisTask where
  toQuery GetVoiceToneAnalysisTask' {..} =
    Prelude.mconcat ["isCaller" Data.=: isCaller]

-- | /See:/ 'newGetVoiceToneAnalysisTaskResponse' smart constructor.
data GetVoiceToneAnalysisTaskResponse = GetVoiceToneAnalysisTaskResponse'
  { -- | The details of the voice tone analysis task.
    voiceToneAnalysisTask :: Prelude.Maybe VoiceToneAnalysisTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceToneAnalysisTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceToneAnalysisTask', 'getVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask' - The details of the voice tone analysis task.
--
-- 'httpStatus', 'getVoiceToneAnalysisTaskResponse_httpStatus' - The response's http status code.
newGetVoiceToneAnalysisTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceToneAnalysisTaskResponse
newGetVoiceToneAnalysisTaskResponse pHttpStatus_ =
  GetVoiceToneAnalysisTaskResponse'
    { voiceToneAnalysisTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the voice tone analysis task.
getVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask :: Lens.Lens' GetVoiceToneAnalysisTaskResponse (Prelude.Maybe VoiceToneAnalysisTask)
getVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask = Lens.lens (\GetVoiceToneAnalysisTaskResponse' {voiceToneAnalysisTask} -> voiceToneAnalysisTask) (\s@GetVoiceToneAnalysisTaskResponse' {} a -> s {voiceToneAnalysisTask = a} :: GetVoiceToneAnalysisTaskResponse)

-- | The response's http status code.
getVoiceToneAnalysisTaskResponse_httpStatus :: Lens.Lens' GetVoiceToneAnalysisTaskResponse Prelude.Int
getVoiceToneAnalysisTaskResponse_httpStatus = Lens.lens (\GetVoiceToneAnalysisTaskResponse' {httpStatus} -> httpStatus) (\s@GetVoiceToneAnalysisTaskResponse' {} a -> s {httpStatus = a} :: GetVoiceToneAnalysisTaskResponse)

instance
  Prelude.NFData
    GetVoiceToneAnalysisTaskResponse
  where
  rnf GetVoiceToneAnalysisTaskResponse' {..} =
    Prelude.rnf voiceToneAnalysisTask
      `Prelude.seq` Prelude.rnf httpStatus
