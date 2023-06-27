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
-- Module      : Amazonka.ChimeSdkVoice.GetSpeakerSearchTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of the specified speaker search task.
module Amazonka.ChimeSdkVoice.GetSpeakerSearchTask
  ( -- * Creating a Request
    GetSpeakerSearchTask (..),
    newGetSpeakerSearchTask,

    -- * Request Lenses
    getSpeakerSearchTask_voiceConnectorId,
    getSpeakerSearchTask_speakerSearchTaskId,

    -- * Destructuring the Response
    GetSpeakerSearchTaskResponse (..),
    newGetSpeakerSearchTaskResponse,

    -- * Response Lenses
    getSpeakerSearchTaskResponse_speakerSearchTask,
    getSpeakerSearchTaskResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSpeakerSearchTask' smart constructor.
data GetSpeakerSearchTask = GetSpeakerSearchTask'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The ID of the speaker search task.
    speakerSearchTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSpeakerSearchTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getSpeakerSearchTask_voiceConnectorId' - The Voice Connector ID.
--
-- 'speakerSearchTaskId', 'getSpeakerSearchTask_speakerSearchTaskId' - The ID of the speaker search task.
newGetSpeakerSearchTask ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'speakerSearchTaskId'
  Prelude.Text ->
  GetSpeakerSearchTask
newGetSpeakerSearchTask
  pVoiceConnectorId_
  pSpeakerSearchTaskId_ =
    GetSpeakerSearchTask'
      { voiceConnectorId =
          pVoiceConnectorId_,
        speakerSearchTaskId = pSpeakerSearchTaskId_
      }

-- | The Voice Connector ID.
getSpeakerSearchTask_voiceConnectorId :: Lens.Lens' GetSpeakerSearchTask Prelude.Text
getSpeakerSearchTask_voiceConnectorId = Lens.lens (\GetSpeakerSearchTask' {voiceConnectorId} -> voiceConnectorId) (\s@GetSpeakerSearchTask' {} a -> s {voiceConnectorId = a} :: GetSpeakerSearchTask)

-- | The ID of the speaker search task.
getSpeakerSearchTask_speakerSearchTaskId :: Lens.Lens' GetSpeakerSearchTask Prelude.Text
getSpeakerSearchTask_speakerSearchTaskId = Lens.lens (\GetSpeakerSearchTask' {speakerSearchTaskId} -> speakerSearchTaskId) (\s@GetSpeakerSearchTask' {} a -> s {speakerSearchTaskId = a} :: GetSpeakerSearchTask)

instance Core.AWSRequest GetSpeakerSearchTask where
  type
    AWSResponse GetSpeakerSearchTask =
      GetSpeakerSearchTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSpeakerSearchTaskResponse'
            Prelude.<$> (x Data..?> "SpeakerSearchTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSpeakerSearchTask where
  hashWithSalt _salt GetSpeakerSearchTask' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` speakerSearchTaskId

instance Prelude.NFData GetSpeakerSearchTask where
  rnf GetSpeakerSearchTask' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf speakerSearchTaskId

instance Data.ToHeaders GetSpeakerSearchTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSpeakerSearchTask where
  toPath GetSpeakerSearchTask' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/speaker-search-tasks/",
        Data.toBS speakerSearchTaskId
      ]

instance Data.ToQuery GetSpeakerSearchTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSpeakerSearchTaskResponse' smart constructor.
data GetSpeakerSearchTaskResponse = GetSpeakerSearchTaskResponse'
  { -- | The details of the speaker search task.
    speakerSearchTask :: Prelude.Maybe SpeakerSearchTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSpeakerSearchTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speakerSearchTask', 'getSpeakerSearchTaskResponse_speakerSearchTask' - The details of the speaker search task.
--
-- 'httpStatus', 'getSpeakerSearchTaskResponse_httpStatus' - The response's http status code.
newGetSpeakerSearchTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSpeakerSearchTaskResponse
newGetSpeakerSearchTaskResponse pHttpStatus_ =
  GetSpeakerSearchTaskResponse'
    { speakerSearchTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the speaker search task.
getSpeakerSearchTaskResponse_speakerSearchTask :: Lens.Lens' GetSpeakerSearchTaskResponse (Prelude.Maybe SpeakerSearchTask)
getSpeakerSearchTaskResponse_speakerSearchTask = Lens.lens (\GetSpeakerSearchTaskResponse' {speakerSearchTask} -> speakerSearchTask) (\s@GetSpeakerSearchTaskResponse' {} a -> s {speakerSearchTask = a} :: GetSpeakerSearchTaskResponse)

-- | The response's http status code.
getSpeakerSearchTaskResponse_httpStatus :: Lens.Lens' GetSpeakerSearchTaskResponse Prelude.Int
getSpeakerSearchTaskResponse_httpStatus = Lens.lens (\GetSpeakerSearchTaskResponse' {httpStatus} -> httpStatus) (\s@GetSpeakerSearchTaskResponse' {} a -> s {httpStatus = a} :: GetSpeakerSearchTaskResponse)

instance Prelude.NFData GetSpeakerSearchTaskResponse where
  rnf GetSpeakerSearchTaskResponse' {..} =
    Prelude.rnf speakerSearchTask
      `Prelude.seq` Prelude.rnf httpStatus
