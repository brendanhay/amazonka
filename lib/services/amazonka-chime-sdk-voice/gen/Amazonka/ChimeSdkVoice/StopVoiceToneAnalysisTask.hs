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
-- Module      : Amazonka.ChimeSdkVoice.StopVoiceToneAnalysisTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a voice tone analysis task.
module Amazonka.ChimeSdkVoice.StopVoiceToneAnalysisTask
  ( -- * Creating a Request
    StopVoiceToneAnalysisTask (..),
    newStopVoiceToneAnalysisTask,

    -- * Request Lenses
    stopVoiceToneAnalysisTask_voiceConnectorId,
    stopVoiceToneAnalysisTask_voiceToneAnalysisTaskId,

    -- * Destructuring the Response
    StopVoiceToneAnalysisTaskResponse (..),
    newStopVoiceToneAnalysisTaskResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopVoiceToneAnalysisTask' smart constructor.
data StopVoiceToneAnalysisTask = StopVoiceToneAnalysisTask'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The ID of the voice tone analysis task.
    voiceToneAnalysisTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopVoiceToneAnalysisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'stopVoiceToneAnalysisTask_voiceConnectorId' - The Voice Connector ID.
--
-- 'voiceToneAnalysisTaskId', 'stopVoiceToneAnalysisTask_voiceToneAnalysisTaskId' - The ID of the voice tone analysis task.
newStopVoiceToneAnalysisTask ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'voiceToneAnalysisTaskId'
  Prelude.Text ->
  StopVoiceToneAnalysisTask
newStopVoiceToneAnalysisTask
  pVoiceConnectorId_
  pVoiceToneAnalysisTaskId_ =
    StopVoiceToneAnalysisTask'
      { voiceConnectorId =
          pVoiceConnectorId_,
        voiceToneAnalysisTaskId =
          pVoiceToneAnalysisTaskId_
      }

-- | The Voice Connector ID.
stopVoiceToneAnalysisTask_voiceConnectorId :: Lens.Lens' StopVoiceToneAnalysisTask Prelude.Text
stopVoiceToneAnalysisTask_voiceConnectorId = Lens.lens (\StopVoiceToneAnalysisTask' {voiceConnectorId} -> voiceConnectorId) (\s@StopVoiceToneAnalysisTask' {} a -> s {voiceConnectorId = a} :: StopVoiceToneAnalysisTask)

-- | The ID of the voice tone analysis task.
stopVoiceToneAnalysisTask_voiceToneAnalysisTaskId :: Lens.Lens' StopVoiceToneAnalysisTask Prelude.Text
stopVoiceToneAnalysisTask_voiceToneAnalysisTaskId = Lens.lens (\StopVoiceToneAnalysisTask' {voiceToneAnalysisTaskId} -> voiceToneAnalysisTaskId) (\s@StopVoiceToneAnalysisTask' {} a -> s {voiceToneAnalysisTaskId = a} :: StopVoiceToneAnalysisTask)

instance Core.AWSRequest StopVoiceToneAnalysisTask where
  type
    AWSResponse StopVoiceToneAnalysisTask =
      StopVoiceToneAnalysisTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StopVoiceToneAnalysisTaskResponse'

instance Prelude.Hashable StopVoiceToneAnalysisTask where
  hashWithSalt _salt StopVoiceToneAnalysisTask' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` voiceToneAnalysisTaskId

instance Prelude.NFData StopVoiceToneAnalysisTask where
  rnf StopVoiceToneAnalysisTask' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf voiceToneAnalysisTaskId

instance Data.ToHeaders StopVoiceToneAnalysisTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StopVoiceToneAnalysisTask where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopVoiceToneAnalysisTask where
  toPath StopVoiceToneAnalysisTask' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/voice-tone-analysis-tasks/",
        Data.toBS voiceToneAnalysisTaskId
      ]

instance Data.ToQuery StopVoiceToneAnalysisTask where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=stop"])

-- | /See:/ 'newStopVoiceToneAnalysisTaskResponse' smart constructor.
data StopVoiceToneAnalysisTaskResponse = StopVoiceToneAnalysisTaskResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopVoiceToneAnalysisTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopVoiceToneAnalysisTaskResponse ::
  StopVoiceToneAnalysisTaskResponse
newStopVoiceToneAnalysisTaskResponse =
  StopVoiceToneAnalysisTaskResponse'

instance
  Prelude.NFData
    StopVoiceToneAnalysisTaskResponse
  where
  rnf _ = ()
