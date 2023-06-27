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
-- Module      : Amazonka.ChimeSdkVoice.StopSpeakerSearchTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a speaker search task.
module Amazonka.ChimeSdkVoice.StopSpeakerSearchTask
  ( -- * Creating a Request
    StopSpeakerSearchTask (..),
    newStopSpeakerSearchTask,

    -- * Request Lenses
    stopSpeakerSearchTask_voiceConnectorId,
    stopSpeakerSearchTask_speakerSearchTaskId,

    -- * Destructuring the Response
    StopSpeakerSearchTaskResponse (..),
    newStopSpeakerSearchTaskResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopSpeakerSearchTask' smart constructor.
data StopSpeakerSearchTask = StopSpeakerSearchTask'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The speaker search task ID.
    speakerSearchTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSpeakerSearchTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'stopSpeakerSearchTask_voiceConnectorId' - The Voice Connector ID.
--
-- 'speakerSearchTaskId', 'stopSpeakerSearchTask_speakerSearchTaskId' - The speaker search task ID.
newStopSpeakerSearchTask ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'speakerSearchTaskId'
  Prelude.Text ->
  StopSpeakerSearchTask
newStopSpeakerSearchTask
  pVoiceConnectorId_
  pSpeakerSearchTaskId_ =
    StopSpeakerSearchTask'
      { voiceConnectorId =
          pVoiceConnectorId_,
        speakerSearchTaskId = pSpeakerSearchTaskId_
      }

-- | The Voice Connector ID.
stopSpeakerSearchTask_voiceConnectorId :: Lens.Lens' StopSpeakerSearchTask Prelude.Text
stopSpeakerSearchTask_voiceConnectorId = Lens.lens (\StopSpeakerSearchTask' {voiceConnectorId} -> voiceConnectorId) (\s@StopSpeakerSearchTask' {} a -> s {voiceConnectorId = a} :: StopSpeakerSearchTask)

-- | The speaker search task ID.
stopSpeakerSearchTask_speakerSearchTaskId :: Lens.Lens' StopSpeakerSearchTask Prelude.Text
stopSpeakerSearchTask_speakerSearchTaskId = Lens.lens (\StopSpeakerSearchTask' {speakerSearchTaskId} -> speakerSearchTaskId) (\s@StopSpeakerSearchTask' {} a -> s {speakerSearchTaskId = a} :: StopSpeakerSearchTask)

instance Core.AWSRequest StopSpeakerSearchTask where
  type
    AWSResponse StopSpeakerSearchTask =
      StopSpeakerSearchTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopSpeakerSearchTaskResponse'

instance Prelude.Hashable StopSpeakerSearchTask where
  hashWithSalt _salt StopSpeakerSearchTask' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` speakerSearchTaskId

instance Prelude.NFData StopSpeakerSearchTask where
  rnf StopSpeakerSearchTask' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf speakerSearchTaskId

instance Data.ToHeaders StopSpeakerSearchTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StopSpeakerSearchTask where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopSpeakerSearchTask where
  toPath StopSpeakerSearchTask' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/speaker-search-tasks/",
        Data.toBS speakerSearchTaskId
      ]

instance Data.ToQuery StopSpeakerSearchTask where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=stop"])

-- | /See:/ 'newStopSpeakerSearchTaskResponse' smart constructor.
data StopSpeakerSearchTaskResponse = StopSpeakerSearchTaskResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSpeakerSearchTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopSpeakerSearchTaskResponse ::
  StopSpeakerSearchTaskResponse
newStopSpeakerSearchTaskResponse =
  StopSpeakerSearchTaskResponse'

instance Prelude.NFData StopSpeakerSearchTaskResponse where
  rnf _ = ()
