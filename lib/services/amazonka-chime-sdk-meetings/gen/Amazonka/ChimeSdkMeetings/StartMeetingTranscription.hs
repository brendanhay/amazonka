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
-- Module      : Amazonka.ChimeSdkMeetings.StartMeetingTranscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts transcription for the specified @meetingId@.
module Amazonka.ChimeSdkMeetings.StartMeetingTranscription
  ( -- * Creating a Request
    StartMeetingTranscription (..),
    newStartMeetingTranscription,

    -- * Request Lenses
    startMeetingTranscription_meetingId,
    startMeetingTranscription_transcriptionConfiguration,

    -- * Destructuring the Response
    StartMeetingTranscriptionResponse (..),
    newStartMeetingTranscriptionResponse,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartMeetingTranscription' smart constructor.
data StartMeetingTranscription = StartMeetingTranscription'
  { -- | The unique ID of the meeting being transcribed.
    meetingId :: Prelude.Text,
    -- | The configuration for the current transcription operation. Must contain
    -- @EngineTranscribeSettings@ or @EngineTranscribeMedicalSettings@.
    transcriptionConfiguration :: TranscriptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMeetingTranscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'startMeetingTranscription_meetingId' - The unique ID of the meeting being transcribed.
--
-- 'transcriptionConfiguration', 'startMeetingTranscription_transcriptionConfiguration' - The configuration for the current transcription operation. Must contain
-- @EngineTranscribeSettings@ or @EngineTranscribeMedicalSettings@.
newStartMeetingTranscription ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'transcriptionConfiguration'
  TranscriptionConfiguration ->
  StartMeetingTranscription
newStartMeetingTranscription
  pMeetingId_
  pTranscriptionConfiguration_ =
    StartMeetingTranscription'
      { meetingId = pMeetingId_,
        transcriptionConfiguration =
          pTranscriptionConfiguration_
      }

-- | The unique ID of the meeting being transcribed.
startMeetingTranscription_meetingId :: Lens.Lens' StartMeetingTranscription Prelude.Text
startMeetingTranscription_meetingId = Lens.lens (\StartMeetingTranscription' {meetingId} -> meetingId) (\s@StartMeetingTranscription' {} a -> s {meetingId = a} :: StartMeetingTranscription)

-- | The configuration for the current transcription operation. Must contain
-- @EngineTranscribeSettings@ or @EngineTranscribeMedicalSettings@.
startMeetingTranscription_transcriptionConfiguration :: Lens.Lens' StartMeetingTranscription TranscriptionConfiguration
startMeetingTranscription_transcriptionConfiguration = Lens.lens (\StartMeetingTranscription' {transcriptionConfiguration} -> transcriptionConfiguration) (\s@StartMeetingTranscription' {} a -> s {transcriptionConfiguration = a} :: StartMeetingTranscription)

instance Core.AWSRequest StartMeetingTranscription where
  type
    AWSResponse StartMeetingTranscription =
      StartMeetingTranscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StartMeetingTranscriptionResponse'

instance Prelude.Hashable StartMeetingTranscription where
  hashWithSalt _salt StartMeetingTranscription' {..} =
    _salt
      `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` transcriptionConfiguration

instance Prelude.NFData StartMeetingTranscription where
  rnf StartMeetingTranscription' {..} =
    Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf transcriptionConfiguration

instance Data.ToHeaders StartMeetingTranscription where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartMeetingTranscription where
  toJSON StartMeetingTranscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TranscriptionConfiguration"
                  Data..= transcriptionConfiguration
              )
          ]
      )

instance Data.ToPath StartMeetingTranscription where
  toPath StartMeetingTranscription' {..} =
    Prelude.mconcat
      ["/meetings/", Data.toBS meetingId, "/transcription"]

instance Data.ToQuery StartMeetingTranscription where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=start"])

-- | /See:/ 'newStartMeetingTranscriptionResponse' smart constructor.
data StartMeetingTranscriptionResponse = StartMeetingTranscriptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMeetingTranscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartMeetingTranscriptionResponse ::
  StartMeetingTranscriptionResponse
newStartMeetingTranscriptionResponse =
  StartMeetingTranscriptionResponse'

instance
  Prelude.NFData
    StartMeetingTranscriptionResponse
  where
  rnf _ = ()
