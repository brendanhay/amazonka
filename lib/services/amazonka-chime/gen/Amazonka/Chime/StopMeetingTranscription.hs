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
-- Module      : Amazonka.Chime.StopMeetingTranscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops transcription for the specified @meetingId@.
module Amazonka.Chime.StopMeetingTranscription
  ( -- * Creating a Request
    StopMeetingTranscription (..),
    newStopMeetingTranscription,

    -- * Request Lenses
    stopMeetingTranscription_meetingId,

    -- * Destructuring the Response
    StopMeetingTranscriptionResponse (..),
    newStopMeetingTranscriptionResponse,

    -- * Response Lenses
    stopMeetingTranscriptionResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopMeetingTranscription' smart constructor.
data StopMeetingTranscription = StopMeetingTranscription'
  { -- | The unique ID of the meeting for which you stop transcription.
    meetingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopMeetingTranscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'stopMeetingTranscription_meetingId' - The unique ID of the meeting for which you stop transcription.
newStopMeetingTranscription ::
  -- | 'meetingId'
  Prelude.Text ->
  StopMeetingTranscription
newStopMeetingTranscription pMeetingId_ =
  StopMeetingTranscription' {meetingId = pMeetingId_}

-- | The unique ID of the meeting for which you stop transcription.
stopMeetingTranscription_meetingId :: Lens.Lens' StopMeetingTranscription Prelude.Text
stopMeetingTranscription_meetingId = Lens.lens (\StopMeetingTranscription' {meetingId} -> meetingId) (\s@StopMeetingTranscription' {} a -> s {meetingId = a} :: StopMeetingTranscription)

instance Core.AWSRequest StopMeetingTranscription where
  type
    AWSResponse StopMeetingTranscription =
      StopMeetingTranscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopMeetingTranscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopMeetingTranscription where
  hashWithSalt _salt StopMeetingTranscription' {..} =
    _salt `Prelude.hashWithSalt` meetingId

instance Prelude.NFData StopMeetingTranscription where
  rnf StopMeetingTranscription' {..} =
    Prelude.rnf meetingId

instance Data.ToHeaders StopMeetingTranscription where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StopMeetingTranscription where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopMeetingTranscription where
  toPath StopMeetingTranscription' {..} =
    Prelude.mconcat
      ["/meetings/", Data.toBS meetingId, "/transcription"]

instance Data.ToQuery StopMeetingTranscription where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=stop"])

-- | /See:/ 'newStopMeetingTranscriptionResponse' smart constructor.
data StopMeetingTranscriptionResponse = StopMeetingTranscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopMeetingTranscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopMeetingTranscriptionResponse_httpStatus' - The response's http status code.
newStopMeetingTranscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopMeetingTranscriptionResponse
newStopMeetingTranscriptionResponse pHttpStatus_ =
  StopMeetingTranscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopMeetingTranscriptionResponse_httpStatus :: Lens.Lens' StopMeetingTranscriptionResponse Prelude.Int
stopMeetingTranscriptionResponse_httpStatus = Lens.lens (\StopMeetingTranscriptionResponse' {httpStatus} -> httpStatus) (\s@StopMeetingTranscriptionResponse' {} a -> s {httpStatus = a} :: StopMeetingTranscriptionResponse)

instance
  Prelude.NFData
    StopMeetingTranscriptionResponse
  where
  rnf StopMeetingTranscriptionResponse' {..} =
    Prelude.rnf httpStatus
