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
-- Module      : Amazonka.ChimeSdkMeetings.GetMeeting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Amazon Chime SDK meeting details for the specified meeting ID.
-- For more information about the Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/.
module Amazonka.ChimeSdkMeetings.GetMeeting
  ( -- * Creating a Request
    GetMeeting (..),
    newGetMeeting,

    -- * Request Lenses
    getMeeting_meetingId,

    -- * Destructuring the Response
    GetMeetingResponse (..),
    newGetMeetingResponse,

    -- * Response Lenses
    getMeetingResponse_meeting,
    getMeetingResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMeeting' smart constructor.
data GetMeeting = GetMeeting'
  { -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMeeting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'getMeeting_meetingId' - The Amazon Chime SDK meeting ID.
newGetMeeting ::
  -- | 'meetingId'
  Prelude.Text ->
  GetMeeting
newGetMeeting pMeetingId_ =
  GetMeeting' {meetingId = pMeetingId_}

-- | The Amazon Chime SDK meeting ID.
getMeeting_meetingId :: Lens.Lens' GetMeeting Prelude.Text
getMeeting_meetingId = Lens.lens (\GetMeeting' {meetingId} -> meetingId) (\s@GetMeeting' {} a -> s {meetingId = a} :: GetMeeting)

instance Core.AWSRequest GetMeeting where
  type AWSResponse GetMeeting = GetMeetingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMeetingResponse'
            Prelude.<$> (x Core..?> "Meeting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMeeting where
  hashWithSalt _salt GetMeeting' {..} =
    _salt `Prelude.hashWithSalt` meetingId

instance Prelude.NFData GetMeeting where
  rnf GetMeeting' {..} = Prelude.rnf meetingId

instance Core.ToHeaders GetMeeting where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetMeeting where
  toPath GetMeeting' {..} =
    Prelude.mconcat ["/meetings/", Core.toBS meetingId]

instance Core.ToQuery GetMeeting where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMeetingResponse' smart constructor.
data GetMeetingResponse = GetMeetingResponse'
  { -- | The Amazon Chime SDK meeting information.
    meeting :: Prelude.Maybe Meeting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMeetingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meeting', 'getMeetingResponse_meeting' - The Amazon Chime SDK meeting information.
--
-- 'httpStatus', 'getMeetingResponse_httpStatus' - The response's http status code.
newGetMeetingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMeetingResponse
newGetMeetingResponse pHttpStatus_ =
  GetMeetingResponse'
    { meeting = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Chime SDK meeting information.
getMeetingResponse_meeting :: Lens.Lens' GetMeetingResponse (Prelude.Maybe Meeting)
getMeetingResponse_meeting = Lens.lens (\GetMeetingResponse' {meeting} -> meeting) (\s@GetMeetingResponse' {} a -> s {meeting = a} :: GetMeetingResponse)

-- | The response's http status code.
getMeetingResponse_httpStatus :: Lens.Lens' GetMeetingResponse Prelude.Int
getMeetingResponse_httpStatus = Lens.lens (\GetMeetingResponse' {httpStatus} -> httpStatus) (\s@GetMeetingResponse' {} a -> s {httpStatus = a} :: GetMeetingResponse)

instance Prelude.NFData GetMeetingResponse where
  rnf GetMeetingResponse' {..} =
    Prelude.rnf meeting
      `Prelude.seq` Prelude.rnf httpStatus
