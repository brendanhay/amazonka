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
-- Module      : Amazonka.ChimeSdkMeetings.ListAttendees
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attendees for the specified Amazon Chime SDK meeting. For more
-- information about the Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/.
module Amazonka.ChimeSdkMeetings.ListAttendees
  ( -- * Creating a Request
    ListAttendees (..),
    newListAttendees,

    -- * Request Lenses
    listAttendees_maxResults,
    listAttendees_nextToken,
    listAttendees_meetingId,

    -- * Destructuring the Response
    ListAttendeesResponse (..),
    newListAttendeesResponse,

    -- * Response Lenses
    listAttendeesResponse_attendees,
    listAttendeesResponse_nextToken,
    listAttendeesResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttendees' smart constructor.
data ListAttendees = ListAttendees'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttendees' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAttendees_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listAttendees_nextToken' - The token to use to retrieve the next page of results.
--
-- 'meetingId', 'listAttendees_meetingId' - The Amazon Chime SDK meeting ID.
newListAttendees ::
  -- | 'meetingId'
  Prelude.Text ->
  ListAttendees
newListAttendees pMeetingId_ =
  ListAttendees'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      meetingId = pMeetingId_
    }

-- | The maximum number of results to return in a single call.
listAttendees_maxResults :: Lens.Lens' ListAttendees (Prelude.Maybe Prelude.Natural)
listAttendees_maxResults = Lens.lens (\ListAttendees' {maxResults} -> maxResults) (\s@ListAttendees' {} a -> s {maxResults = a} :: ListAttendees)

-- | The token to use to retrieve the next page of results.
listAttendees_nextToken :: Lens.Lens' ListAttendees (Prelude.Maybe Prelude.Text)
listAttendees_nextToken = Lens.lens (\ListAttendees' {nextToken} -> nextToken) (\s@ListAttendees' {} a -> s {nextToken = a} :: ListAttendees)

-- | The Amazon Chime SDK meeting ID.
listAttendees_meetingId :: Lens.Lens' ListAttendees Prelude.Text
listAttendees_meetingId = Lens.lens (\ListAttendees' {meetingId} -> meetingId) (\s@ListAttendees' {} a -> s {meetingId = a} :: ListAttendees)

instance Core.AWSRequest ListAttendees where
  type
    AWSResponse ListAttendees =
      ListAttendeesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttendeesResponse'
            Prelude.<$> (x Data..?> "Attendees" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttendees where
  hashWithSalt _salt ListAttendees' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` meetingId

instance Prelude.NFData ListAttendees where
  rnf ListAttendees' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf meetingId

instance Data.ToHeaders ListAttendees where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAttendees where
  toPath ListAttendees' {..} =
    Prelude.mconcat
      ["/meetings/", Data.toBS meetingId, "/attendees"]

instance Data.ToQuery ListAttendees where
  toQuery ListAttendees' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListAttendeesResponse' smart constructor.
data ListAttendeesResponse = ListAttendeesResponse'
  { -- | The Amazon Chime SDK attendee information.
    attendees :: Prelude.Maybe [Attendee],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttendeesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendees', 'listAttendeesResponse_attendees' - The Amazon Chime SDK attendee information.
--
-- 'nextToken', 'listAttendeesResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'httpStatus', 'listAttendeesResponse_httpStatus' - The response's http status code.
newListAttendeesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttendeesResponse
newListAttendeesResponse pHttpStatus_ =
  ListAttendeesResponse'
    { attendees = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Chime SDK attendee information.
listAttendeesResponse_attendees :: Lens.Lens' ListAttendeesResponse (Prelude.Maybe [Attendee])
listAttendeesResponse_attendees = Lens.lens (\ListAttendeesResponse' {attendees} -> attendees) (\s@ListAttendeesResponse' {} a -> s {attendees = a} :: ListAttendeesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results.
listAttendeesResponse_nextToken :: Lens.Lens' ListAttendeesResponse (Prelude.Maybe Prelude.Text)
listAttendeesResponse_nextToken = Lens.lens (\ListAttendeesResponse' {nextToken} -> nextToken) (\s@ListAttendeesResponse' {} a -> s {nextToken = a} :: ListAttendeesResponse)

-- | The response's http status code.
listAttendeesResponse_httpStatus :: Lens.Lens' ListAttendeesResponse Prelude.Int
listAttendeesResponse_httpStatus = Lens.lens (\ListAttendeesResponse' {httpStatus} -> httpStatus) (\s@ListAttendeesResponse' {} a -> s {httpStatus = a} :: ListAttendeesResponse)

instance Prelude.NFData ListAttendeesResponse where
  rnf ListAttendeesResponse' {..} =
    Prelude.rnf attendees
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
