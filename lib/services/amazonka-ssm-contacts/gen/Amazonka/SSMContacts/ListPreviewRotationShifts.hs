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
-- Module      : Amazonka.SSMContacts.ListPreviewRotationShifts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of shifts based on rotation configuration parameters.
--
-- The Incident Manager primarily uses this operation to populate the
-- __Preview__ calendar. It is not typically run by end users.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListPreviewRotationShifts
  ( -- * Creating a Request
    ListPreviewRotationShifts (..),
    newListPreviewRotationShifts,

    -- * Request Lenses
    listPreviewRotationShifts_maxResults,
    listPreviewRotationShifts_nextToken,
    listPreviewRotationShifts_overrides,
    listPreviewRotationShifts_rotationStartTime,
    listPreviewRotationShifts_startTime,
    listPreviewRotationShifts_endTime,
    listPreviewRotationShifts_members,
    listPreviewRotationShifts_timeZoneId,
    listPreviewRotationShifts_recurrence,

    -- * Destructuring the Response
    ListPreviewRotationShiftsResponse (..),
    newListPreviewRotationShiftsResponse,

    -- * Response Lenses
    listPreviewRotationShiftsResponse_nextToken,
    listPreviewRotationShiftsResponse_rotationShifts,
    listPreviewRotationShiftsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListPreviewRotationShifts' smart constructor.
data ListPreviewRotationShifts = ListPreviewRotationShifts'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that can be specified in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. This token is used to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about changes that would be made in a rotation override.
    overrides :: Prelude.Maybe [PreviewOverride],
    -- | The date and time a rotation would begin. The first shift is calculated
    -- from this date and time.
    rotationStartTime :: Prelude.Maybe Data.POSIX,
    -- | Used to filter the range of calculated shifts before sending the
    -- response back to the user.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time a rotation shift would end.
    endTime :: Data.POSIX,
    -- | The contacts that would be assigned to a rotation.
    members :: Prelude.NonEmpty Prelude.Text,
    -- | The time zone the rotation’s activity would be based on, in Internet
    -- Assigned Numbers Authority (IANA) format. For example:
    -- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\".
    timeZoneId :: Prelude.Text,
    -- | Information about how long a rotation would last before restarting at
    -- the beginning of the shift order.
    recurrence :: RecurrenceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPreviewRotationShifts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPreviewRotationShifts_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that can be specified in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listPreviewRotationShifts_nextToken' - A token to start the list. This token is used to get the next set of
-- results.
--
-- 'overrides', 'listPreviewRotationShifts_overrides' - Information about changes that would be made in a rotation override.
--
-- 'rotationStartTime', 'listPreviewRotationShifts_rotationStartTime' - The date and time a rotation would begin. The first shift is calculated
-- from this date and time.
--
-- 'startTime', 'listPreviewRotationShifts_startTime' - Used to filter the range of calculated shifts before sending the
-- response back to the user.
--
-- 'endTime', 'listPreviewRotationShifts_endTime' - The date and time a rotation shift would end.
--
-- 'members', 'listPreviewRotationShifts_members' - The contacts that would be assigned to a rotation.
--
-- 'timeZoneId', 'listPreviewRotationShifts_timeZoneId' - The time zone the rotation’s activity would be based on, in Internet
-- Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\".
--
-- 'recurrence', 'listPreviewRotationShifts_recurrence' - Information about how long a rotation would last before restarting at
-- the beginning of the shift order.
newListPreviewRotationShifts ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'members'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'timeZoneId'
  Prelude.Text ->
  -- | 'recurrence'
  RecurrenceSettings ->
  ListPreviewRotationShifts
newListPreviewRotationShifts
  pEndTime_
  pMembers_
  pTimeZoneId_
  pRecurrence_ =
    ListPreviewRotationShifts'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        overrides = Prelude.Nothing,
        rotationStartTime = Prelude.Nothing,
        startTime = Prelude.Nothing,
        endTime = Data._Time Lens.# pEndTime_,
        members = Lens.coerced Lens.# pMembers_,
        timeZoneId = pTimeZoneId_,
        recurrence = pRecurrence_
      }

-- | The maximum number of items to return for this call. The call also
-- returns a token that can be specified in a subsequent call to get the
-- next set of results.
listPreviewRotationShifts_maxResults :: Lens.Lens' ListPreviewRotationShifts (Prelude.Maybe Prelude.Natural)
listPreviewRotationShifts_maxResults = Lens.lens (\ListPreviewRotationShifts' {maxResults} -> maxResults) (\s@ListPreviewRotationShifts' {} a -> s {maxResults = a} :: ListPreviewRotationShifts)

-- | A token to start the list. This token is used to get the next set of
-- results.
listPreviewRotationShifts_nextToken :: Lens.Lens' ListPreviewRotationShifts (Prelude.Maybe Prelude.Text)
listPreviewRotationShifts_nextToken = Lens.lens (\ListPreviewRotationShifts' {nextToken} -> nextToken) (\s@ListPreviewRotationShifts' {} a -> s {nextToken = a} :: ListPreviewRotationShifts)

-- | Information about changes that would be made in a rotation override.
listPreviewRotationShifts_overrides :: Lens.Lens' ListPreviewRotationShifts (Prelude.Maybe [PreviewOverride])
listPreviewRotationShifts_overrides = Lens.lens (\ListPreviewRotationShifts' {overrides} -> overrides) (\s@ListPreviewRotationShifts' {} a -> s {overrides = a} :: ListPreviewRotationShifts) Prelude.. Lens.mapping Lens.coerced

-- | The date and time a rotation would begin. The first shift is calculated
-- from this date and time.
listPreviewRotationShifts_rotationStartTime :: Lens.Lens' ListPreviewRotationShifts (Prelude.Maybe Prelude.UTCTime)
listPreviewRotationShifts_rotationStartTime = Lens.lens (\ListPreviewRotationShifts' {rotationStartTime} -> rotationStartTime) (\s@ListPreviewRotationShifts' {} a -> s {rotationStartTime = a} :: ListPreviewRotationShifts) Prelude.. Lens.mapping Data._Time

-- | Used to filter the range of calculated shifts before sending the
-- response back to the user.
listPreviewRotationShifts_startTime :: Lens.Lens' ListPreviewRotationShifts (Prelude.Maybe Prelude.UTCTime)
listPreviewRotationShifts_startTime = Lens.lens (\ListPreviewRotationShifts' {startTime} -> startTime) (\s@ListPreviewRotationShifts' {} a -> s {startTime = a} :: ListPreviewRotationShifts) Prelude.. Lens.mapping Data._Time

-- | The date and time a rotation shift would end.
listPreviewRotationShifts_endTime :: Lens.Lens' ListPreviewRotationShifts Prelude.UTCTime
listPreviewRotationShifts_endTime = Lens.lens (\ListPreviewRotationShifts' {endTime} -> endTime) (\s@ListPreviewRotationShifts' {} a -> s {endTime = a} :: ListPreviewRotationShifts) Prelude.. Data._Time

-- | The contacts that would be assigned to a rotation.
listPreviewRotationShifts_members :: Lens.Lens' ListPreviewRotationShifts (Prelude.NonEmpty Prelude.Text)
listPreviewRotationShifts_members = Lens.lens (\ListPreviewRotationShifts' {members} -> members) (\s@ListPreviewRotationShifts' {} a -> s {members = a} :: ListPreviewRotationShifts) Prelude.. Lens.coerced

-- | The time zone the rotation’s activity would be based on, in Internet
-- Assigned Numbers Authority (IANA) format. For example:
-- \"America\/Los_Angeles\", \"UTC\", or \"Asia\/Seoul\".
listPreviewRotationShifts_timeZoneId :: Lens.Lens' ListPreviewRotationShifts Prelude.Text
listPreviewRotationShifts_timeZoneId = Lens.lens (\ListPreviewRotationShifts' {timeZoneId} -> timeZoneId) (\s@ListPreviewRotationShifts' {} a -> s {timeZoneId = a} :: ListPreviewRotationShifts)

-- | Information about how long a rotation would last before restarting at
-- the beginning of the shift order.
listPreviewRotationShifts_recurrence :: Lens.Lens' ListPreviewRotationShifts RecurrenceSettings
listPreviewRotationShifts_recurrence = Lens.lens (\ListPreviewRotationShifts' {recurrence} -> recurrence) (\s@ListPreviewRotationShifts' {} a -> s {recurrence = a} :: ListPreviewRotationShifts)

instance Core.AWSPager ListPreviewRotationShifts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPreviewRotationShiftsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPreviewRotationShiftsResponse_rotationShifts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPreviewRotationShifts_nextToken
          Lens..~ rs
          Lens.^? listPreviewRotationShiftsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPreviewRotationShifts where
  type
    AWSResponse ListPreviewRotationShifts =
      ListPreviewRotationShiftsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPreviewRotationShiftsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RotationShifts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPreviewRotationShifts where
  hashWithSalt _salt ListPreviewRotationShifts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` rotationStartTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` timeZoneId
      `Prelude.hashWithSalt` recurrence

instance Prelude.NFData ListPreviewRotationShifts where
  rnf ListPreviewRotationShifts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf rotationStartTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf recurrence

instance Data.ToHeaders ListPreviewRotationShifts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.ListPreviewRotationShifts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPreviewRotationShifts where
  toJSON ListPreviewRotationShifts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Overrides" Data..=) Prelude.<$> overrides,
            ("RotationStartTime" Data..=)
              Prelude.<$> rotationStartTime,
            ("StartTime" Data..=) Prelude.<$> startTime,
            Prelude.Just ("EndTime" Data..= endTime),
            Prelude.Just ("Members" Data..= members),
            Prelude.Just ("TimeZoneId" Data..= timeZoneId),
            Prelude.Just ("Recurrence" Data..= recurrence)
          ]
      )

instance Data.ToPath ListPreviewRotationShifts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPreviewRotationShifts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPreviewRotationShiftsResponse' smart constructor.
data ListPreviewRotationShiftsResponse = ListPreviewRotationShiftsResponse'
  { -- | The token for the next set of items to return. This token is used to get
    -- the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details about a rotation shift, including times, types, and contacts.
    rotationShifts :: Prelude.Maybe [RotationShift],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPreviewRotationShiftsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPreviewRotationShiftsResponse_nextToken' - The token for the next set of items to return. This token is used to get
-- the next set of results.
--
-- 'rotationShifts', 'listPreviewRotationShiftsResponse_rotationShifts' - Details about a rotation shift, including times, types, and contacts.
--
-- 'httpStatus', 'listPreviewRotationShiftsResponse_httpStatus' - The response's http status code.
newListPreviewRotationShiftsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPreviewRotationShiftsResponse
newListPreviewRotationShiftsResponse pHttpStatus_ =
  ListPreviewRotationShiftsResponse'
    { nextToken =
        Prelude.Nothing,
      rotationShifts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. This token is used to get
-- the next set of results.
listPreviewRotationShiftsResponse_nextToken :: Lens.Lens' ListPreviewRotationShiftsResponse (Prelude.Maybe Prelude.Text)
listPreviewRotationShiftsResponse_nextToken = Lens.lens (\ListPreviewRotationShiftsResponse' {nextToken} -> nextToken) (\s@ListPreviewRotationShiftsResponse' {} a -> s {nextToken = a} :: ListPreviewRotationShiftsResponse)

-- | Details about a rotation shift, including times, types, and contacts.
listPreviewRotationShiftsResponse_rotationShifts :: Lens.Lens' ListPreviewRotationShiftsResponse (Prelude.Maybe [RotationShift])
listPreviewRotationShiftsResponse_rotationShifts = Lens.lens (\ListPreviewRotationShiftsResponse' {rotationShifts} -> rotationShifts) (\s@ListPreviewRotationShiftsResponse' {} a -> s {rotationShifts = a} :: ListPreviewRotationShiftsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPreviewRotationShiftsResponse_httpStatus :: Lens.Lens' ListPreviewRotationShiftsResponse Prelude.Int
listPreviewRotationShiftsResponse_httpStatus = Lens.lens (\ListPreviewRotationShiftsResponse' {httpStatus} -> httpStatus) (\s@ListPreviewRotationShiftsResponse' {} a -> s {httpStatus = a} :: ListPreviewRotationShiftsResponse)

instance
  Prelude.NFData
    ListPreviewRotationShiftsResponse
  where
  rnf ListPreviewRotationShiftsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rotationShifts
      `Prelude.seq` Prelude.rnf httpStatus
