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
-- Module      : Amazonka.GroundStation.ListContacts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of contacts.
--
-- If @statusList@ contains AVAILABLE, the request must include
-- @groundStation@, @missionprofileArn@, and @satelliteArn@.
--
-- This operation returns paginated results.
module Amazonka.GroundStation.ListContacts
  ( -- * Creating a Request
    ListContacts (..),
    newListContacts,

    -- * Request Lenses
    listContacts_missionProfileArn,
    listContacts_nextToken,
    listContacts_satelliteArn,
    listContacts_maxResults,
    listContacts_groundStation,
    listContacts_endTime,
    listContacts_startTime,
    listContacts_statusList,

    -- * Destructuring the Response
    ListContactsResponse (..),
    newListContactsResponse,

    -- * Response Lenses
    listContactsResponse_nextToken,
    listContactsResponse_contactList,
    listContactsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListContacts' smart constructor.
data ListContacts = ListContacts'
  { -- | ARN of a mission profile.
    missionProfileArn :: Prelude.Maybe Prelude.Text,
    -- | Next token returned in the request of a previous @ListContacts@ call.
    -- Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | ARN of a satellite.
    satelliteArn :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of contacts returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Name of a ground station.
    groundStation :: Prelude.Maybe Prelude.Text,
    -- | End time of a contact in UTC.
    endTime :: Core.POSIX,
    -- | Start time of a contact in UTC.
    startTime :: Core.POSIX,
    -- | Status of a contact reservation.
    statusList :: [ContactStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missionProfileArn', 'listContacts_missionProfileArn' - ARN of a mission profile.
--
-- 'nextToken', 'listContacts_nextToken' - Next token returned in the request of a previous @ListContacts@ call.
-- Used to get the next page of results.
--
-- 'satelliteArn', 'listContacts_satelliteArn' - ARN of a satellite.
--
-- 'maxResults', 'listContacts_maxResults' - Maximum number of contacts returned.
--
-- 'groundStation', 'listContacts_groundStation' - Name of a ground station.
--
-- 'endTime', 'listContacts_endTime' - End time of a contact in UTC.
--
-- 'startTime', 'listContacts_startTime' - Start time of a contact in UTC.
--
-- 'statusList', 'listContacts_statusList' - Status of a contact reservation.
newListContacts ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'startTime'
  Prelude.UTCTime ->
  ListContacts
newListContacts pEndTime_ pStartTime_ =
  ListContacts'
    { missionProfileArn = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      satelliteArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      groundStation = Prelude.Nothing,
      endTime = Core._Time Lens.# pEndTime_,
      startTime = Core._Time Lens.# pStartTime_,
      statusList = Prelude.mempty
    }

-- | ARN of a mission profile.
listContacts_missionProfileArn :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Text)
listContacts_missionProfileArn = Lens.lens (\ListContacts' {missionProfileArn} -> missionProfileArn) (\s@ListContacts' {} a -> s {missionProfileArn = a} :: ListContacts)

-- | Next token returned in the request of a previous @ListContacts@ call.
-- Used to get the next page of results.
listContacts_nextToken :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Text)
listContacts_nextToken = Lens.lens (\ListContacts' {nextToken} -> nextToken) (\s@ListContacts' {} a -> s {nextToken = a} :: ListContacts)

-- | ARN of a satellite.
listContacts_satelliteArn :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Text)
listContacts_satelliteArn = Lens.lens (\ListContacts' {satelliteArn} -> satelliteArn) (\s@ListContacts' {} a -> s {satelliteArn = a} :: ListContacts)

-- | Maximum number of contacts returned.
listContacts_maxResults :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Natural)
listContacts_maxResults = Lens.lens (\ListContacts' {maxResults} -> maxResults) (\s@ListContacts' {} a -> s {maxResults = a} :: ListContacts)

-- | Name of a ground station.
listContacts_groundStation :: Lens.Lens' ListContacts (Prelude.Maybe Prelude.Text)
listContacts_groundStation = Lens.lens (\ListContacts' {groundStation} -> groundStation) (\s@ListContacts' {} a -> s {groundStation = a} :: ListContacts)

-- | End time of a contact in UTC.
listContacts_endTime :: Lens.Lens' ListContacts Prelude.UTCTime
listContacts_endTime = Lens.lens (\ListContacts' {endTime} -> endTime) (\s@ListContacts' {} a -> s {endTime = a} :: ListContacts) Prelude.. Core._Time

-- | Start time of a contact in UTC.
listContacts_startTime :: Lens.Lens' ListContacts Prelude.UTCTime
listContacts_startTime = Lens.lens (\ListContacts' {startTime} -> startTime) (\s@ListContacts' {} a -> s {startTime = a} :: ListContacts) Prelude.. Core._Time

-- | Status of a contact reservation.
listContacts_statusList :: Lens.Lens' ListContacts [ContactStatus]
listContacts_statusList = Lens.lens (\ListContacts' {statusList} -> statusList) (\s@ListContacts' {} a -> s {statusList = a} :: ListContacts) Prelude.. Lens.coerced

instance Core.AWSPager ListContacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listContactsResponse_contactList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listContacts_nextToken
          Lens..~ rs
          Lens.^? listContactsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListContacts where
  type AWSResponse ListContacts = ListContactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "contactList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContacts where
  hashWithSalt _salt ListContacts' {..} =
    _salt `Prelude.hashWithSalt` missionProfileArn
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` satelliteArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` groundStation
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statusList

instance Prelude.NFData ListContacts where
  rnf ListContacts' {..} =
    Prelude.rnf missionProfileArn
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf satelliteArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf groundStation
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf statusList

instance Core.ToHeaders ListContacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListContacts where
  toJSON ListContacts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("missionProfileArn" Core..=)
              Prelude.<$> missionProfileArn,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("satelliteArn" Core..=) Prelude.<$> satelliteArn,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("groundStation" Core..=) Prelude.<$> groundStation,
            Prelude.Just ("endTime" Core..= endTime),
            Prelude.Just ("startTime" Core..= startTime),
            Prelude.Just ("statusList" Core..= statusList)
          ]
      )

instance Core.ToPath ListContacts where
  toPath = Prelude.const "/contacts"

instance Core.ToQuery ListContacts where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newListContactsResponse' smart constructor.
data ListContactsResponse = ListContactsResponse'
  { -- | Next token returned in the response of a previous @ListContacts@ call.
    -- Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of contacts.
    contactList :: Prelude.Maybe [ContactData],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactsResponse_nextToken' - Next token returned in the response of a previous @ListContacts@ call.
-- Used to get the next page of results.
--
-- 'contactList', 'listContactsResponse_contactList' - List of contacts.
--
-- 'httpStatus', 'listContactsResponse_httpStatus' - The response's http status code.
newListContactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactsResponse
newListContactsResponse pHttpStatus_ =
  ListContactsResponse'
    { nextToken = Prelude.Nothing,
      contactList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Next token returned in the response of a previous @ListContacts@ call.
-- Used to get the next page of results.
listContactsResponse_nextToken :: Lens.Lens' ListContactsResponse (Prelude.Maybe Prelude.Text)
listContactsResponse_nextToken = Lens.lens (\ListContactsResponse' {nextToken} -> nextToken) (\s@ListContactsResponse' {} a -> s {nextToken = a} :: ListContactsResponse)

-- | List of contacts.
listContactsResponse_contactList :: Lens.Lens' ListContactsResponse (Prelude.Maybe [ContactData])
listContactsResponse_contactList = Lens.lens (\ListContactsResponse' {contactList} -> contactList) (\s@ListContactsResponse' {} a -> s {contactList = a} :: ListContactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listContactsResponse_httpStatus :: Lens.Lens' ListContactsResponse Prelude.Int
listContactsResponse_httpStatus = Lens.lens (\ListContactsResponse' {httpStatus} -> httpStatus) (\s@ListContactsResponse' {} a -> s {httpStatus = a} :: ListContactsResponse)

instance Prelude.NFData ListContactsResponse where
  rnf ListContactsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf contactList
      `Prelude.seq` Prelude.rnf httpStatus
