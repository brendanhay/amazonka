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
-- Module      : Network.AWS.Chime.ListRooms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the room details for the specified Amazon Chime Enterprise
-- account. Optionally, filter the results by a member ID (user ID or bot
-- ID) to see a list of rooms that the member belongs to.
module Network.AWS.Chime.ListRooms
  ( -- * Creating a Request
    ListRooms (..),
    newListRooms,

    -- * Request Lenses
    listRooms_memberId,
    listRooms_nextToken,
    listRooms_maxResults,
    listRooms_accountId,

    -- * Destructuring the Response
    ListRoomsResponse (..),
    newListRoomsResponse,

    -- * Response Lenses
    listRoomsResponse_rooms,
    listRoomsResponse_nextToken,
    listRoomsResponse_httpStatus,
  )
where

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRooms' smart constructor.
data ListRooms = ListRooms'
  { -- | The member ID (user ID or bot ID).
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRooms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'listRooms_memberId' - The member ID (user ID or bot ID).
--
-- 'nextToken', 'listRooms_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'listRooms_maxResults' - The maximum number of results to return in a single call.
--
-- 'accountId', 'listRooms_accountId' - The Amazon Chime account ID.
newListRooms ::
  -- | 'accountId'
  Prelude.Text ->
  ListRooms
newListRooms pAccountId_ =
  ListRooms'
    { memberId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The member ID (user ID or bot ID).
listRooms_memberId :: Lens.Lens' ListRooms (Prelude.Maybe Prelude.Text)
listRooms_memberId = Lens.lens (\ListRooms' {memberId} -> memberId) (\s@ListRooms' {} a -> s {memberId = a} :: ListRooms)

-- | The token to use to retrieve the next page of results.
listRooms_nextToken :: Lens.Lens' ListRooms (Prelude.Maybe Prelude.Text)
listRooms_nextToken = Lens.lens (\ListRooms' {nextToken} -> nextToken) (\s@ListRooms' {} a -> s {nextToken = a} :: ListRooms)

-- | The maximum number of results to return in a single call.
listRooms_maxResults :: Lens.Lens' ListRooms (Prelude.Maybe Prelude.Natural)
listRooms_maxResults = Lens.lens (\ListRooms' {maxResults} -> maxResults) (\s@ListRooms' {} a -> s {maxResults = a} :: ListRooms)

-- | The Amazon Chime account ID.
listRooms_accountId :: Lens.Lens' ListRooms Prelude.Text
listRooms_accountId = Lens.lens (\ListRooms' {accountId} -> accountId) (\s@ListRooms' {} a -> s {accountId = a} :: ListRooms)

instance Core.AWSRequest ListRooms where
  type AWSResponse ListRooms = ListRoomsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoomsResponse'
            Prelude.<$> (x Core..?> "Rooms" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRooms

instance Prelude.NFData ListRooms

instance Core.ToHeaders ListRooms where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListRooms where
  toPath ListRooms' {..} =
    Prelude.mconcat
      ["/accounts/", Core.toBS accountId, "/rooms"]

instance Core.ToQuery ListRooms where
  toQuery ListRooms' {..} =
    Prelude.mconcat
      [ "member-id" Core.=: memberId,
        "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListRoomsResponse' smart constructor.
data ListRoomsResponse = ListRoomsResponse'
  { -- | The room details.
    rooms :: Prelude.Maybe [Room],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoomsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rooms', 'listRoomsResponse_rooms' - The room details.
--
-- 'nextToken', 'listRoomsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'httpStatus', 'listRoomsResponse_httpStatus' - The response's http status code.
newListRoomsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoomsResponse
newListRoomsResponse pHttpStatus_ =
  ListRoomsResponse'
    { rooms = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The room details.
listRoomsResponse_rooms :: Lens.Lens' ListRoomsResponse (Prelude.Maybe [Room])
listRoomsResponse_rooms = Lens.lens (\ListRoomsResponse' {rooms} -> rooms) (\s@ListRoomsResponse' {} a -> s {rooms = a} :: ListRoomsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results.
listRoomsResponse_nextToken :: Lens.Lens' ListRoomsResponse (Prelude.Maybe Prelude.Text)
listRoomsResponse_nextToken = Lens.lens (\ListRoomsResponse' {nextToken} -> nextToken) (\s@ListRoomsResponse' {} a -> s {nextToken = a} :: ListRoomsResponse)

-- | The response's http status code.
listRoomsResponse_httpStatus :: Lens.Lens' ListRoomsResponse Prelude.Int
listRoomsResponse_httpStatus = Lens.lens (\ListRoomsResponse' {httpStatus} -> httpStatus) (\s@ListRoomsResponse' {} a -> s {httpStatus = a} :: ListRoomsResponse)

instance Prelude.NFData ListRoomsResponse
