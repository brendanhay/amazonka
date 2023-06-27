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
-- Module      : Amazonka.Chime.ListRooms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the room details for the specified Amazon Chime Enterprise
-- account. Optionally, filter the results by a member ID (user ID or bot
-- ID) to see a list of rooms that the member belongs to.
module Amazonka.Chime.ListRooms
  ( -- * Creating a Request
    ListRooms (..),
    newListRooms,

    -- * Request Lenses
    listRooms_maxResults,
    listRooms_memberId,
    listRooms_nextToken,
    listRooms_accountId,

    -- * Destructuring the Response
    ListRoomsResponse (..),
    newListRoomsResponse,

    -- * Response Lenses
    listRoomsResponse_nextToken,
    listRoomsResponse_rooms,
    listRoomsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRooms' smart constructor.
data ListRooms = ListRooms'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The member ID (user ID or bot ID).
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listRooms_maxResults' - The maximum number of results to return in a single call.
--
-- 'memberId', 'listRooms_memberId' - The member ID (user ID or bot ID).
--
-- 'nextToken', 'listRooms_nextToken' - The token to use to retrieve the next page of results.
--
-- 'accountId', 'listRooms_accountId' - The Amazon Chime account ID.
newListRooms ::
  -- | 'accountId'
  Prelude.Text ->
  ListRooms
newListRooms pAccountId_ =
  ListRooms'
    { maxResults = Prelude.Nothing,
      memberId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The maximum number of results to return in a single call.
listRooms_maxResults :: Lens.Lens' ListRooms (Prelude.Maybe Prelude.Natural)
listRooms_maxResults = Lens.lens (\ListRooms' {maxResults} -> maxResults) (\s@ListRooms' {} a -> s {maxResults = a} :: ListRooms)

-- | The member ID (user ID or bot ID).
listRooms_memberId :: Lens.Lens' ListRooms (Prelude.Maybe Prelude.Text)
listRooms_memberId = Lens.lens (\ListRooms' {memberId} -> memberId) (\s@ListRooms' {} a -> s {memberId = a} :: ListRooms)

-- | The token to use to retrieve the next page of results.
listRooms_nextToken :: Lens.Lens' ListRooms (Prelude.Maybe Prelude.Text)
listRooms_nextToken = Lens.lens (\ListRooms' {nextToken} -> nextToken) (\s@ListRooms' {} a -> s {nextToken = a} :: ListRooms)

-- | The Amazon Chime account ID.
listRooms_accountId :: Lens.Lens' ListRooms Prelude.Text
listRooms_accountId = Lens.lens (\ListRooms' {accountId} -> accountId) (\s@ListRooms' {} a -> s {accountId = a} :: ListRooms)

instance Core.AWSRequest ListRooms where
  type AWSResponse ListRooms = ListRoomsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoomsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Rooms" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRooms where
  hashWithSalt _salt ListRooms' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData ListRooms where
  rnf ListRooms' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToHeaders ListRooms where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListRooms where
  toPath ListRooms' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS accountId, "/rooms"]

instance Data.ToQuery ListRooms where
  toQuery ListRooms' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "member-id" Data.=: memberId,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListRoomsResponse' smart constructor.
data ListRoomsResponse = ListRoomsResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The room details.
    rooms :: Prelude.Maybe [Room],
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
-- 'nextToken', 'listRoomsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'rooms', 'listRoomsResponse_rooms' - The room details.
--
-- 'httpStatus', 'listRoomsResponse_httpStatus' - The response's http status code.
newListRoomsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoomsResponse
newListRoomsResponse pHttpStatus_ =
  ListRoomsResponse'
    { nextToken = Prelude.Nothing,
      rooms = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results.
listRoomsResponse_nextToken :: Lens.Lens' ListRoomsResponse (Prelude.Maybe Prelude.Text)
listRoomsResponse_nextToken = Lens.lens (\ListRoomsResponse' {nextToken} -> nextToken) (\s@ListRoomsResponse' {} a -> s {nextToken = a} :: ListRoomsResponse)

-- | The room details.
listRoomsResponse_rooms :: Lens.Lens' ListRoomsResponse (Prelude.Maybe [Room])
listRoomsResponse_rooms = Lens.lens (\ListRoomsResponse' {rooms} -> rooms) (\s@ListRoomsResponse' {} a -> s {rooms = a} :: ListRoomsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRoomsResponse_httpStatus :: Lens.Lens' ListRoomsResponse Prelude.Int
listRoomsResponse_httpStatus = Lens.lens (\ListRoomsResponse' {httpStatus} -> httpStatus) (\s@ListRoomsResponse' {} a -> s {httpStatus = a} :: ListRoomsResponse)

instance Prelude.NFData ListRoomsResponse where
  rnf ListRoomsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rooms
      `Prelude.seq` Prelude.rnf httpStatus
