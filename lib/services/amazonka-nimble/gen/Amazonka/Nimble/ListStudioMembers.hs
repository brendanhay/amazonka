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
-- Module      : Amazonka.Nimble.ListStudioMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get all users in a given studio membership.
--
-- @ListStudioMembers@ only returns admin members.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListStudioMembers
  ( -- * Creating a Request
    ListStudioMembers (..),
    newListStudioMembers,

    -- * Request Lenses
    listStudioMembers_maxResults,
    listStudioMembers_nextToken,
    listStudioMembers_studioId,

    -- * Destructuring the Response
    ListStudioMembersResponse (..),
    newListStudioMembersResponse,

    -- * Response Lenses
    listStudioMembersResponse_members,
    listStudioMembersResponse_nextToken,
    listStudioMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStudioMembers' smart constructor.
data ListStudioMembers = ListStudioMembers'
  { -- | The max number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStudioMembers_maxResults' - The max number of results to return in the response.
--
-- 'nextToken', 'listStudioMembers_nextToken' - The token to request the next page of results.
--
-- 'studioId', 'listStudioMembers_studioId' - The studio ID.
newListStudioMembers ::
  -- | 'studioId'
  Prelude.Text ->
  ListStudioMembers
newListStudioMembers pStudioId_ =
  ListStudioMembers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The max number of results to return in the response.
listStudioMembers_maxResults :: Lens.Lens' ListStudioMembers (Prelude.Maybe Prelude.Natural)
listStudioMembers_maxResults = Lens.lens (\ListStudioMembers' {maxResults} -> maxResults) (\s@ListStudioMembers' {} a -> s {maxResults = a} :: ListStudioMembers)

-- | The token to request the next page of results.
listStudioMembers_nextToken :: Lens.Lens' ListStudioMembers (Prelude.Maybe Prelude.Text)
listStudioMembers_nextToken = Lens.lens (\ListStudioMembers' {nextToken} -> nextToken) (\s@ListStudioMembers' {} a -> s {nextToken = a} :: ListStudioMembers)

-- | The studio ID.
listStudioMembers_studioId :: Lens.Lens' ListStudioMembers Prelude.Text
listStudioMembers_studioId = Lens.lens (\ListStudioMembers' {studioId} -> studioId) (\s@ListStudioMembers' {} a -> s {studioId = a} :: ListStudioMembers)

instance Core.AWSPager ListStudioMembers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStudioMembersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStudioMembersResponse_members
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStudioMembers_nextToken
          Lens..~ rs
          Lens.^? listStudioMembersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStudioMembers where
  type
    AWSResponse ListStudioMembers =
      ListStudioMembersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudioMembersResponse'
            Prelude.<$> (x Data..?> "members" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudioMembers where
  hashWithSalt _salt ListStudioMembers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListStudioMembers where
  rnf ListStudioMembers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders ListStudioMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStudioMembers where
  toPath ListStudioMembers' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/membership"
      ]

instance Data.ToQuery ListStudioMembers where
  toQuery ListStudioMembers' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListStudioMembersResponse' smart constructor.
data ListStudioMembersResponse = ListStudioMembersResponse'
  { -- | A list of admin members.
    members :: Prelude.Maybe [StudioMembership],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudioMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'members', 'listStudioMembersResponse_members' - A list of admin members.
--
-- 'nextToken', 'listStudioMembersResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listStudioMembersResponse_httpStatus' - The response's http status code.
newListStudioMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudioMembersResponse
newListStudioMembersResponse pHttpStatus_ =
  ListStudioMembersResponse'
    { members =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of admin members.
listStudioMembersResponse_members :: Lens.Lens' ListStudioMembersResponse (Prelude.Maybe [StudioMembership])
listStudioMembersResponse_members = Lens.lens (\ListStudioMembersResponse' {members} -> members) (\s@ListStudioMembersResponse' {} a -> s {members = a} :: ListStudioMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listStudioMembersResponse_nextToken :: Lens.Lens' ListStudioMembersResponse (Prelude.Maybe Prelude.Text)
listStudioMembersResponse_nextToken = Lens.lens (\ListStudioMembersResponse' {nextToken} -> nextToken) (\s@ListStudioMembersResponse' {} a -> s {nextToken = a} :: ListStudioMembersResponse)

-- | The response's http status code.
listStudioMembersResponse_httpStatus :: Lens.Lens' ListStudioMembersResponse Prelude.Int
listStudioMembersResponse_httpStatus = Lens.lens (\ListStudioMembersResponse' {httpStatus} -> httpStatus) (\s@ListStudioMembersResponse' {} a -> s {httpStatus = a} :: ListStudioMembersResponse)

instance Prelude.NFData ListStudioMembersResponse where
  rnf ListStudioMembersResponse' {..} =
    Prelude.rnf members
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
