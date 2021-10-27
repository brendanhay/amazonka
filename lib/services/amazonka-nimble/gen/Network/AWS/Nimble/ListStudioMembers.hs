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
-- Module      : Network.AWS.Nimble.ListStudioMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get all users in a given studio membership.
--
-- This operation returns paginated results.
module Network.AWS.Nimble.ListStudioMembers
  ( -- * Creating a Request
    ListStudioMembers (..),
    newListStudioMembers,

    -- * Request Lenses
    listStudioMembers_nextToken,
    listStudioMembers_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Nimble.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStudioMembers' smart constructor.
data ListStudioMembers = ListStudioMembers'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listStudioMembers_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'listStudioMembers_maxResults' - The maximum number of results to be returned per request.
--
-- 'studioId', 'listStudioMembers_studioId' - The studio ID.
newListStudioMembers ::
  -- | 'studioId'
  Prelude.Text ->
  ListStudioMembers
newListStudioMembers pStudioId_ =
  ListStudioMembers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listStudioMembers_nextToken :: Lens.Lens' ListStudioMembers (Prelude.Maybe Prelude.Text)
listStudioMembers_nextToken = Lens.lens (\ListStudioMembers' {nextToken} -> nextToken) (\s@ListStudioMembers' {} a -> s {nextToken = a} :: ListStudioMembers)

-- | The maximum number of results to be returned per request.
listStudioMembers_maxResults :: Lens.Lens' ListStudioMembers (Prelude.Maybe Prelude.Natural)
listStudioMembers_maxResults = Lens.lens (\ListStudioMembers' {maxResults} -> maxResults) (\s@ListStudioMembers' {} a -> s {maxResults = a} :: ListStudioMembers)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudioMembersResponse'
            Prelude.<$> (x Core..?> "members" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudioMembers

instance Prelude.NFData ListStudioMembers

instance Core.ToHeaders ListStudioMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListStudioMembers where
  toPath ListStudioMembers' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/membership"
      ]

instance Core.ToQuery ListStudioMembers where
  toQuery ListStudioMembers' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListStudioMembersResponse' smart constructor.
data ListStudioMembersResponse = ListStudioMembersResponse'
  { -- | A list of members.
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
-- 'members', 'listStudioMembersResponse_members' - A list of members.
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

-- | A list of members.
listStudioMembersResponse_members :: Lens.Lens' ListStudioMembersResponse (Prelude.Maybe [StudioMembership])
listStudioMembersResponse_members = Lens.lens (\ListStudioMembersResponse' {members} -> members) (\s@ListStudioMembersResponse' {} a -> s {members = a} :: ListStudioMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listStudioMembersResponse_nextToken :: Lens.Lens' ListStudioMembersResponse (Prelude.Maybe Prelude.Text)
listStudioMembersResponse_nextToken = Lens.lens (\ListStudioMembersResponse' {nextToken} -> nextToken) (\s@ListStudioMembersResponse' {} a -> s {nextToken = a} :: ListStudioMembersResponse)

-- | The response's http status code.
listStudioMembersResponse_httpStatus :: Lens.Lens' ListStudioMembersResponse Prelude.Int
listStudioMembersResponse_httpStatus = Lens.lens (\ListStudioMembersResponse' {httpStatus} -> httpStatus) (\s@ListStudioMembersResponse' {} a -> s {httpStatus = a} :: ListStudioMembersResponse)

instance Prelude.NFData ListStudioMembersResponse
