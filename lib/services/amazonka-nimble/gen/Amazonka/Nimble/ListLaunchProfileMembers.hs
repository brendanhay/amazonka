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
-- Module      : Amazonka.Nimble.ListLaunchProfileMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get all users in a given launch profile membership.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListLaunchProfileMembers
  ( -- * Creating a Request
    ListLaunchProfileMembers (..),
    newListLaunchProfileMembers,

    -- * Request Lenses
    listLaunchProfileMembers_nextToken,
    listLaunchProfileMembers_maxResults,
    listLaunchProfileMembers_launchProfileId,
    listLaunchProfileMembers_studioId,

    -- * Destructuring the Response
    ListLaunchProfileMembersResponse (..),
    newListLaunchProfileMembersResponse,

    -- * Response Lenses
    listLaunchProfileMembersResponse_nextToken,
    listLaunchProfileMembersResponse_members,
    listLaunchProfileMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLaunchProfileMembers' smart constructor.
data ListLaunchProfileMembers = ListLaunchProfileMembers'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The max number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Launch Profile ID.
    launchProfileId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLaunchProfileMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLaunchProfileMembers_nextToken' - The token to request the next page of results.
--
-- 'maxResults', 'listLaunchProfileMembers_maxResults' - The max number of results to return in the response.
--
-- 'launchProfileId', 'listLaunchProfileMembers_launchProfileId' - The Launch Profile ID.
--
-- 'studioId', 'listLaunchProfileMembers_studioId' - The studio ID.
newListLaunchProfileMembers ::
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  ListLaunchProfileMembers
newListLaunchProfileMembers
  pLaunchProfileId_
  pStudioId_ =
    ListLaunchProfileMembers'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        launchProfileId = pLaunchProfileId_,
        studioId = pStudioId_
      }

-- | The token to request the next page of results.
listLaunchProfileMembers_nextToken :: Lens.Lens' ListLaunchProfileMembers (Prelude.Maybe Prelude.Text)
listLaunchProfileMembers_nextToken = Lens.lens (\ListLaunchProfileMembers' {nextToken} -> nextToken) (\s@ListLaunchProfileMembers' {} a -> s {nextToken = a} :: ListLaunchProfileMembers)

-- | The max number of results to return in the response.
listLaunchProfileMembers_maxResults :: Lens.Lens' ListLaunchProfileMembers (Prelude.Maybe Prelude.Natural)
listLaunchProfileMembers_maxResults = Lens.lens (\ListLaunchProfileMembers' {maxResults} -> maxResults) (\s@ListLaunchProfileMembers' {} a -> s {maxResults = a} :: ListLaunchProfileMembers)

-- | The Launch Profile ID.
listLaunchProfileMembers_launchProfileId :: Lens.Lens' ListLaunchProfileMembers Prelude.Text
listLaunchProfileMembers_launchProfileId = Lens.lens (\ListLaunchProfileMembers' {launchProfileId} -> launchProfileId) (\s@ListLaunchProfileMembers' {} a -> s {launchProfileId = a} :: ListLaunchProfileMembers)

-- | The studio ID.
listLaunchProfileMembers_studioId :: Lens.Lens' ListLaunchProfileMembers Prelude.Text
listLaunchProfileMembers_studioId = Lens.lens (\ListLaunchProfileMembers' {studioId} -> studioId) (\s@ListLaunchProfileMembers' {} a -> s {studioId = a} :: ListLaunchProfileMembers)

instance Core.AWSPager ListLaunchProfileMembers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLaunchProfileMembersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLaunchProfileMembersResponse_members
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLaunchProfileMembers_nextToken
          Lens..~ rs
          Lens.^? listLaunchProfileMembersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLaunchProfileMembers where
  type
    AWSResponse ListLaunchProfileMembers =
      ListLaunchProfileMembersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLaunchProfileMembersResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "members" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLaunchProfileMembers where
  hashWithSalt _salt ListLaunchProfileMembers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListLaunchProfileMembers where
  rnf ListLaunchProfileMembers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf studioId

instance Core.ToHeaders ListLaunchProfileMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLaunchProfileMembers where
  toPath ListLaunchProfileMembers' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles/",
        Core.toBS launchProfileId,
        "/membership"
      ]

instance Core.ToQuery ListLaunchProfileMembers where
  toQuery ListLaunchProfileMembers' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLaunchProfileMembersResponse' smart constructor.
data ListLaunchProfileMembersResponse = ListLaunchProfileMembersResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of members.
    members :: Prelude.Maybe [LaunchProfileMembership],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLaunchProfileMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLaunchProfileMembersResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'members', 'listLaunchProfileMembersResponse_members' - A list of members.
--
-- 'httpStatus', 'listLaunchProfileMembersResponse_httpStatus' - The response's http status code.
newListLaunchProfileMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLaunchProfileMembersResponse
newListLaunchProfileMembersResponse pHttpStatus_ =
  ListLaunchProfileMembersResponse'
    { nextToken =
        Prelude.Nothing,
      members = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listLaunchProfileMembersResponse_nextToken :: Lens.Lens' ListLaunchProfileMembersResponse (Prelude.Maybe Prelude.Text)
listLaunchProfileMembersResponse_nextToken = Lens.lens (\ListLaunchProfileMembersResponse' {nextToken} -> nextToken) (\s@ListLaunchProfileMembersResponse' {} a -> s {nextToken = a} :: ListLaunchProfileMembersResponse)

-- | A list of members.
listLaunchProfileMembersResponse_members :: Lens.Lens' ListLaunchProfileMembersResponse (Prelude.Maybe [LaunchProfileMembership])
listLaunchProfileMembersResponse_members = Lens.lens (\ListLaunchProfileMembersResponse' {members} -> members) (\s@ListLaunchProfileMembersResponse' {} a -> s {members = a} :: ListLaunchProfileMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLaunchProfileMembersResponse_httpStatus :: Lens.Lens' ListLaunchProfileMembersResponse Prelude.Int
listLaunchProfileMembersResponse_httpStatus = Lens.lens (\ListLaunchProfileMembersResponse' {httpStatus} -> httpStatus) (\s@ListLaunchProfileMembersResponse' {} a -> s {httpStatus = a} :: ListLaunchProfileMembersResponse)

instance
  Prelude.NFData
    ListLaunchProfileMembersResponse
  where
  rnf ListLaunchProfileMembersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf httpStatus
