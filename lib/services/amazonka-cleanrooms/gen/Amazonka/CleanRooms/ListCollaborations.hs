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
-- Module      : Amazonka.CleanRooms.ListCollaborations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists collaborations the caller owns, is active in, or has been invited
-- to.
--
-- This operation returns paginated results.
module Amazonka.CleanRooms.ListCollaborations
  ( -- * Creating a Request
    ListCollaborations (..),
    newListCollaborations,

    -- * Request Lenses
    listCollaborations_maxResults,
    listCollaborations_memberStatus,
    listCollaborations_nextToken,

    -- * Destructuring the Response
    ListCollaborationsResponse (..),
    newListCollaborationsResponse,

    -- * Response Lenses
    listCollaborationsResponse_nextToken,
    listCollaborationsResponse_httpStatus,
    listCollaborationsResponse_collaborationList,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCollaborations' smart constructor.
data ListCollaborations = ListCollaborations'
  { -- | The maximum size of the results that is returned per call. Service
    -- chooses a default if it has not been set. Service may return a nextToken
    -- even if the maximum results has not been met.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The caller\'s status in a collaboration.
    memberStatus :: Prelude.Maybe FilterableMemberStatus,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollaborations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCollaborations_maxResults' - The maximum size of the results that is returned per call. Service
-- chooses a default if it has not been set. Service may return a nextToken
-- even if the maximum results has not been met.
--
-- 'memberStatus', 'listCollaborations_memberStatus' - The caller\'s status in a collaboration.
--
-- 'nextToken', 'listCollaborations_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
newListCollaborations ::
  ListCollaborations
newListCollaborations =
  ListCollaborations'
    { maxResults = Prelude.Nothing,
      memberStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum size of the results that is returned per call. Service
-- chooses a default if it has not been set. Service may return a nextToken
-- even if the maximum results has not been met.
listCollaborations_maxResults :: Lens.Lens' ListCollaborations (Prelude.Maybe Prelude.Natural)
listCollaborations_maxResults = Lens.lens (\ListCollaborations' {maxResults} -> maxResults) (\s@ListCollaborations' {} a -> s {maxResults = a} :: ListCollaborations)

-- | The caller\'s status in a collaboration.
listCollaborations_memberStatus :: Lens.Lens' ListCollaborations (Prelude.Maybe FilterableMemberStatus)
listCollaborations_memberStatus = Lens.lens (\ListCollaborations' {memberStatus} -> memberStatus) (\s@ListCollaborations' {} a -> s {memberStatus = a} :: ListCollaborations)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listCollaborations_nextToken :: Lens.Lens' ListCollaborations (Prelude.Maybe Prelude.Text)
listCollaborations_nextToken = Lens.lens (\ListCollaborations' {nextToken} -> nextToken) (\s@ListCollaborations' {} a -> s {nextToken = a} :: ListCollaborations)

instance Core.AWSPager ListCollaborations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCollaborationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listCollaborationsResponse_collaborationList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCollaborations_nextToken
          Lens..~ rs
          Lens.^? listCollaborationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCollaborations where
  type
    AWSResponse ListCollaborations =
      ListCollaborationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCollaborationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "collaborationList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListCollaborations where
  hashWithSalt _salt ListCollaborations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` memberStatus
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCollaborations where
  rnf ListCollaborations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf memberStatus
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCollaborations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCollaborations where
  toPath = Prelude.const "/collaborations"

instance Data.ToQuery ListCollaborations where
  toQuery ListCollaborations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "memberStatus" Data.=: memberStatus,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCollaborationsResponse' smart constructor.
data ListCollaborationsResponse = ListCollaborationsResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of collaborations.
    collaborationList :: [CollaborationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollaborationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCollaborationsResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'httpStatus', 'listCollaborationsResponse_httpStatus' - The response's http status code.
--
-- 'collaborationList', 'listCollaborationsResponse_collaborationList' - The list of collaborations.
newListCollaborationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCollaborationsResponse
newListCollaborationsResponse pHttpStatus_ =
  ListCollaborationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      collaborationList = Prelude.mempty
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listCollaborationsResponse_nextToken :: Lens.Lens' ListCollaborationsResponse (Prelude.Maybe Prelude.Text)
listCollaborationsResponse_nextToken = Lens.lens (\ListCollaborationsResponse' {nextToken} -> nextToken) (\s@ListCollaborationsResponse' {} a -> s {nextToken = a} :: ListCollaborationsResponse)

-- | The response's http status code.
listCollaborationsResponse_httpStatus :: Lens.Lens' ListCollaborationsResponse Prelude.Int
listCollaborationsResponse_httpStatus = Lens.lens (\ListCollaborationsResponse' {httpStatus} -> httpStatus) (\s@ListCollaborationsResponse' {} a -> s {httpStatus = a} :: ListCollaborationsResponse)

-- | The list of collaborations.
listCollaborationsResponse_collaborationList :: Lens.Lens' ListCollaborationsResponse [CollaborationSummary]
listCollaborationsResponse_collaborationList = Lens.lens (\ListCollaborationsResponse' {collaborationList} -> collaborationList) (\s@ListCollaborationsResponse' {} a -> s {collaborationList = a} :: ListCollaborationsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListCollaborationsResponse where
  rnf ListCollaborationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf collaborationList
