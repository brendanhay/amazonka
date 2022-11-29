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
-- Module      : Amazonka.MacieV2.ListInvitations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the Amazon Macie membership invitations that
-- were received by an account.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListInvitations
  ( -- * Creating a Request
    ListInvitations (..),
    newListInvitations,

    -- * Request Lenses
    listInvitations_nextToken,
    listInvitations_maxResults,

    -- * Destructuring the Response
    ListInvitationsResponse (..),
    newListInvitationsResponse,

    -- * Response Lenses
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInvitations' smart constructor.
data ListInvitations = ListInvitations'
  { -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInvitations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInvitations_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'maxResults', 'listInvitations_maxResults' - The maximum number of items to include in each page of a paginated
-- response.
newListInvitations ::
  ListInvitations
newListInvitations =
  ListInvitations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listInvitations_nextToken :: Lens.Lens' ListInvitations (Prelude.Maybe Prelude.Text)
listInvitations_nextToken = Lens.lens (\ListInvitations' {nextToken} -> nextToken) (\s@ListInvitations' {} a -> s {nextToken = a} :: ListInvitations)

-- | The maximum number of items to include in each page of a paginated
-- response.
listInvitations_maxResults :: Lens.Lens' ListInvitations (Prelude.Maybe Prelude.Natural)
listInvitations_maxResults = Lens.lens (\ListInvitations' {maxResults} -> maxResults) (\s@ListInvitations' {} a -> s {maxResults = a} :: ListInvitations)

instance Core.AWSPager ListInvitations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInvitationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInvitationsResponse_invitations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInvitations_nextToken
          Lens..~ rs
          Lens.^? listInvitationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListInvitations where
  type
    AWSResponse ListInvitations =
      ListInvitationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInvitationsResponse'
            Prelude.<$> (x Core..?> "invitations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInvitations where
  hashWithSalt _salt ListInvitations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListInvitations where
  rnf ListInvitations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListInvitations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListInvitations where
  toPath = Prelude.const "/invitations"

instance Core.ToQuery ListInvitations where
  toQuery ListInvitations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListInvitationsResponse' smart constructor.
data ListInvitationsResponse = ListInvitationsResponse'
  { -- | An array of objects, one for each invitation that was received by the
    -- account.
    invitations :: Prelude.Maybe [Invitation],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInvitationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitations', 'listInvitationsResponse_invitations' - An array of objects, one for each invitation that was received by the
-- account.
--
-- 'nextToken', 'listInvitationsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listInvitationsResponse_httpStatus' - The response's http status code.
newListInvitationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInvitationsResponse
newListInvitationsResponse pHttpStatus_ =
  ListInvitationsResponse'
    { invitations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each invitation that was received by the
-- account.
listInvitationsResponse_invitations :: Lens.Lens' ListInvitationsResponse (Prelude.Maybe [Invitation])
listInvitationsResponse_invitations = Lens.lens (\ListInvitationsResponse' {invitations} -> invitations) (\s@ListInvitationsResponse' {} a -> s {invitations = a} :: ListInvitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listInvitationsResponse_nextToken :: Lens.Lens' ListInvitationsResponse (Prelude.Maybe Prelude.Text)
listInvitationsResponse_nextToken = Lens.lens (\ListInvitationsResponse' {nextToken} -> nextToken) (\s@ListInvitationsResponse' {} a -> s {nextToken = a} :: ListInvitationsResponse)

-- | The response's http status code.
listInvitationsResponse_httpStatus :: Lens.Lens' ListInvitationsResponse Prelude.Int
listInvitationsResponse_httpStatus = Lens.lens (\ListInvitationsResponse' {httpStatus} -> httpStatus) (\s@ListInvitationsResponse' {} a -> s {httpStatus = a} :: ListInvitationsResponse)

instance Prelude.NFData ListInvitationsResponse where
  rnf ListInvitationsResponse' {..} =
    Prelude.rnf invitations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
