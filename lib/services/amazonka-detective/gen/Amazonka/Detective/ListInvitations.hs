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
-- Module      : Amazonka.Detective.ListInvitations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of open and accepted behavior graph invitations for
-- the member account. This operation can only be called by an invited
-- member account.
--
-- Open invitations are invitations that the member account has not
-- responded to.
--
-- The results do not include behavior graphs for which the member account
-- declined the invitation. The results also do not include behavior graphs
-- that the member account resigned from or was removed from.
module Amazonka.Detective.ListInvitations
  ( -- * Creating a Request
    ListInvitations (..),
    newListInvitations,

    -- * Request Lenses
    listInvitations_maxResults,
    listInvitations_nextToken,

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
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInvitations' smart constructor.
data ListInvitations = ListInvitations'
  { -- | The maximum number of behavior graph invitations to return in the
    -- response. The total must be less than the overall limit on the number of
    -- results to return, which is currently 200.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For requests to retrieve the next page of results, the pagination token
    -- that was returned with the previous page of results. The initial request
    -- does not include a pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listInvitations_maxResults' - The maximum number of behavior graph invitations to return in the
-- response. The total must be less than the overall limit on the number of
-- results to return, which is currently 200.
--
-- 'nextToken', 'listInvitations_nextToken' - For requests to retrieve the next page of results, the pagination token
-- that was returned with the previous page of results. The initial request
-- does not include a pagination token.
newListInvitations ::
  ListInvitations
newListInvitations =
  ListInvitations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of behavior graph invitations to return in the
-- response. The total must be less than the overall limit on the number of
-- results to return, which is currently 200.
listInvitations_maxResults :: Lens.Lens' ListInvitations (Prelude.Maybe Prelude.Natural)
listInvitations_maxResults = Lens.lens (\ListInvitations' {maxResults} -> maxResults) (\s@ListInvitations' {} a -> s {maxResults = a} :: ListInvitations)

-- | For requests to retrieve the next page of results, the pagination token
-- that was returned with the previous page of results. The initial request
-- does not include a pagination token.
listInvitations_nextToken :: Lens.Lens' ListInvitations (Prelude.Maybe Prelude.Text)
listInvitations_nextToken = Lens.lens (\ListInvitations' {nextToken} -> nextToken) (\s@ListInvitations' {} a -> s {nextToken = a} :: ListInvitations)

instance Core.AWSRequest ListInvitations where
  type
    AWSResponse ListInvitations =
      ListInvitationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInvitationsResponse'
            Prelude.<$> (x Data..?> "Invitations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInvitations where
  hashWithSalt _salt ListInvitations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListInvitations where
  rnf ListInvitations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListInvitations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInvitations where
  toJSON ListInvitations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListInvitations where
  toPath = Prelude.const "/invitations/list"

instance Data.ToQuery ListInvitations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInvitationsResponse' smart constructor.
data ListInvitationsResponse = ListInvitationsResponse'
  { -- | The list of behavior graphs for which the member account has open or
    -- accepted invitations.
    invitations :: Prelude.Maybe [MemberDetail],
    -- | If there are more behavior graphs remaining in the results, then this is
    -- the pagination token to use to request the next page of behavior graphs.
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
-- 'invitations', 'listInvitationsResponse_invitations' - The list of behavior graphs for which the member account has open or
-- accepted invitations.
--
-- 'nextToken', 'listInvitationsResponse_nextToken' - If there are more behavior graphs remaining in the results, then this is
-- the pagination token to use to request the next page of behavior graphs.
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

-- | The list of behavior graphs for which the member account has open or
-- accepted invitations.
listInvitationsResponse_invitations :: Lens.Lens' ListInvitationsResponse (Prelude.Maybe [MemberDetail])
listInvitationsResponse_invitations = Lens.lens (\ListInvitationsResponse' {invitations} -> invitations) (\s@ListInvitationsResponse' {} a -> s {invitations = a} :: ListInvitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more behavior graphs remaining in the results, then this is
-- the pagination token to use to request the next page of behavior graphs.
listInvitationsResponse_nextToken :: Lens.Lens' ListInvitationsResponse (Prelude.Maybe Prelude.Text)
listInvitationsResponse_nextToken = Lens.lens (\ListInvitationsResponse' {nextToken} -> nextToken) (\s@ListInvitationsResponse' {} a -> s {nextToken = a} :: ListInvitationsResponse)

-- | The response's http status code.
listInvitationsResponse_httpStatus :: Lens.Lens' ListInvitationsResponse Prelude.Int
listInvitationsResponse_httpStatus = Lens.lens (\ListInvitationsResponse' {httpStatus} -> httpStatus) (\s@ListInvitationsResponse' {} a -> s {httpStatus = a} :: ListInvitationsResponse)

instance Prelude.NFData ListInvitationsResponse where
  rnf ListInvitationsResponse' {..} =
    Prelude.rnf invitations `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
