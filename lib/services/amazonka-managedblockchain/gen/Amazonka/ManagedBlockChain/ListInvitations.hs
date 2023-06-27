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
-- Module      : Amazonka.ManagedBlockChain.ListInvitations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all invitations for the current Amazon Web Services
-- account.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.ListInvitations
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
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInvitations' smart constructor.
data ListInvitations = ListInvitations'
  { -- | The maximum number of invitations to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results to retrieve.
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
-- 'maxResults', 'listInvitations_maxResults' - The maximum number of invitations to return.
--
-- 'nextToken', 'listInvitations_nextToken' - The pagination token that indicates the next set of results to retrieve.
newListInvitations ::
  ListInvitations
newListInvitations =
  ListInvitations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of invitations to return.
listInvitations_maxResults :: Lens.Lens' ListInvitations (Prelude.Maybe Prelude.Natural)
listInvitations_maxResults = Lens.lens (\ListInvitations' {maxResults} -> maxResults) (\s@ListInvitations' {} a -> s {maxResults = a} :: ListInvitations)

-- | The pagination token that indicates the next set of results to retrieve.
listInvitations_nextToken :: Lens.Lens' ListInvitations (Prelude.Maybe Prelude.Text)
listInvitations_nextToken = Lens.lens (\ListInvitations' {nextToken} -> nextToken) (\s@ListInvitations' {} a -> s {nextToken = a} :: ListInvitations)

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
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

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

instance Data.ToPath ListInvitations where
  toPath = Prelude.const "/invitations"

instance Data.ToQuery ListInvitations where
  toQuery ListInvitations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListInvitationsResponse' smart constructor.
data ListInvitationsResponse = ListInvitationsResponse'
  { -- | The invitations for the network.
    invitations :: Prelude.Maybe [Invitation],
    -- | The pagination token that indicates the next set of results to retrieve.
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
-- 'invitations', 'listInvitationsResponse_invitations' - The invitations for the network.
--
-- 'nextToken', 'listInvitationsResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
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

-- | The invitations for the network.
listInvitationsResponse_invitations :: Lens.Lens' ListInvitationsResponse (Prelude.Maybe [Invitation])
listInvitationsResponse_invitations = Lens.lens (\ListInvitationsResponse' {invitations} -> invitations) (\s@ListInvitationsResponse' {} a -> s {invitations = a} :: ListInvitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that indicates the next set of results to retrieve.
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
