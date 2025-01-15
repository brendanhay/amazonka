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
-- Module      : Amazonka.ManagedBlockChain.ListMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the members in a network and properties of their
-- configurations.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.ListMembers
  ( -- * Creating a Request
    ListMembers (..),
    newListMembers,

    -- * Request Lenses
    listMembers_isOwned,
    listMembers_maxResults,
    listMembers_name,
    listMembers_nextToken,
    listMembers_status,
    listMembers_networkId,

    -- * Destructuring the Response
    ListMembersResponse (..),
    newListMembersResponse,

    -- * Response Lenses
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMembers' smart constructor.
data ListMembers = ListMembers'
  { -- | An optional Boolean value. If provided, the request is limited either to
    -- members that the current Amazon Web Services account owns (@true@) or
    -- that other Amazon Web Services accountsn own (@false@). If omitted, all
    -- members are listed.
    isOwned :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of members to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The optional name of the member to list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional status specifier. If provided, only members currently in
    -- this status are listed.
    status :: Prelude.Maybe MemberStatus,
    -- | The unique identifier of the network for which to list members.
    networkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isOwned', 'listMembers_isOwned' - An optional Boolean value. If provided, the request is limited either to
-- members that the current Amazon Web Services account owns (@true@) or
-- that other Amazon Web Services accountsn own (@false@). If omitted, all
-- members are listed.
--
-- 'maxResults', 'listMembers_maxResults' - The maximum number of members to return in the request.
--
-- 'name', 'listMembers_name' - The optional name of the member to list.
--
-- 'nextToken', 'listMembers_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'status', 'listMembers_status' - An optional status specifier. If provided, only members currently in
-- this status are listed.
--
-- 'networkId', 'listMembers_networkId' - The unique identifier of the network for which to list members.
newListMembers ::
  -- | 'networkId'
  Prelude.Text ->
  ListMembers
newListMembers pNetworkId_ =
  ListMembers'
    { isOwned = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      networkId = pNetworkId_
    }

-- | An optional Boolean value. If provided, the request is limited either to
-- members that the current Amazon Web Services account owns (@true@) or
-- that other Amazon Web Services accountsn own (@false@). If omitted, all
-- members are listed.
listMembers_isOwned :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Bool)
listMembers_isOwned = Lens.lens (\ListMembers' {isOwned} -> isOwned) (\s@ListMembers' {} a -> s {isOwned = a} :: ListMembers)

-- | The maximum number of members to return in the request.
listMembers_maxResults :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Natural)
listMembers_maxResults = Lens.lens (\ListMembers' {maxResults} -> maxResults) (\s@ListMembers' {} a -> s {maxResults = a} :: ListMembers)

-- | The optional name of the member to list.
listMembers_name :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Text)
listMembers_name = Lens.lens (\ListMembers' {name} -> name) (\s@ListMembers' {} a -> s {name = a} :: ListMembers)

-- | The pagination token that indicates the next set of results to retrieve.
listMembers_nextToken :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Text)
listMembers_nextToken = Lens.lens (\ListMembers' {nextToken} -> nextToken) (\s@ListMembers' {} a -> s {nextToken = a} :: ListMembers)

-- | An optional status specifier. If provided, only members currently in
-- this status are listed.
listMembers_status :: Lens.Lens' ListMembers (Prelude.Maybe MemberStatus)
listMembers_status = Lens.lens (\ListMembers' {status} -> status) (\s@ListMembers' {} a -> s {status = a} :: ListMembers)

-- | The unique identifier of the network for which to list members.
listMembers_networkId :: Lens.Lens' ListMembers Prelude.Text
listMembers_networkId = Lens.lens (\ListMembers' {networkId} -> networkId) (\s@ListMembers' {} a -> s {networkId = a} :: ListMembers)

instance Core.AWSRequest ListMembers where
  type AWSResponse ListMembers = ListMembersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMembersResponse'
            Prelude.<$> (x Data..?> "Members" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMembers where
  hashWithSalt _salt ListMembers' {..} =
    _salt
      `Prelude.hashWithSalt` isOwned
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` networkId

instance Prelude.NFData ListMembers where
  rnf ListMembers' {..} =
    Prelude.rnf isOwned `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf status `Prelude.seq`
              Prelude.rnf networkId

instance Data.ToHeaders ListMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListMembers where
  toPath ListMembers' {..} =
    Prelude.mconcat
      ["/networks/", Data.toBS networkId, "/members"]

instance Data.ToQuery ListMembers where
  toQuery ListMembers' {..} =
    Prelude.mconcat
      [ "isOwned" Data.=: isOwned,
        "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { -- | An array of @MemberSummary@ objects. Each object contains details about
    -- a network member.
    members :: Prelude.Maybe [MemberSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'members', 'listMembersResponse_members' - An array of @MemberSummary@ objects. Each object contains details about
-- a network member.
--
-- 'nextToken', 'listMembersResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listMembersResponse_httpStatus' - The response's http status code.
newListMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMembersResponse
newListMembersResponse pHttpStatus_ =
  ListMembersResponse'
    { members = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @MemberSummary@ objects. Each object contains details about
-- a network member.
listMembersResponse_members :: Lens.Lens' ListMembersResponse (Prelude.Maybe [MemberSummary])
listMembersResponse_members = Lens.lens (\ListMembersResponse' {members} -> members) (\s@ListMembersResponse' {} a -> s {members = a} :: ListMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that indicates the next set of results to retrieve.
listMembersResponse_nextToken :: Lens.Lens' ListMembersResponse (Prelude.Maybe Prelude.Text)
listMembersResponse_nextToken = Lens.lens (\ListMembersResponse' {nextToken} -> nextToken) (\s@ListMembersResponse' {} a -> s {nextToken = a} :: ListMembersResponse)

-- | The response's http status code.
listMembersResponse_httpStatus :: Lens.Lens' ListMembersResponse Prelude.Int
listMembersResponse_httpStatus = Lens.lens (\ListMembersResponse' {httpStatus} -> httpStatus) (\s@ListMembersResponse' {} a -> s {httpStatus = a} :: ListMembersResponse)

instance Prelude.NFData ListMembersResponse where
  rnf ListMembersResponse' {..} =
    Prelude.rnf members `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
