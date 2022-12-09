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
-- Module      : Amazonka.WorkMail.ListGroupMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an overview of the members of a group. Users and groups can be
-- members of a group.
--
-- This operation returns paginated results.
module Amazonka.WorkMail.ListGroupMembers
  ( -- * Creating a Request
    ListGroupMembers (..),
    newListGroupMembers,

    -- * Request Lenses
    listGroupMembers_maxResults,
    listGroupMembers_nextToken,
    listGroupMembers_organizationId,
    listGroupMembers_groupId,

    -- * Destructuring the Response
    ListGroupMembersResponse (..),
    newListGroupMembersResponse,

    -- * Response Lenses
    listGroupMembersResponse_members,
    listGroupMembersResponse_nextToken,
    listGroupMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListGroupMembers' smart constructor.
data ListGroupMembers = ListGroupMembers'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the organization under which the group exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the group to which the members (users or groups) are
    -- associated.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGroupMembers_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listGroupMembers_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'organizationId', 'listGroupMembers_organizationId' - The identifier for the organization under which the group exists.
--
-- 'groupId', 'listGroupMembers_groupId' - The identifier for the group to which the members (users or groups) are
-- associated.
newListGroupMembers ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  ListGroupMembers
newListGroupMembers pOrganizationId_ pGroupId_ =
  ListGroupMembers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationId = pOrganizationId_,
      groupId = pGroupId_
    }

-- | The maximum number of results to return in a single call.
listGroupMembers_maxResults :: Lens.Lens' ListGroupMembers (Prelude.Maybe Prelude.Natural)
listGroupMembers_maxResults = Lens.lens (\ListGroupMembers' {maxResults} -> maxResults) (\s@ListGroupMembers' {} a -> s {maxResults = a} :: ListGroupMembers)

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listGroupMembers_nextToken :: Lens.Lens' ListGroupMembers (Prelude.Maybe Prelude.Text)
listGroupMembers_nextToken = Lens.lens (\ListGroupMembers' {nextToken} -> nextToken) (\s@ListGroupMembers' {} a -> s {nextToken = a} :: ListGroupMembers)

-- | The identifier for the organization under which the group exists.
listGroupMembers_organizationId :: Lens.Lens' ListGroupMembers Prelude.Text
listGroupMembers_organizationId = Lens.lens (\ListGroupMembers' {organizationId} -> organizationId) (\s@ListGroupMembers' {} a -> s {organizationId = a} :: ListGroupMembers)

-- | The identifier for the group to which the members (users or groups) are
-- associated.
listGroupMembers_groupId :: Lens.Lens' ListGroupMembers Prelude.Text
listGroupMembers_groupId = Lens.lens (\ListGroupMembers' {groupId} -> groupId) (\s@ListGroupMembers' {} a -> s {groupId = a} :: ListGroupMembers)

instance Core.AWSPager ListGroupMembers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupMembersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGroupMembersResponse_members
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGroupMembers_nextToken
          Lens..~ rs
          Lens.^? listGroupMembersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListGroupMembers where
  type
    AWSResponse ListGroupMembers =
      ListGroupMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupMembersResponse'
            Prelude.<$> (x Data..?> "Members" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroupMembers where
  hashWithSalt _salt ListGroupMembers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData ListGroupMembers where
  rnf ListGroupMembers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders ListGroupMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListGroupMembers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGroupMembers where
  toJSON ListGroupMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("GroupId" Data..= groupId)
          ]
      )

instance Data.ToPath ListGroupMembers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGroupMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGroupMembersResponse' smart constructor.
data ListGroupMembersResponse = ListGroupMembersResponse'
  { -- | The members associated to the group.
    members :: Prelude.Maybe [Member],
    -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'members', 'listGroupMembersResponse_members' - The members associated to the group.
--
-- 'nextToken', 'listGroupMembersResponse_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'httpStatus', 'listGroupMembersResponse_httpStatus' - The response's http status code.
newListGroupMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupMembersResponse
newListGroupMembersResponse pHttpStatus_ =
  ListGroupMembersResponse'
    { members =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The members associated to the group.
listGroupMembersResponse_members :: Lens.Lens' ListGroupMembersResponse (Prelude.Maybe [Member])
listGroupMembersResponse_members = Lens.lens (\ListGroupMembersResponse' {members} -> members) (\s@ListGroupMembersResponse' {} a -> s {members = a} :: ListGroupMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listGroupMembersResponse_nextToken :: Lens.Lens' ListGroupMembersResponse (Prelude.Maybe Prelude.Text)
listGroupMembersResponse_nextToken = Lens.lens (\ListGroupMembersResponse' {nextToken} -> nextToken) (\s@ListGroupMembersResponse' {} a -> s {nextToken = a} :: ListGroupMembersResponse)

-- | The response's http status code.
listGroupMembersResponse_httpStatus :: Lens.Lens' ListGroupMembersResponse Prelude.Int
listGroupMembersResponse_httpStatus = Lens.lens (\ListGroupMembersResponse' {httpStatus} -> httpStatus) (\s@ListGroupMembersResponse' {} a -> s {httpStatus = a} :: ListGroupMembersResponse)

instance Prelude.NFData ListGroupMembersResponse where
  rnf ListGroupMembersResponse' {..} =
    Prelude.rnf members
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
