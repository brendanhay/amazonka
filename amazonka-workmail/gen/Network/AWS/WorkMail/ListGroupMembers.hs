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
-- Module      : Network.AWS.WorkMail.ListGroupMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an overview of the members of a group. Users and groups can be
-- members of a group.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListGroupMembers
  ( -- * Creating a Request
    ListGroupMembers (..),
    newListGroupMembers,

    -- * Request Lenses
    listGroupMembers_nextToken,
    listGroupMembers_maxResults,
    listGroupMembers_organizationId,
    listGroupMembers_groupId,

    -- * Destructuring the Response
    ListGroupMembersResponse (..),
    newListGroupMembersResponse,

    -- * Response Lenses
    listGroupMembersResponse_nextToken,
    listGroupMembersResponse_members,
    listGroupMembersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListGroupMembers' smart constructor.
data ListGroupMembers = ListGroupMembers'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listGroupMembers_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'maxResults', 'listGroupMembers_maxResults' - The maximum number of results to return in a single call.
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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      organizationId = pOrganizationId_,
      groupId = pGroupId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listGroupMembers_nextToken :: Lens.Lens' ListGroupMembers (Prelude.Maybe Prelude.Text)
listGroupMembers_nextToken = Lens.lens (\ListGroupMembers' {nextToken} -> nextToken) (\s@ListGroupMembers' {} a -> s {nextToken = a} :: ListGroupMembers)

-- | The maximum number of results to return in a single call.
listGroupMembers_maxResults :: Lens.Lens' ListGroupMembers (Prelude.Maybe Prelude.Natural)
listGroupMembers_maxResults = Lens.lens (\ListGroupMembers' {maxResults} -> maxResults) (\s@ListGroupMembers' {} a -> s {maxResults = a} :: ListGroupMembers)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupMembersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Members" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroupMembers

instance Prelude.NFData ListGroupMembers

instance Core.ToHeaders ListGroupMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListGroupMembers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListGroupMembers where
  toJSON ListGroupMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("GroupId" Core..= groupId)
          ]
      )

instance Core.ToPath ListGroupMembers where
  toPath = Prelude.const "/"

instance Core.ToQuery ListGroupMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGroupMembersResponse' smart constructor.
data ListGroupMembersResponse = ListGroupMembersResponse'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The members associated to the group.
    members :: Prelude.Maybe [Member],
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
-- 'nextToken', 'listGroupMembersResponse_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'members', 'listGroupMembersResponse_members' - The members associated to the group.
--
-- 'httpStatus', 'listGroupMembersResponse_httpStatus' - The response's http status code.
newListGroupMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupMembersResponse
newListGroupMembersResponse pHttpStatus_ =
  ListGroupMembersResponse'
    { nextToken =
        Prelude.Nothing,
      members = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listGroupMembersResponse_nextToken :: Lens.Lens' ListGroupMembersResponse (Prelude.Maybe Prelude.Text)
listGroupMembersResponse_nextToken = Lens.lens (\ListGroupMembersResponse' {nextToken} -> nextToken) (\s@ListGroupMembersResponse' {} a -> s {nextToken = a} :: ListGroupMembersResponse)

-- | The members associated to the group.
listGroupMembersResponse_members :: Lens.Lens' ListGroupMembersResponse (Prelude.Maybe [Member])
listGroupMembersResponse_members = Lens.lens (\ListGroupMembersResponse' {members} -> members) (\s@ListGroupMembersResponse' {} a -> s {members = a} :: ListGroupMembersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGroupMembersResponse_httpStatus :: Lens.Lens' ListGroupMembersResponse Prelude.Int
listGroupMembersResponse_httpStatus = Lens.lens (\ListGroupMembersResponse' {httpStatus} -> httpStatus) (\s@ListGroupMembersResponse' {} a -> s {httpStatus = a} :: ListGroupMembersResponse)

instance Prelude.NFData ListGroupMembersResponse
