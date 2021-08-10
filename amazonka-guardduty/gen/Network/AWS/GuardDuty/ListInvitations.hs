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
-- Module      : Network.AWS.GuardDuty.ListInvitations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all GuardDuty membership invitations that were sent to the current
-- AWS account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListInvitations
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
    listInvitationsResponse_nextToken,
    listInvitationsResponse_invitations,
    listInvitationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInvitations' smart constructor.
data ListInvitations = ListInvitations'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 50. The maximum value is
    -- 50.
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
-- 'nextToken', 'listInvitations_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'maxResults', 'listInvitations_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
newListInvitations ::
  ListInvitations
newListInvitations =
  ListInvitations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listInvitations_nextToken :: Lens.Lens' ListInvitations (Prelude.Maybe Prelude.Text)
listInvitations_nextToken = Lens.lens (\ListInvitations' {nextToken} -> nextToken) (\s@ListInvitations' {} a -> s {nextToken = a} :: ListInvitations)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 50. The maximum value is
-- 50.
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInvitationsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "invitations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInvitations

instance Prelude.NFData ListInvitations

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
  toPath = Prelude.const "/invitation"

instance Core.ToQuery ListInvitations where
  toQuery ListInvitations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListInvitationsResponse' smart constructor.
data ListInvitationsResponse = ListInvitationsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of invitation descriptions.
    invitations :: Prelude.Maybe [Invitation],
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
-- 'nextToken', 'listInvitationsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'invitations', 'listInvitationsResponse_invitations' - A list of invitation descriptions.
--
-- 'httpStatus', 'listInvitationsResponse_httpStatus' - The response's http status code.
newListInvitationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInvitationsResponse
newListInvitationsResponse pHttpStatus_ =
  ListInvitationsResponse'
    { nextToken =
        Prelude.Nothing,
      invitations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listInvitationsResponse_nextToken :: Lens.Lens' ListInvitationsResponse (Prelude.Maybe Prelude.Text)
listInvitationsResponse_nextToken = Lens.lens (\ListInvitationsResponse' {nextToken} -> nextToken) (\s@ListInvitationsResponse' {} a -> s {nextToken = a} :: ListInvitationsResponse)

-- | A list of invitation descriptions.
listInvitationsResponse_invitations :: Lens.Lens' ListInvitationsResponse (Prelude.Maybe [Invitation])
listInvitationsResponse_invitations = Lens.lens (\ListInvitationsResponse' {invitations} -> invitations) (\s@ListInvitationsResponse' {} a -> s {invitations = a} :: ListInvitationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInvitationsResponse_httpStatus :: Lens.Lens' ListInvitationsResponse Prelude.Int
listInvitationsResponse_httpStatus = Lens.lens (\ListInvitationsResponse' {httpStatus} -> httpStatus) (\s@ListInvitationsResponse' {} a -> s {httpStatus = a} :: ListInvitationsResponse)

instance Prelude.NFData ListInvitationsResponse
