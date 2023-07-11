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
-- Module      : Amazonka.GuardDuty.ListMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details about all member accounts for the current GuardDuty
-- administrator account.
--
-- This operation returns paginated results.
module Amazonka.GuardDuty.ListMembers
  ( -- * Creating a Request
    ListMembers (..),
    newListMembers,

    -- * Request Lenses
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembers_detectorId,

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
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMembers' smart constructor.
data ListMembers = ListMembers'
  { -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to only return associated members or to return all
    -- members (including members who haven\'t been invited yet or have been
    -- disassociated). Member accounts must have been previously associated
    -- with the GuardDuty administrator account using
    -- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_CreateMembers.html Create Members>
    -- .
    onlyAssociated :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the detector the member is associated with.
    detectorId :: Prelude.Text
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
-- 'maxResults', 'listMembers_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
--
-- 'nextToken', 'listMembers_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'onlyAssociated', 'listMembers_onlyAssociated' - Specifies whether to only return associated members or to return all
-- members (including members who haven\'t been invited yet or have been
-- disassociated). Member accounts must have been previously associated
-- with the GuardDuty administrator account using
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_CreateMembers.html Create Members>
-- .
--
-- 'detectorId', 'listMembers_detectorId' - The unique ID of the detector the member is associated with.
newListMembers ::
  -- | 'detectorId'
  Prelude.Text ->
  ListMembers
newListMembers pDetectorId_ =
  ListMembers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      onlyAssociated = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
listMembers_maxResults :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Natural)
listMembers_maxResults = Lens.lens (\ListMembers' {maxResults} -> maxResults) (\s@ListMembers' {} a -> s {maxResults = a} :: ListMembers)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listMembers_nextToken :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Text)
listMembers_nextToken = Lens.lens (\ListMembers' {nextToken} -> nextToken) (\s@ListMembers' {} a -> s {nextToken = a} :: ListMembers)

-- | Specifies whether to only return associated members or to return all
-- members (including members who haven\'t been invited yet or have been
-- disassociated). Member accounts must have been previously associated
-- with the GuardDuty administrator account using
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_CreateMembers.html Create Members>
-- .
listMembers_onlyAssociated :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Text)
listMembers_onlyAssociated = Lens.lens (\ListMembers' {onlyAssociated} -> onlyAssociated) (\s@ListMembers' {} a -> s {onlyAssociated = a} :: ListMembers)

-- | The unique ID of the detector the member is associated with.
listMembers_detectorId :: Lens.Lens' ListMembers Prelude.Text
listMembers_detectorId = Lens.lens (\ListMembers' {detectorId} -> detectorId) (\s@ListMembers' {} a -> s {detectorId = a} :: ListMembers)

instance Core.AWSPager ListMembers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMembersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMembersResponse_members
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMembers_nextToken
          Lens..~ rs
          Lens.^? listMembersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMembers where
  type AWSResponse ListMembers = ListMembersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMembersResponse'
            Prelude.<$> (x Data..?> "members" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMembers where
  hashWithSalt _salt ListMembers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` onlyAssociated
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData ListMembers where
  rnf ListMembers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf onlyAssociated
      `Prelude.seq` Prelude.rnf detectorId

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
      ["/detector/", Data.toBS detectorId, "/member"]

instance Data.ToQuery ListMembers where
  toQuery ListMembers' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "onlyAssociated" Data.=: onlyAssociated
      ]

-- | /See:/ 'newListMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { -- | A list of members.
    members :: Prelude.Maybe [Member],
    -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
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
-- 'members', 'listMembersResponse_members' - A list of members.
--
-- 'nextToken', 'listMembersResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
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

-- | A list of members.
listMembersResponse_members :: Lens.Lens' ListMembersResponse (Prelude.Maybe [Member])
listMembersResponse_members = Lens.lens (\ListMembersResponse' {members} -> members) (\s@ListMembersResponse' {} a -> s {members = a} :: ListMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listMembersResponse_nextToken :: Lens.Lens' ListMembersResponse (Prelude.Maybe Prelude.Text)
listMembersResponse_nextToken = Lens.lens (\ListMembersResponse' {nextToken} -> nextToken) (\s@ListMembersResponse' {} a -> s {nextToken = a} :: ListMembersResponse)

-- | The response's http status code.
listMembersResponse_httpStatus :: Lens.Lens' ListMembersResponse Prelude.Int
listMembersResponse_httpStatus = Lens.lens (\ListMembersResponse' {httpStatus} -> httpStatus) (\s@ListMembersResponse' {} a -> s {httpStatus = a} :: ListMembersResponse)

instance Prelude.NFData ListMembersResponse where
  rnf ListMembersResponse' {..} =
    Prelude.rnf members
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
