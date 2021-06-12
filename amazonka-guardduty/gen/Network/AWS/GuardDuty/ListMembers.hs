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
-- Module      : Network.AWS.GuardDuty.ListMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details about all member accounts for the current GuardDuty
-- administrator account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListMembers
  ( -- * Creating a Request
    ListMembers (..),
    newListMembers,

    -- * Request Lenses
    listMembers_nextToken,
    listMembers_maxResults,
    listMembers_onlyAssociated,
    listMembers_detectorId,

    -- * Destructuring the Response
    ListMembersResponse (..),
    newListMembersResponse,

    -- * Response Lenses
    listMembersResponse_nextToken,
    listMembersResponse_members,
    listMembersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMembers' smart constructor.
data ListMembers = ListMembers'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill nextToken in the request with the
    -- value of NextToken from the previous response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies whether to only return associated members or to return all
    -- members (including members who haven\'t been invited yet or have been
    -- disassociated).
    onlyAssociated :: Core.Maybe Core.Text,
    -- | The unique ID of the detector the member is associated with.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMembers_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
--
-- 'maxResults', 'listMembers_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
--
-- 'onlyAssociated', 'listMembers_onlyAssociated' - Specifies whether to only return associated members or to return all
-- members (including members who haven\'t been invited yet or have been
-- disassociated).
--
-- 'detectorId', 'listMembers_detectorId' - The unique ID of the detector the member is associated with.
newListMembers ::
  -- | 'detectorId'
  Core.Text ->
  ListMembers
newListMembers pDetectorId_ =
  ListMembers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      onlyAssociated = Core.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill nextToken in the request with the
-- value of NextToken from the previous response to continue listing data.
listMembers_nextToken :: Lens.Lens' ListMembers (Core.Maybe Core.Text)
listMembers_nextToken = Lens.lens (\ListMembers' {nextToken} -> nextToken) (\s@ListMembers' {} a -> s {nextToken = a} :: ListMembers)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 50. The maximum value is 50.
listMembers_maxResults :: Lens.Lens' ListMembers (Core.Maybe Core.Natural)
listMembers_maxResults = Lens.lens (\ListMembers' {maxResults} -> maxResults) (\s@ListMembers' {} a -> s {maxResults = a} :: ListMembers)

-- | Specifies whether to only return associated members or to return all
-- members (including members who haven\'t been invited yet or have been
-- disassociated).
listMembers_onlyAssociated :: Lens.Lens' ListMembers (Core.Maybe Core.Text)
listMembers_onlyAssociated = Lens.lens (\ListMembers' {onlyAssociated} -> onlyAssociated) (\s@ListMembers' {} a -> s {onlyAssociated = a} :: ListMembers)

-- | The unique ID of the detector the member is associated with.
listMembers_detectorId :: Lens.Lens' ListMembers Core.Text
listMembers_detectorId = Lens.lens (\ListMembers' {detectorId} -> detectorId) (\s@ListMembers' {} a -> s {detectorId = a} :: ListMembers)

instance Core.AWSPager ListMembers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMembersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listMembersResponse_members Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMembers_nextToken
          Lens..~ rs
          Lens.^? listMembersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListMembers where
  type AWSResponse ListMembers = ListMembersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMembersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "members" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMembers

instance Core.NFData ListMembers

instance Core.ToHeaders ListMembers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListMembers where
  toPath ListMembers' {..} =
    Core.mconcat
      ["/detector/", Core.toBS detectorId, "/member"]

instance Core.ToQuery ListMembers where
  toQuery ListMembers' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "onlyAssociated" Core.=: onlyAssociated
      ]

-- | /See:/ 'newListMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of members.
    members :: Core.Maybe [Member],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMembersResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'members', 'listMembersResponse_members' - A list of members.
--
-- 'httpStatus', 'listMembersResponse_httpStatus' - The response's http status code.
newListMembersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMembersResponse
newListMembersResponse pHttpStatus_ =
  ListMembersResponse'
    { nextToken = Core.Nothing,
      members = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listMembersResponse_nextToken :: Lens.Lens' ListMembersResponse (Core.Maybe Core.Text)
listMembersResponse_nextToken = Lens.lens (\ListMembersResponse' {nextToken} -> nextToken) (\s@ListMembersResponse' {} a -> s {nextToken = a} :: ListMembersResponse)

-- | A list of members.
listMembersResponse_members :: Lens.Lens' ListMembersResponse (Core.Maybe [Member])
listMembersResponse_members = Lens.lens (\ListMembersResponse' {members} -> members) (\s@ListMembersResponse' {} a -> s {members = a} :: ListMembersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMembersResponse_httpStatus :: Lens.Lens' ListMembersResponse Core.Int
listMembersResponse_httpStatus = Lens.lens (\ListMembersResponse' {httpStatus} -> httpStatus) (\s@ListMembersResponse' {} a -> s {httpStatus = a} :: ListMembersResponse)

instance Core.NFData ListMembersResponse
