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
-- Module      : Network.AWS.MechanicalTurk.ListBonusPayments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListBonusPayments@ operation retrieves the amounts of bonuses you
-- have paid to Workers for a given HIT or assignment.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListBonusPayments
  ( -- * Creating a Request
    ListBonusPayments (..),
    newListBonusPayments,

    -- * Request Lenses
    listBonusPayments_nextToken,
    listBonusPayments_assignmentId,
    listBonusPayments_maxResults,
    listBonusPayments_hITId,

    -- * Destructuring the Response
    ListBonusPaymentsResponse (..),
    newListBonusPaymentsResponse,

    -- * Response Lenses
    listBonusPaymentsResponse_nextToken,
    listBonusPaymentsResponse_numResults,
    listBonusPaymentsResponse_bonusPayments,
    listBonusPaymentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBonusPayments' smart constructor.
data ListBonusPayments = ListBonusPayments'
  { -- | Pagination token
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the assignment associated with the bonus payments to retrieve.
    -- If specified, only bonus payments for the given assignment are returned.
    -- Either the HITId parameter or the AssignmentId parameter must be
    -- specified
    assignmentId :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the HIT associated with the bonus payments to retrieve. If not
    -- specified, all bonus payments for all assignments for the given HIT are
    -- returned. Either the HITId parameter or the AssignmentId parameter must
    -- be specified
    hITId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBonusPayments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBonusPayments_nextToken' - Pagination token
--
-- 'assignmentId', 'listBonusPayments_assignmentId' - The ID of the assignment associated with the bonus payments to retrieve.
-- If specified, only bonus payments for the given assignment are returned.
-- Either the HITId parameter or the AssignmentId parameter must be
-- specified
--
-- 'maxResults', 'listBonusPayments_maxResults' - Undocumented member.
--
-- 'hITId', 'listBonusPayments_hITId' - The ID of the HIT associated with the bonus payments to retrieve. If not
-- specified, all bonus payments for all assignments for the given HIT are
-- returned. Either the HITId parameter or the AssignmentId parameter must
-- be specified
newListBonusPayments ::
  ListBonusPayments
newListBonusPayments =
  ListBonusPayments'
    { nextToken = Core.Nothing,
      assignmentId = Core.Nothing,
      maxResults = Core.Nothing,
      hITId = Core.Nothing
    }

-- | Pagination token
listBonusPayments_nextToken :: Lens.Lens' ListBonusPayments (Core.Maybe Core.Text)
listBonusPayments_nextToken = Lens.lens (\ListBonusPayments' {nextToken} -> nextToken) (\s@ListBonusPayments' {} a -> s {nextToken = a} :: ListBonusPayments)

-- | The ID of the assignment associated with the bonus payments to retrieve.
-- If specified, only bonus payments for the given assignment are returned.
-- Either the HITId parameter or the AssignmentId parameter must be
-- specified
listBonusPayments_assignmentId :: Lens.Lens' ListBonusPayments (Core.Maybe Core.Text)
listBonusPayments_assignmentId = Lens.lens (\ListBonusPayments' {assignmentId} -> assignmentId) (\s@ListBonusPayments' {} a -> s {assignmentId = a} :: ListBonusPayments)

-- | Undocumented member.
listBonusPayments_maxResults :: Lens.Lens' ListBonusPayments (Core.Maybe Core.Natural)
listBonusPayments_maxResults = Lens.lens (\ListBonusPayments' {maxResults} -> maxResults) (\s@ListBonusPayments' {} a -> s {maxResults = a} :: ListBonusPayments)

-- | The ID of the HIT associated with the bonus payments to retrieve. If not
-- specified, all bonus payments for all assignments for the given HIT are
-- returned. Either the HITId parameter or the AssignmentId parameter must
-- be specified
listBonusPayments_hITId :: Lens.Lens' ListBonusPayments (Core.Maybe Core.Text)
listBonusPayments_hITId = Lens.lens (\ListBonusPayments' {hITId} -> hITId) (\s@ListBonusPayments' {} a -> s {hITId = a} :: ListBonusPayments)

instance Core.AWSPager ListBonusPayments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBonusPaymentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBonusPaymentsResponse_bonusPayments
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBonusPayments_nextToken
          Lens..~ rs
          Lens.^? listBonusPaymentsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListBonusPayments where
  type
    AWSResponse ListBonusPayments =
      ListBonusPaymentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBonusPaymentsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "NumResults")
            Core.<*> (x Core..?> "BonusPayments" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBonusPayments

instance Core.NFData ListBonusPayments

instance Core.ToHeaders ListBonusPayments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListBonusPayments" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBonusPayments where
  toJSON ListBonusPayments' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("AssignmentId" Core..=) Core.<$> assignmentId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("HITId" Core..=) Core.<$> hITId
          ]
      )

instance Core.ToPath ListBonusPayments where
  toPath = Core.const "/"

instance Core.ToQuery ListBonusPayments where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBonusPaymentsResponse' smart constructor.
data ListBonusPaymentsResponse = ListBonusPaymentsResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The number of bonus payments on this page in the filtered results list,
    -- equivalent to the number of bonus payments being returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | A successful request to the ListBonusPayments operation returns a list
    -- of BonusPayment objects.
    bonusPayments :: Core.Maybe [BonusPayment],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBonusPaymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBonusPaymentsResponse_nextToken' - Undocumented member.
--
-- 'numResults', 'listBonusPaymentsResponse_numResults' - The number of bonus payments on this page in the filtered results list,
-- equivalent to the number of bonus payments being returned by this call.
--
-- 'bonusPayments', 'listBonusPaymentsResponse_bonusPayments' - A successful request to the ListBonusPayments operation returns a list
-- of BonusPayment objects.
--
-- 'httpStatus', 'listBonusPaymentsResponse_httpStatus' - The response's http status code.
newListBonusPaymentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBonusPaymentsResponse
newListBonusPaymentsResponse pHttpStatus_ =
  ListBonusPaymentsResponse'
    { nextToken =
        Core.Nothing,
      numResults = Core.Nothing,
      bonusPayments = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listBonusPaymentsResponse_nextToken :: Lens.Lens' ListBonusPaymentsResponse (Core.Maybe Core.Text)
listBonusPaymentsResponse_nextToken = Lens.lens (\ListBonusPaymentsResponse' {nextToken} -> nextToken) (\s@ListBonusPaymentsResponse' {} a -> s {nextToken = a} :: ListBonusPaymentsResponse)

-- | The number of bonus payments on this page in the filtered results list,
-- equivalent to the number of bonus payments being returned by this call.
listBonusPaymentsResponse_numResults :: Lens.Lens' ListBonusPaymentsResponse (Core.Maybe Core.Int)
listBonusPaymentsResponse_numResults = Lens.lens (\ListBonusPaymentsResponse' {numResults} -> numResults) (\s@ListBonusPaymentsResponse' {} a -> s {numResults = a} :: ListBonusPaymentsResponse)

-- | A successful request to the ListBonusPayments operation returns a list
-- of BonusPayment objects.
listBonusPaymentsResponse_bonusPayments :: Lens.Lens' ListBonusPaymentsResponse (Core.Maybe [BonusPayment])
listBonusPaymentsResponse_bonusPayments = Lens.lens (\ListBonusPaymentsResponse' {bonusPayments} -> bonusPayments) (\s@ListBonusPaymentsResponse' {} a -> s {bonusPayments = a} :: ListBonusPaymentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBonusPaymentsResponse_httpStatus :: Lens.Lens' ListBonusPaymentsResponse Core.Int
listBonusPaymentsResponse_httpStatus = Lens.lens (\ListBonusPaymentsResponse' {httpStatus} -> httpStatus) (\s@ListBonusPaymentsResponse' {} a -> s {httpStatus = a} :: ListBonusPaymentsResponse)

instance Core.NFData ListBonusPaymentsResponse
