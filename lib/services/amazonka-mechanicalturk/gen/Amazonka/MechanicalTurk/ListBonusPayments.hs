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
-- Module      : Amazonka.MechanicalTurk.ListBonusPayments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListBonusPayments@ operation retrieves the amounts of bonuses you
-- have paid to Workers for a given HIT or assignment.
--
-- This operation returns paginated results.
module Amazonka.MechanicalTurk.ListBonusPayments
  ( -- * Creating a Request
    ListBonusPayments (..),
    newListBonusPayments,

    -- * Request Lenses
    listBonusPayments_nextToken,
    listBonusPayments_hITId,
    listBonusPayments_maxResults,
    listBonusPayments_assignmentId,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBonusPayments' smart constructor.
data ListBonusPayments = ListBonusPayments'
  { -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the HIT associated with the bonus payments to retrieve. If not
    -- specified, all bonus payments for all assignments for the given HIT are
    -- returned. Either the HITId parameter or the AssignmentId parameter must
    -- be specified
    hITId :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the assignment associated with the bonus payments to retrieve.
    -- If specified, only bonus payments for the given assignment are returned.
    -- Either the HITId parameter or the AssignmentId parameter must be
    -- specified
    assignmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'hITId', 'listBonusPayments_hITId' - The ID of the HIT associated with the bonus payments to retrieve. If not
-- specified, all bonus payments for all assignments for the given HIT are
-- returned. Either the HITId parameter or the AssignmentId parameter must
-- be specified
--
-- 'maxResults', 'listBonusPayments_maxResults' - Undocumented member.
--
-- 'assignmentId', 'listBonusPayments_assignmentId' - The ID of the assignment associated with the bonus payments to retrieve.
-- If specified, only bonus payments for the given assignment are returned.
-- Either the HITId parameter or the AssignmentId parameter must be
-- specified
newListBonusPayments ::
  ListBonusPayments
newListBonusPayments =
  ListBonusPayments'
    { nextToken = Prelude.Nothing,
      hITId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      assignmentId = Prelude.Nothing
    }

-- | Pagination token
listBonusPayments_nextToken :: Lens.Lens' ListBonusPayments (Prelude.Maybe Prelude.Text)
listBonusPayments_nextToken = Lens.lens (\ListBonusPayments' {nextToken} -> nextToken) (\s@ListBonusPayments' {} a -> s {nextToken = a} :: ListBonusPayments)

-- | The ID of the HIT associated with the bonus payments to retrieve. If not
-- specified, all bonus payments for all assignments for the given HIT are
-- returned. Either the HITId parameter or the AssignmentId parameter must
-- be specified
listBonusPayments_hITId :: Lens.Lens' ListBonusPayments (Prelude.Maybe Prelude.Text)
listBonusPayments_hITId = Lens.lens (\ListBonusPayments' {hITId} -> hITId) (\s@ListBonusPayments' {} a -> s {hITId = a} :: ListBonusPayments)

-- | Undocumented member.
listBonusPayments_maxResults :: Lens.Lens' ListBonusPayments (Prelude.Maybe Prelude.Natural)
listBonusPayments_maxResults = Lens.lens (\ListBonusPayments' {maxResults} -> maxResults) (\s@ListBonusPayments' {} a -> s {maxResults = a} :: ListBonusPayments)

-- | The ID of the assignment associated with the bonus payments to retrieve.
-- If specified, only bonus payments for the given assignment are returned.
-- Either the HITId parameter or the AssignmentId parameter must be
-- specified
listBonusPayments_assignmentId :: Lens.Lens' ListBonusPayments (Prelude.Maybe Prelude.Text)
listBonusPayments_assignmentId = Lens.lens (\ListBonusPayments' {assignmentId} -> assignmentId) (\s@ListBonusPayments' {} a -> s {assignmentId = a} :: ListBonusPayments)

instance Core.AWSPager ListBonusPayments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBonusPaymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBonusPaymentsResponse_bonusPayments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBonusPayments_nextToken
          Lens..~ rs
          Lens.^? listBonusPaymentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBonusPayments where
  type
    AWSResponse ListBonusPayments =
      ListBonusPaymentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBonusPaymentsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "NumResults")
            Prelude.<*> (x Core..?> "BonusPayments" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBonusPayments where
  hashWithSalt _salt ListBonusPayments' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` hITId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` assignmentId

instance Prelude.NFData ListBonusPayments where
  rnf ListBonusPayments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf hITId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf assignmentId

instance Core.ToHeaders ListBonusPayments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListBonusPayments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBonusPayments where
  toJSON ListBonusPayments' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("HITId" Core..=) Prelude.<$> hITId,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("AssignmentId" Core..=) Prelude.<$> assignmentId
          ]
      )

instance Core.ToPath ListBonusPayments where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBonusPayments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBonusPaymentsResponse' smart constructor.
data ListBonusPaymentsResponse = ListBonusPaymentsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of bonus payments on this page in the filtered results list,
    -- equivalent to the number of bonus payments being returned by this call.
    numResults :: Prelude.Maybe Prelude.Int,
    -- | A successful request to the ListBonusPayments operation returns a list
    -- of BonusPayment objects.
    bonusPayments :: Prelude.Maybe [BonusPayment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListBonusPaymentsResponse
newListBonusPaymentsResponse pHttpStatus_ =
  ListBonusPaymentsResponse'
    { nextToken =
        Prelude.Nothing,
      numResults = Prelude.Nothing,
      bonusPayments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listBonusPaymentsResponse_nextToken :: Lens.Lens' ListBonusPaymentsResponse (Prelude.Maybe Prelude.Text)
listBonusPaymentsResponse_nextToken = Lens.lens (\ListBonusPaymentsResponse' {nextToken} -> nextToken) (\s@ListBonusPaymentsResponse' {} a -> s {nextToken = a} :: ListBonusPaymentsResponse)

-- | The number of bonus payments on this page in the filtered results list,
-- equivalent to the number of bonus payments being returned by this call.
listBonusPaymentsResponse_numResults :: Lens.Lens' ListBonusPaymentsResponse (Prelude.Maybe Prelude.Int)
listBonusPaymentsResponse_numResults = Lens.lens (\ListBonusPaymentsResponse' {numResults} -> numResults) (\s@ListBonusPaymentsResponse' {} a -> s {numResults = a} :: ListBonusPaymentsResponse)

-- | A successful request to the ListBonusPayments operation returns a list
-- of BonusPayment objects.
listBonusPaymentsResponse_bonusPayments :: Lens.Lens' ListBonusPaymentsResponse (Prelude.Maybe [BonusPayment])
listBonusPaymentsResponse_bonusPayments = Lens.lens (\ListBonusPaymentsResponse' {bonusPayments} -> bonusPayments) (\s@ListBonusPaymentsResponse' {} a -> s {bonusPayments = a} :: ListBonusPaymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBonusPaymentsResponse_httpStatus :: Lens.Lens' ListBonusPaymentsResponse Prelude.Int
listBonusPaymentsResponse_httpStatus = Lens.lens (\ListBonusPaymentsResponse' {httpStatus} -> httpStatus) (\s@ListBonusPaymentsResponse' {} a -> s {httpStatus = a} :: ListBonusPaymentsResponse)

instance Prelude.NFData ListBonusPaymentsResponse where
  rnf ListBonusPaymentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf bonusPayments
      `Prelude.seq` Prelude.rnf httpStatus
