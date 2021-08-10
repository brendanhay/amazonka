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
-- Module      : Network.AWS.Connect.ListHoursOfOperations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the hours of operation for the specified
-- Amazon Connect instance.
--
-- For more information about hours of operation, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/set-hours-operation.html Set the Hours of Operation for a Queue>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListHoursOfOperations
  ( -- * Creating a Request
    ListHoursOfOperations (..),
    newListHoursOfOperations,

    -- * Request Lenses
    listHoursOfOperations_nextToken,
    listHoursOfOperations_maxResults,
    listHoursOfOperations_instanceId,

    -- * Destructuring the Response
    ListHoursOfOperationsResponse (..),
    newListHoursOfOperationsResponse,

    -- * Response Lenses
    listHoursOfOperationsResponse_nextToken,
    listHoursOfOperationsResponse_hoursOfOperationSummaryList,
    listHoursOfOperationsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHoursOfOperations' smart constructor.
data ListHoursOfOperations = ListHoursOfOperations'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHoursOfOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHoursOfOperations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listHoursOfOperations_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listHoursOfOperations_instanceId' - The identifier of the Amazon Connect instance.
newListHoursOfOperations ::
  -- | 'instanceId'
  Prelude.Text ->
  ListHoursOfOperations
newListHoursOfOperations pInstanceId_ =
  ListHoursOfOperations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listHoursOfOperations_nextToken :: Lens.Lens' ListHoursOfOperations (Prelude.Maybe Prelude.Text)
listHoursOfOperations_nextToken = Lens.lens (\ListHoursOfOperations' {nextToken} -> nextToken) (\s@ListHoursOfOperations' {} a -> s {nextToken = a} :: ListHoursOfOperations)

-- | The maximum number of results to return per page.
listHoursOfOperations_maxResults :: Lens.Lens' ListHoursOfOperations (Prelude.Maybe Prelude.Natural)
listHoursOfOperations_maxResults = Lens.lens (\ListHoursOfOperations' {maxResults} -> maxResults) (\s@ListHoursOfOperations' {} a -> s {maxResults = a} :: ListHoursOfOperations)

-- | The identifier of the Amazon Connect instance.
listHoursOfOperations_instanceId :: Lens.Lens' ListHoursOfOperations Prelude.Text
listHoursOfOperations_instanceId = Lens.lens (\ListHoursOfOperations' {instanceId} -> instanceId) (\s@ListHoursOfOperations' {} a -> s {instanceId = a} :: ListHoursOfOperations)

instance Core.AWSPager ListHoursOfOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHoursOfOperationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listHoursOfOperationsResponse_hoursOfOperationSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listHoursOfOperations_nextToken
          Lens..~ rs
          Lens.^? listHoursOfOperationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListHoursOfOperations where
  type
    AWSResponse ListHoursOfOperations =
      ListHoursOfOperationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHoursOfOperationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "HoursOfOperationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHoursOfOperations

instance Prelude.NFData ListHoursOfOperations

instance Core.ToHeaders ListHoursOfOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListHoursOfOperations where
  toPath ListHoursOfOperations' {..} =
    Prelude.mconcat
      [ "/hours-of-operations-summary/",
        Core.toBS instanceId
      ]

instance Core.ToQuery ListHoursOfOperations where
  toQuery ListHoursOfOperations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListHoursOfOperationsResponse' smart constructor.
data ListHoursOfOperationsResponse = ListHoursOfOperationsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the hours of operation.
    hoursOfOperationSummaryList :: Prelude.Maybe [HoursOfOperationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHoursOfOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHoursOfOperationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'hoursOfOperationSummaryList', 'listHoursOfOperationsResponse_hoursOfOperationSummaryList' - Information about the hours of operation.
--
-- 'httpStatus', 'listHoursOfOperationsResponse_httpStatus' - The response's http status code.
newListHoursOfOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHoursOfOperationsResponse
newListHoursOfOperationsResponse pHttpStatus_ =
  ListHoursOfOperationsResponse'
    { nextToken =
        Prelude.Nothing,
      hoursOfOperationSummaryList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listHoursOfOperationsResponse_nextToken :: Lens.Lens' ListHoursOfOperationsResponse (Prelude.Maybe Prelude.Text)
listHoursOfOperationsResponse_nextToken = Lens.lens (\ListHoursOfOperationsResponse' {nextToken} -> nextToken) (\s@ListHoursOfOperationsResponse' {} a -> s {nextToken = a} :: ListHoursOfOperationsResponse)

-- | Information about the hours of operation.
listHoursOfOperationsResponse_hoursOfOperationSummaryList :: Lens.Lens' ListHoursOfOperationsResponse (Prelude.Maybe [HoursOfOperationSummary])
listHoursOfOperationsResponse_hoursOfOperationSummaryList = Lens.lens (\ListHoursOfOperationsResponse' {hoursOfOperationSummaryList} -> hoursOfOperationSummaryList) (\s@ListHoursOfOperationsResponse' {} a -> s {hoursOfOperationSummaryList = a} :: ListHoursOfOperationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listHoursOfOperationsResponse_httpStatus :: Lens.Lens' ListHoursOfOperationsResponse Prelude.Int
listHoursOfOperationsResponse_httpStatus = Lens.lens (\ListHoursOfOperationsResponse' {httpStatus} -> httpStatus) (\s@ListHoursOfOperationsResponse' {} a -> s {httpStatus = a} :: ListHoursOfOperationsResponse)

instance Prelude.NFData ListHoursOfOperationsResponse
