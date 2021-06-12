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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHoursOfOperations' smart constructor.
data ListHoursOfOperations = ListHoursOfOperations'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListHoursOfOperations
newListHoursOfOperations pInstanceId_ =
  ListHoursOfOperations'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listHoursOfOperations_nextToken :: Lens.Lens' ListHoursOfOperations (Core.Maybe Core.Text)
listHoursOfOperations_nextToken = Lens.lens (\ListHoursOfOperations' {nextToken} -> nextToken) (\s@ListHoursOfOperations' {} a -> s {nextToken = a} :: ListHoursOfOperations)

-- | The maximum number of results to return per page.
listHoursOfOperations_maxResults :: Lens.Lens' ListHoursOfOperations (Core.Maybe Core.Natural)
listHoursOfOperations_maxResults = Lens.lens (\ListHoursOfOperations' {maxResults} -> maxResults) (\s@ListHoursOfOperations' {} a -> s {maxResults = a} :: ListHoursOfOperations)

-- | The identifier of the Amazon Connect instance.
listHoursOfOperations_instanceId :: Lens.Lens' ListHoursOfOperations Core.Text
listHoursOfOperations_instanceId = Lens.lens (\ListHoursOfOperations' {instanceId} -> instanceId) (\s@ListHoursOfOperations' {} a -> s {instanceId = a} :: ListHoursOfOperations)

instance Core.AWSPager ListHoursOfOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHoursOfOperationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listHoursOfOperationsResponse_hoursOfOperationSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHoursOfOperations_nextToken
          Lens..~ rs
          Lens.^? listHoursOfOperationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListHoursOfOperations where
  type
    AWSResponse ListHoursOfOperations =
      ListHoursOfOperationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHoursOfOperationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "HoursOfOperationSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListHoursOfOperations

instance Core.NFData ListHoursOfOperations

instance Core.ToHeaders ListHoursOfOperations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListHoursOfOperations where
  toPath ListHoursOfOperations' {..} =
    Core.mconcat
      [ "/hours-of-operations-summary/",
        Core.toBS instanceId
      ]

instance Core.ToQuery ListHoursOfOperations where
  toQuery ListHoursOfOperations' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListHoursOfOperationsResponse' smart constructor.
data ListHoursOfOperationsResponse = ListHoursOfOperationsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the hours of operation.
    hoursOfOperationSummaryList :: Core.Maybe [HoursOfOperationSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListHoursOfOperationsResponse
newListHoursOfOperationsResponse pHttpStatus_ =
  ListHoursOfOperationsResponse'
    { nextToken =
        Core.Nothing,
      hoursOfOperationSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listHoursOfOperationsResponse_nextToken :: Lens.Lens' ListHoursOfOperationsResponse (Core.Maybe Core.Text)
listHoursOfOperationsResponse_nextToken = Lens.lens (\ListHoursOfOperationsResponse' {nextToken} -> nextToken) (\s@ListHoursOfOperationsResponse' {} a -> s {nextToken = a} :: ListHoursOfOperationsResponse)

-- | Information about the hours of operation.
listHoursOfOperationsResponse_hoursOfOperationSummaryList :: Lens.Lens' ListHoursOfOperationsResponse (Core.Maybe [HoursOfOperationSummary])
listHoursOfOperationsResponse_hoursOfOperationSummaryList = Lens.lens (\ListHoursOfOperationsResponse' {hoursOfOperationSummaryList} -> hoursOfOperationSummaryList) (\s@ListHoursOfOperationsResponse' {} a -> s {hoursOfOperationSummaryList = a} :: ListHoursOfOperationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listHoursOfOperationsResponse_httpStatus :: Lens.Lens' ListHoursOfOperationsResponse Core.Int
listHoursOfOperationsResponse_httpStatus = Lens.lens (\ListHoursOfOperationsResponse' {httpStatus} -> httpStatus) (\s@ListHoursOfOperationsResponse' {} a -> s {httpStatus = a} :: ListHoursOfOperationsResponse)

instance Core.NFData ListHoursOfOperationsResponse
