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
-- Module      : Network.AWS.Kendra.ListGroupsOlderThanOrderingId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of groups that are mapped to users before a given
-- ordering or timestamp identifier.
module Network.AWS.Kendra.ListGroupsOlderThanOrderingId
  ( -- * Creating a Request
    ListGroupsOlderThanOrderingId (..),
    newListGroupsOlderThanOrderingId,

    -- * Request Lenses
    listGroupsOlderThanOrderingId_dataSourceId,
    listGroupsOlderThanOrderingId_nextToken,
    listGroupsOlderThanOrderingId_maxResults,
    listGroupsOlderThanOrderingId_indexId,
    listGroupsOlderThanOrderingId_orderingId,

    -- * Destructuring the Response
    ListGroupsOlderThanOrderingIdResponse (..),
    newListGroupsOlderThanOrderingIdResponse,

    -- * Response Lenses
    listGroupsOlderThanOrderingIdResponse_groupsSummaries,
    listGroupsOlderThanOrderingIdResponse_nextToken,
    listGroupsOlderThanOrderingIdResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGroupsOlderThanOrderingId' smart constructor.
data ListGroupsOlderThanOrderingId = ListGroupsOlderThanOrderingId'
  { -- | The identifier of the data source for getting a list of groups mapped to
    -- users before a given ordering timestamp identifier.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of groups that
    -- are mapped to users before a given ordering or timestamp identifier.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned groups that are mapped to users before a
    -- given ordering or timestamp identifier.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the index for getting a list of groups mapped to users
    -- before a given ordering or timestamp identifier.
    indexId :: Prelude.Text,
    -- | The timestamp identifier used for the latest @PUT@ or @DELETE@ action
    -- for mapping users to their groups.
    orderingId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupsOlderThanOrderingId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'listGroupsOlderThanOrderingId_dataSourceId' - The identifier of the data source for getting a list of groups mapped to
-- users before a given ordering timestamp identifier.
--
-- 'nextToken', 'listGroupsOlderThanOrderingId_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of groups that
-- are mapped to users before a given ordering or timestamp identifier.
--
-- 'maxResults', 'listGroupsOlderThanOrderingId_maxResults' - The maximum number of returned groups that are mapped to users before a
-- given ordering or timestamp identifier.
--
-- 'indexId', 'listGroupsOlderThanOrderingId_indexId' - The identifier of the index for getting a list of groups mapped to users
-- before a given ordering or timestamp identifier.
--
-- 'orderingId', 'listGroupsOlderThanOrderingId_orderingId' - The timestamp identifier used for the latest @PUT@ or @DELETE@ action
-- for mapping users to their groups.
newListGroupsOlderThanOrderingId ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'orderingId'
  Prelude.Natural ->
  ListGroupsOlderThanOrderingId
newListGroupsOlderThanOrderingId
  pIndexId_
  pOrderingId_ =
    ListGroupsOlderThanOrderingId'
      { dataSourceId =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        indexId = pIndexId_,
        orderingId = pOrderingId_
      }

-- | The identifier of the data source for getting a list of groups mapped to
-- users before a given ordering timestamp identifier.
listGroupsOlderThanOrderingId_dataSourceId :: Lens.Lens' ListGroupsOlderThanOrderingId (Prelude.Maybe Prelude.Text)
listGroupsOlderThanOrderingId_dataSourceId = Lens.lens (\ListGroupsOlderThanOrderingId' {dataSourceId} -> dataSourceId) (\s@ListGroupsOlderThanOrderingId' {} a -> s {dataSourceId = a} :: ListGroupsOlderThanOrderingId)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of groups that
-- are mapped to users before a given ordering or timestamp identifier.
listGroupsOlderThanOrderingId_nextToken :: Lens.Lens' ListGroupsOlderThanOrderingId (Prelude.Maybe Prelude.Text)
listGroupsOlderThanOrderingId_nextToken = Lens.lens (\ListGroupsOlderThanOrderingId' {nextToken} -> nextToken) (\s@ListGroupsOlderThanOrderingId' {} a -> s {nextToken = a} :: ListGroupsOlderThanOrderingId)

-- | The maximum number of returned groups that are mapped to users before a
-- given ordering or timestamp identifier.
listGroupsOlderThanOrderingId_maxResults :: Lens.Lens' ListGroupsOlderThanOrderingId (Prelude.Maybe Prelude.Natural)
listGroupsOlderThanOrderingId_maxResults = Lens.lens (\ListGroupsOlderThanOrderingId' {maxResults} -> maxResults) (\s@ListGroupsOlderThanOrderingId' {} a -> s {maxResults = a} :: ListGroupsOlderThanOrderingId)

-- | The identifier of the index for getting a list of groups mapped to users
-- before a given ordering or timestamp identifier.
listGroupsOlderThanOrderingId_indexId :: Lens.Lens' ListGroupsOlderThanOrderingId Prelude.Text
listGroupsOlderThanOrderingId_indexId = Lens.lens (\ListGroupsOlderThanOrderingId' {indexId} -> indexId) (\s@ListGroupsOlderThanOrderingId' {} a -> s {indexId = a} :: ListGroupsOlderThanOrderingId)

-- | The timestamp identifier used for the latest @PUT@ or @DELETE@ action
-- for mapping users to their groups.
listGroupsOlderThanOrderingId_orderingId :: Lens.Lens' ListGroupsOlderThanOrderingId Prelude.Natural
listGroupsOlderThanOrderingId_orderingId = Lens.lens (\ListGroupsOlderThanOrderingId' {orderingId} -> orderingId) (\s@ListGroupsOlderThanOrderingId' {} a -> s {orderingId = a} :: ListGroupsOlderThanOrderingId)

instance
  Core.AWSRequest
    ListGroupsOlderThanOrderingId
  where
  type
    AWSResponse ListGroupsOlderThanOrderingId =
      ListGroupsOlderThanOrderingIdResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupsOlderThanOrderingIdResponse'
            Prelude.<$> ( x Core..?> "GroupsSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListGroupsOlderThanOrderingId

instance Prelude.NFData ListGroupsOlderThanOrderingId

instance Core.ToHeaders ListGroupsOlderThanOrderingId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.ListGroupsOlderThanOrderingId" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListGroupsOlderThanOrderingId where
  toJSON ListGroupsOlderThanOrderingId' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataSourceId" Core..=) Prelude.<$> dataSourceId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("OrderingId" Core..= orderingId)
          ]
      )

instance Core.ToPath ListGroupsOlderThanOrderingId where
  toPath = Prelude.const "/"

instance Core.ToQuery ListGroupsOlderThanOrderingId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGroupsOlderThanOrderingIdResponse' smart constructor.
data ListGroupsOlderThanOrderingIdResponse = ListGroupsOlderThanOrderingIdResponse'
  { -- | Summary information for list of groups that are mapped to users before a
    -- given ordering or timestamp identifier.
    groupsSummaries :: Prelude.Maybe [GroupSummary],
    -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of groups
    -- that are mapped to users before a given ordering or timestamp
    -- identifier.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupsOlderThanOrderingIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupsSummaries', 'listGroupsOlderThanOrderingIdResponse_groupsSummaries' - Summary information for list of groups that are mapped to users before a
-- given ordering or timestamp identifier.
--
-- 'nextToken', 'listGroupsOlderThanOrderingIdResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of groups
-- that are mapped to users before a given ordering or timestamp
-- identifier.
--
-- 'httpStatus', 'listGroupsOlderThanOrderingIdResponse_httpStatus' - The response's http status code.
newListGroupsOlderThanOrderingIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupsOlderThanOrderingIdResponse
newListGroupsOlderThanOrderingIdResponse pHttpStatus_ =
  ListGroupsOlderThanOrderingIdResponse'
    { groupsSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information for list of groups that are mapped to users before a
-- given ordering or timestamp identifier.
listGroupsOlderThanOrderingIdResponse_groupsSummaries :: Lens.Lens' ListGroupsOlderThanOrderingIdResponse (Prelude.Maybe [GroupSummary])
listGroupsOlderThanOrderingIdResponse_groupsSummaries = Lens.lens (\ListGroupsOlderThanOrderingIdResponse' {groupsSummaries} -> groupsSummaries) (\s@ListGroupsOlderThanOrderingIdResponse' {} a -> s {groupsSummaries = a} :: ListGroupsOlderThanOrderingIdResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of groups
-- that are mapped to users before a given ordering or timestamp
-- identifier.
listGroupsOlderThanOrderingIdResponse_nextToken :: Lens.Lens' ListGroupsOlderThanOrderingIdResponse (Prelude.Maybe Prelude.Text)
listGroupsOlderThanOrderingIdResponse_nextToken = Lens.lens (\ListGroupsOlderThanOrderingIdResponse' {nextToken} -> nextToken) (\s@ListGroupsOlderThanOrderingIdResponse' {} a -> s {nextToken = a} :: ListGroupsOlderThanOrderingIdResponse)

-- | The response's http status code.
listGroupsOlderThanOrderingIdResponse_httpStatus :: Lens.Lens' ListGroupsOlderThanOrderingIdResponse Prelude.Int
listGroupsOlderThanOrderingIdResponse_httpStatus = Lens.lens (\ListGroupsOlderThanOrderingIdResponse' {httpStatus} -> httpStatus) (\s@ListGroupsOlderThanOrderingIdResponse' {} a -> s {httpStatus = a} :: ListGroupsOlderThanOrderingIdResponse)

instance
  Prelude.NFData
    ListGroupsOlderThanOrderingIdResponse
