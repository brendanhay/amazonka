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
-- Module      : Network.AWS.Kafka.ListClusterOperations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the operations that have been performed on the
-- specified MSK cluster.
--
-- This operation returns paginated results.
module Network.AWS.Kafka.ListClusterOperations
  ( -- * Creating a Request
    ListClusterOperations (..),
    newListClusterOperations,

    -- * Request Lenses
    listClusterOperations_nextToken,
    listClusterOperations_maxResults,
    listClusterOperations_clusterArn,

    -- * Destructuring the Response
    ListClusterOperationsResponse (..),
    newListClusterOperationsResponse,

    -- * Response Lenses
    listClusterOperationsResponse_clusterOperationInfoList,
    listClusterOperationsResponse_nextToken,
    listClusterOperationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kafka.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListClusterOperations' smart constructor.
data ListClusterOperations = ListClusterOperations'
  { -- | The paginated results marker. When the result of the operation is
    -- truncated, the call returns NextToken in the response. To get the next
    -- batch, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response. If there are
    -- more results, the response includes a NextToken parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusterOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listClusterOperations_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
--
-- 'maxResults', 'listClusterOperations_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
--
-- 'clusterArn', 'listClusterOperations_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
newListClusterOperations ::
  -- | 'clusterArn'
  Prelude.Text ->
  ListClusterOperations
newListClusterOperations pClusterArn_ =
  ListClusterOperations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      clusterArn = pClusterArn_
    }

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listClusterOperations_nextToken :: Lens.Lens' ListClusterOperations (Prelude.Maybe Prelude.Text)
listClusterOperations_nextToken = Lens.lens (\ListClusterOperations' {nextToken} -> nextToken) (\s@ListClusterOperations' {} a -> s {nextToken = a} :: ListClusterOperations)

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listClusterOperations_maxResults :: Lens.Lens' ListClusterOperations (Prelude.Maybe Prelude.Natural)
listClusterOperations_maxResults = Lens.lens (\ListClusterOperations' {maxResults} -> maxResults) (\s@ListClusterOperations' {} a -> s {maxResults = a} :: ListClusterOperations)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
listClusterOperations_clusterArn :: Lens.Lens' ListClusterOperations Prelude.Text
listClusterOperations_clusterArn = Lens.lens (\ListClusterOperations' {clusterArn} -> clusterArn) (\s@ListClusterOperations' {} a -> s {clusterArn = a} :: ListClusterOperations)

instance Core.AWSPager ListClusterOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClusterOperationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClusterOperationsResponse_clusterOperationInfoList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listClusterOperations_nextToken
          Lens..~ rs
          Lens.^? listClusterOperationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListClusterOperations where
  type
    AWSResponse ListClusterOperations =
      ListClusterOperationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClusterOperationsResponse'
            Prelude.<$> ( x Core..?> "clusterOperationInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusterOperations

instance Prelude.NFData ListClusterOperations

instance Core.ToHeaders ListClusterOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListClusterOperations where
  toPath ListClusterOperations' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Core.toBS clusterArn,
        "/operations"
      ]

instance Core.ToQuery ListClusterOperations where
  toQuery ListClusterOperations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListClusterOperationsResponse' smart constructor.
data ListClusterOperationsResponse = ListClusterOperationsResponse'
  { -- | An array of cluster operation information objects.
    clusterOperationInfoList :: Prelude.Maybe [ClusterOperationInfo],
    -- | If the response of ListClusterOperations is truncated, it returns a
    -- NextToken in the response. This Nexttoken should be sent in the
    -- subsequent request to ListClusterOperations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusterOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterOperationInfoList', 'listClusterOperationsResponse_clusterOperationInfoList' - An array of cluster operation information objects.
--
-- 'nextToken', 'listClusterOperationsResponse_nextToken' - If the response of ListClusterOperations is truncated, it returns a
-- NextToken in the response. This Nexttoken should be sent in the
-- subsequent request to ListClusterOperations.
--
-- 'httpStatus', 'listClusterOperationsResponse_httpStatus' - The response's http status code.
newListClusterOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClusterOperationsResponse
newListClusterOperationsResponse pHttpStatus_ =
  ListClusterOperationsResponse'
    { clusterOperationInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of cluster operation information objects.
listClusterOperationsResponse_clusterOperationInfoList :: Lens.Lens' ListClusterOperationsResponse (Prelude.Maybe [ClusterOperationInfo])
listClusterOperationsResponse_clusterOperationInfoList = Lens.lens (\ListClusterOperationsResponse' {clusterOperationInfoList} -> clusterOperationInfoList) (\s@ListClusterOperationsResponse' {} a -> s {clusterOperationInfoList = a} :: ListClusterOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response of ListClusterOperations is truncated, it returns a
-- NextToken in the response. This Nexttoken should be sent in the
-- subsequent request to ListClusterOperations.
listClusterOperationsResponse_nextToken :: Lens.Lens' ListClusterOperationsResponse (Prelude.Maybe Prelude.Text)
listClusterOperationsResponse_nextToken = Lens.lens (\ListClusterOperationsResponse' {nextToken} -> nextToken) (\s@ListClusterOperationsResponse' {} a -> s {nextToken = a} :: ListClusterOperationsResponse)

-- | The response's http status code.
listClusterOperationsResponse_httpStatus :: Lens.Lens' ListClusterOperationsResponse Prelude.Int
listClusterOperationsResponse_httpStatus = Lens.lens (\ListClusterOperationsResponse' {httpStatus} -> httpStatus) (\s@ListClusterOperationsResponse' {} a -> s {httpStatus = a} :: ListClusterOperationsResponse)

instance Prelude.NFData ListClusterOperationsResponse
