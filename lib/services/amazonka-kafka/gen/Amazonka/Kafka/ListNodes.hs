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
-- Module      : Amazonka.Kafka.ListNodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the broker nodes in the cluster.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListNodes
  ( -- * Creating a Request
    ListNodes (..),
    newListNodes,

    -- * Request Lenses
    listNodes_maxResults,
    listNodes_nextToken,
    listNodes_clusterArn,

    -- * Destructuring the Response
    ListNodesResponse (..),
    newListNodesResponse,

    -- * Response Lenses
    listNodesResponse_nextToken,
    listNodesResponse_nodeInfoList,
    listNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNodes' smart constructor.
data ListNodes = ListNodes'
  { -- | The maximum number of results to return in the response. If there are
    -- more results, the response includes a NextToken parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The paginated results marker. When the result of the operation is
    -- truncated, the call returns NextToken in the response. To get the next
    -- batch, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listNodes_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
--
-- 'nextToken', 'listNodes_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
--
-- 'clusterArn', 'listNodes_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
newListNodes ::
  -- | 'clusterArn'
  Prelude.Text ->
  ListNodes
newListNodes pClusterArn_ =
  ListNodes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      clusterArn = pClusterArn_
    }

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listNodes_maxResults :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Natural)
listNodes_maxResults = Lens.lens (\ListNodes' {maxResults} -> maxResults) (\s@ListNodes' {} a -> s {maxResults = a} :: ListNodes)

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listNodes_nextToken :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_nextToken = Lens.lens (\ListNodes' {nextToken} -> nextToken) (\s@ListNodes' {} a -> s {nextToken = a} :: ListNodes)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
listNodes_clusterArn :: Lens.Lens' ListNodes Prelude.Text
listNodes_clusterArn = Lens.lens (\ListNodes' {clusterArn} -> clusterArn) (\s@ListNodes' {} a -> s {clusterArn = a} :: ListNodes)

instance Core.AWSPager ListNodes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNodesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNodesResponse_nodeInfoList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listNodes_nextToken
          Lens..~ rs
          Lens.^? listNodesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListNodes where
  type AWSResponse ListNodes = ListNodesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNodesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "nodeInfoList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNodes where
  hashWithSalt _salt ListNodes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData ListNodes where
  rnf ListNodes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterArn

instance Data.ToHeaders ListNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListNodes where
  toPath ListNodes' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Data.toBS clusterArn, "/nodes"]

instance Data.ToQuery ListNodes where
  toQuery ListNodes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListNodesResponse' smart constructor.
data ListNodesResponse = ListNodesResponse'
  { -- | The paginated results marker. When the result of a ListNodes operation
    -- is truncated, the call returns NextToken in the response. To get another
    -- batch of nodes, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List containing a NodeInfo object.
    nodeInfoList :: Prelude.Maybe [NodeInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNodesResponse_nextToken' - The paginated results marker. When the result of a ListNodes operation
-- is truncated, the call returns NextToken in the response. To get another
-- batch of nodes, provide this token in your next request.
--
-- 'nodeInfoList', 'listNodesResponse_nodeInfoList' - List containing a NodeInfo object.
--
-- 'httpStatus', 'listNodesResponse_httpStatus' - The response's http status code.
newListNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNodesResponse
newListNodesResponse pHttpStatus_ =
  ListNodesResponse'
    { nextToken = Prelude.Nothing,
      nodeInfoList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The paginated results marker. When the result of a ListNodes operation
-- is truncated, the call returns NextToken in the response. To get another
-- batch of nodes, provide this token in your next request.
listNodesResponse_nextToken :: Lens.Lens' ListNodesResponse (Prelude.Maybe Prelude.Text)
listNodesResponse_nextToken = Lens.lens (\ListNodesResponse' {nextToken} -> nextToken) (\s@ListNodesResponse' {} a -> s {nextToken = a} :: ListNodesResponse)

-- | List containing a NodeInfo object.
listNodesResponse_nodeInfoList :: Lens.Lens' ListNodesResponse (Prelude.Maybe [NodeInfo])
listNodesResponse_nodeInfoList = Lens.lens (\ListNodesResponse' {nodeInfoList} -> nodeInfoList) (\s@ListNodesResponse' {} a -> s {nodeInfoList = a} :: ListNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNodesResponse_httpStatus :: Lens.Lens' ListNodesResponse Prelude.Int
listNodesResponse_httpStatus = Lens.lens (\ListNodesResponse' {httpStatus} -> httpStatus) (\s@ListNodesResponse' {} a -> s {httpStatus = a} :: ListNodesResponse)

instance Prelude.NFData ListNodesResponse where
  rnf ListNodesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nodeInfoList
      `Prelude.seq` Prelude.rnf httpStatus
