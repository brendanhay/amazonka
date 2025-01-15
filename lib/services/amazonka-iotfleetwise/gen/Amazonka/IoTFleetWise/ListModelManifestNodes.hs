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
-- Module      : Amazonka.IoTFleetWise.ListModelManifestNodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about nodes specified in a vehicle model (model
-- manifest).
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListModelManifestNodes
  ( -- * Creating a Request
    ListModelManifestNodes (..),
    newListModelManifestNodes,

    -- * Request Lenses
    listModelManifestNodes_maxResults,
    listModelManifestNodes_nextToken,
    listModelManifestNodes_name,

    -- * Destructuring the Response
    ListModelManifestNodesResponse (..),
    newListModelManifestNodesResponse,

    -- * Response Lenses
    listModelManifestNodesResponse_nextToken,
    listModelManifestNodesResponse_nodes,
    listModelManifestNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListModelManifestNodes' smart constructor.
data ListModelManifestNodes = ListModelManifestNodes'
  { -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the vehicle model to list information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelManifestNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listModelManifestNodes_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'nextToken', 'listModelManifestNodes_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'name', 'listModelManifestNodes_name' - The name of the vehicle model to list information about.
newListModelManifestNodes ::
  -- | 'name'
  Prelude.Text ->
  ListModelManifestNodes
newListModelManifestNodes pName_ =
  ListModelManifestNodes'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of items to return, between 1 and 100, inclusive.
listModelManifestNodes_maxResults :: Lens.Lens' ListModelManifestNodes (Prelude.Maybe Prelude.Natural)
listModelManifestNodes_maxResults = Lens.lens (\ListModelManifestNodes' {maxResults} -> maxResults) (\s@ListModelManifestNodes' {} a -> s {maxResults = a} :: ListModelManifestNodes)

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listModelManifestNodes_nextToken :: Lens.Lens' ListModelManifestNodes (Prelude.Maybe Prelude.Text)
listModelManifestNodes_nextToken = Lens.lens (\ListModelManifestNodes' {nextToken} -> nextToken) (\s@ListModelManifestNodes' {} a -> s {nextToken = a} :: ListModelManifestNodes)

-- | The name of the vehicle model to list information about.
listModelManifestNodes_name :: Lens.Lens' ListModelManifestNodes Prelude.Text
listModelManifestNodes_name = Lens.lens (\ListModelManifestNodes' {name} -> name) (\s@ListModelManifestNodes' {} a -> s {name = a} :: ListModelManifestNodes)

instance Core.AWSPager ListModelManifestNodes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelManifestNodesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listModelManifestNodesResponse_nodes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listModelManifestNodes_nextToken
              Lens..~ rs
              Lens.^? listModelManifestNodesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListModelManifestNodes where
  type
    AWSResponse ListModelManifestNodes =
      ListModelManifestNodesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelManifestNodesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "nodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListModelManifestNodes where
  hashWithSalt _salt ListModelManifestNodes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListModelManifestNodes where
  rnf ListModelManifestNodes' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders ListModelManifestNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListModelManifestNodes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelManifestNodes where
  toJSON ListModelManifestNodes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath ListModelManifestNodes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelManifestNodes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelManifestNodesResponse' smart constructor.
data ListModelManifestNodesResponse = ListModelManifestNodesResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of information about nodes.
    nodes :: Prelude.Maybe [Node],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelManifestNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelManifestNodesResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'nodes', 'listModelManifestNodesResponse_nodes' - A list of information about nodes.
--
-- 'httpStatus', 'listModelManifestNodesResponse_httpStatus' - The response's http status code.
newListModelManifestNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelManifestNodesResponse
newListModelManifestNodesResponse pHttpStatus_ =
  ListModelManifestNodesResponse'
    { nextToken =
        Prelude.Nothing,
      nodes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listModelManifestNodesResponse_nextToken :: Lens.Lens' ListModelManifestNodesResponse (Prelude.Maybe Prelude.Text)
listModelManifestNodesResponse_nextToken = Lens.lens (\ListModelManifestNodesResponse' {nextToken} -> nextToken) (\s@ListModelManifestNodesResponse' {} a -> s {nextToken = a} :: ListModelManifestNodesResponse)

-- | A list of information about nodes.
listModelManifestNodesResponse_nodes :: Lens.Lens' ListModelManifestNodesResponse (Prelude.Maybe [Node])
listModelManifestNodesResponse_nodes = Lens.lens (\ListModelManifestNodesResponse' {nodes} -> nodes) (\s@ListModelManifestNodesResponse' {} a -> s {nodes = a} :: ListModelManifestNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listModelManifestNodesResponse_httpStatus :: Lens.Lens' ListModelManifestNodesResponse Prelude.Int
listModelManifestNodesResponse_httpStatus = Lens.lens (\ListModelManifestNodesResponse' {httpStatus} -> httpStatus) (\s@ListModelManifestNodesResponse' {} a -> s {httpStatus = a} :: ListModelManifestNodesResponse)

instance
  Prelude.NFData
    ListModelManifestNodesResponse
  where
  rnf ListModelManifestNodesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf nodes `Prelude.seq`
        Prelude.rnf httpStatus
