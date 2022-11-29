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
-- Module      : Amazonka.IoTFleetWise.ListSignalCatalogNodes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists of information about the signals (nodes) specified in a signal
-- catalog.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListSignalCatalogNodes
  ( -- * Creating a Request
    ListSignalCatalogNodes (..),
    newListSignalCatalogNodes,

    -- * Request Lenses
    listSignalCatalogNodes_nextToken,
    listSignalCatalogNodes_maxResults,
    listSignalCatalogNodes_name,

    -- * Destructuring the Response
    ListSignalCatalogNodesResponse (..),
    newListSignalCatalogNodesResponse,

    -- * Response Lenses
    listSignalCatalogNodesResponse_nextToken,
    listSignalCatalogNodesResponse_nodes,
    listSignalCatalogNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSignalCatalogNodes' smart constructor.
data ListSignalCatalogNodes = ListSignalCatalogNodes'
  { -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the signal catalog to list information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSignalCatalogNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSignalCatalogNodes_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'maxResults', 'listSignalCatalogNodes_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'name', 'listSignalCatalogNodes_name' - The name of the signal catalog to list information about.
newListSignalCatalogNodes ::
  -- | 'name'
  Prelude.Text ->
  ListSignalCatalogNodes
newListSignalCatalogNodes pName_ =
  ListSignalCatalogNodes'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listSignalCatalogNodes_nextToken :: Lens.Lens' ListSignalCatalogNodes (Prelude.Maybe Prelude.Text)
listSignalCatalogNodes_nextToken = Lens.lens (\ListSignalCatalogNodes' {nextToken} -> nextToken) (\s@ListSignalCatalogNodes' {} a -> s {nextToken = a} :: ListSignalCatalogNodes)

-- | The maximum number of items to return, between 1 and 100, inclusive.
listSignalCatalogNodes_maxResults :: Lens.Lens' ListSignalCatalogNodes (Prelude.Maybe Prelude.Natural)
listSignalCatalogNodes_maxResults = Lens.lens (\ListSignalCatalogNodes' {maxResults} -> maxResults) (\s@ListSignalCatalogNodes' {} a -> s {maxResults = a} :: ListSignalCatalogNodes)

-- | The name of the signal catalog to list information about.
listSignalCatalogNodes_name :: Lens.Lens' ListSignalCatalogNodes Prelude.Text
listSignalCatalogNodes_name = Lens.lens (\ListSignalCatalogNodes' {name} -> name) (\s@ListSignalCatalogNodes' {} a -> s {name = a} :: ListSignalCatalogNodes)

instance Core.AWSPager ListSignalCatalogNodes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSignalCatalogNodesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSignalCatalogNodesResponse_nodes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSignalCatalogNodes_nextToken
          Lens..~ rs
          Lens.^? listSignalCatalogNodesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSignalCatalogNodes where
  type
    AWSResponse ListSignalCatalogNodes =
      ListSignalCatalogNodesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSignalCatalogNodesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "nodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSignalCatalogNodes where
  hashWithSalt _salt ListSignalCatalogNodes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListSignalCatalogNodes where
  rnf ListSignalCatalogNodes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders ListSignalCatalogNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.ListSignalCatalogNodes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSignalCatalogNodes where
  toJSON ListSignalCatalogNodes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath ListSignalCatalogNodes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSignalCatalogNodes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSignalCatalogNodesResponse' smart constructor.
data ListSignalCatalogNodesResponse = ListSignalCatalogNodesResponse'
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
-- Create a value of 'ListSignalCatalogNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSignalCatalogNodesResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'nodes', 'listSignalCatalogNodesResponse_nodes' - A list of information about nodes.
--
-- 'httpStatus', 'listSignalCatalogNodesResponse_httpStatus' - The response's http status code.
newListSignalCatalogNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSignalCatalogNodesResponse
newListSignalCatalogNodesResponse pHttpStatus_ =
  ListSignalCatalogNodesResponse'
    { nextToken =
        Prelude.Nothing,
      nodes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listSignalCatalogNodesResponse_nextToken :: Lens.Lens' ListSignalCatalogNodesResponse (Prelude.Maybe Prelude.Text)
listSignalCatalogNodesResponse_nextToken = Lens.lens (\ListSignalCatalogNodesResponse' {nextToken} -> nextToken) (\s@ListSignalCatalogNodesResponse' {} a -> s {nextToken = a} :: ListSignalCatalogNodesResponse)

-- | A list of information about nodes.
listSignalCatalogNodesResponse_nodes :: Lens.Lens' ListSignalCatalogNodesResponse (Prelude.Maybe [Node])
listSignalCatalogNodesResponse_nodes = Lens.lens (\ListSignalCatalogNodesResponse' {nodes} -> nodes) (\s@ListSignalCatalogNodesResponse' {} a -> s {nodes = a} :: ListSignalCatalogNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSignalCatalogNodesResponse_httpStatus :: Lens.Lens' ListSignalCatalogNodesResponse Prelude.Int
listSignalCatalogNodesResponse_httpStatus = Lens.lens (\ListSignalCatalogNodesResponse' {httpStatus} -> httpStatus) (\s@ListSignalCatalogNodesResponse' {} a -> s {httpStatus = a} :: ListSignalCatalogNodesResponse)

instance
  Prelude.NFData
    ListSignalCatalogNodesResponse
  where
  rnf ListSignalCatalogNodesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf httpStatus
