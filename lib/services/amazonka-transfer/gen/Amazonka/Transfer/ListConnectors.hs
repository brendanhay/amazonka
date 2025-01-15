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
-- Module      : Amazonka.Transfer.ListConnectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the connectors for the specified Region.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListConnectors
  ( -- * Creating a Request
    ListConnectors (..),
    newListConnectors,

    -- * Request Lenses
    listConnectors_maxResults,
    listConnectors_nextToken,

    -- * Destructuring the Response
    ListConnectorsResponse (..),
    newListConnectorsResponse,

    -- * Response Lenses
    listConnectorsResponse_nextToken,
    listConnectorsResponse_httpStatus,
    listConnectorsResponse_connectors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListConnectors' smart constructor.
data ListConnectors = ListConnectors'
  { -- | The maximum number of connectors to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you can get additional results from the @ListConnectors@ call, a
    -- @NextToken@ parameter is returned in the output. You can then pass in a
    -- subsequent command to the @NextToken@ parameter to continue listing
    -- additional connectors.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listConnectors_maxResults' - The maximum number of connectors to return.
--
-- 'nextToken', 'listConnectors_nextToken' - When you can get additional results from the @ListConnectors@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional connectors.
newListConnectors ::
  ListConnectors
newListConnectors =
  ListConnectors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of connectors to return.
listConnectors_maxResults :: Lens.Lens' ListConnectors (Prelude.Maybe Prelude.Natural)
listConnectors_maxResults = Lens.lens (\ListConnectors' {maxResults} -> maxResults) (\s@ListConnectors' {} a -> s {maxResults = a} :: ListConnectors)

-- | When you can get additional results from the @ListConnectors@ call, a
-- @NextToken@ parameter is returned in the output. You can then pass in a
-- subsequent command to the @NextToken@ parameter to continue listing
-- additional connectors.
listConnectors_nextToken :: Lens.Lens' ListConnectors (Prelude.Maybe Prelude.Text)
listConnectors_nextToken = Lens.lens (\ListConnectors' {nextToken} -> nextToken) (\s@ListConnectors' {} a -> s {nextToken = a} :: ListConnectors)

instance Core.AWSPager ListConnectors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConnectorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listConnectorsResponse_connectors) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listConnectors_nextToken
              Lens..~ rs
              Lens.^? listConnectorsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListConnectors where
  type
    AWSResponse ListConnectors =
      ListConnectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Connectors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListConnectors where
  hashWithSalt _salt ListConnectors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConnectors where
  rnf ListConnectors' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListConnectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListConnectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListConnectors where
  toJSON ListConnectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListConnectors where
  toPath = Prelude.const "/"

instance Data.ToQuery ListConnectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConnectorsResponse' smart constructor.
data ListConnectorsResponse = ListConnectorsResponse'
  { -- | Returns a token that you can use to call @ListConnectors@ again and
    -- receive additional results, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns an array, where each item contains the details of a connector.
    connectors :: [ListedConnector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectorsResponse_nextToken' - Returns a token that you can use to call @ListConnectors@ again and
-- receive additional results, if there are any.
--
-- 'httpStatus', 'listConnectorsResponse_httpStatus' - The response's http status code.
--
-- 'connectors', 'listConnectorsResponse_connectors' - Returns an array, where each item contains the details of a connector.
newListConnectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectorsResponse
newListConnectorsResponse pHttpStatus_ =
  ListConnectorsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      connectors = Prelude.mempty
    }

-- | Returns a token that you can use to call @ListConnectors@ again and
-- receive additional results, if there are any.
listConnectorsResponse_nextToken :: Lens.Lens' ListConnectorsResponse (Prelude.Maybe Prelude.Text)
listConnectorsResponse_nextToken = Lens.lens (\ListConnectorsResponse' {nextToken} -> nextToken) (\s@ListConnectorsResponse' {} a -> s {nextToken = a} :: ListConnectorsResponse)

-- | The response's http status code.
listConnectorsResponse_httpStatus :: Lens.Lens' ListConnectorsResponse Prelude.Int
listConnectorsResponse_httpStatus = Lens.lens (\ListConnectorsResponse' {httpStatus} -> httpStatus) (\s@ListConnectorsResponse' {} a -> s {httpStatus = a} :: ListConnectorsResponse)

-- | Returns an array, where each item contains the details of a connector.
listConnectorsResponse_connectors :: Lens.Lens' ListConnectorsResponse [ListedConnector]
listConnectorsResponse_connectors = Lens.lens (\ListConnectorsResponse' {connectors} -> connectors) (\s@ListConnectorsResponse' {} a -> s {connectors = a} :: ListConnectorsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListConnectorsResponse where
  rnf ListConnectorsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf connectors
