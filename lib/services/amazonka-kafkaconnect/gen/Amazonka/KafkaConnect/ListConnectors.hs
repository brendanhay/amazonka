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
-- Module      : Amazonka.KafkaConnect.ListConnectors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the connectors in this account and Region. The
-- list is limited to connectors whose name starts with the specified
-- prefix. The response also includes a description of each of the listed
-- connectors.
--
-- This operation returns paginated results.
module Amazonka.KafkaConnect.ListConnectors
  ( -- * Creating a Request
    ListConnectors (..),
    newListConnectors,

    -- * Request Lenses
    listConnectors_connectorNamePrefix,
    listConnectors_nextToken,
    listConnectors_maxResults,

    -- * Destructuring the Response
    ListConnectorsResponse (..),
    newListConnectorsResponse,

    -- * Response Lenses
    listConnectorsResponse_nextToken,
    listConnectorsResponse_connectors,
    listConnectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnectors' smart constructor.
data ListConnectors = ListConnectors'
  { -- | The name prefix that you want to use to search for and list connectors.
    connectorNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | If the response of a ListConnectors operation is truncated, it will
    -- include a NextToken. Send this NextToken in a subsequent request to
    -- continue listing from where the previous operation left off.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of connectors to list in one response.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'connectorNamePrefix', 'listConnectors_connectorNamePrefix' - The name prefix that you want to use to search for and list connectors.
--
-- 'nextToken', 'listConnectors_nextToken' - If the response of a ListConnectors operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
--
-- 'maxResults', 'listConnectors_maxResults' - The maximum number of connectors to list in one response.
newListConnectors ::
  ListConnectors
newListConnectors =
  ListConnectors'
    { connectorNamePrefix =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The name prefix that you want to use to search for and list connectors.
listConnectors_connectorNamePrefix :: Lens.Lens' ListConnectors (Prelude.Maybe Prelude.Text)
listConnectors_connectorNamePrefix = Lens.lens (\ListConnectors' {connectorNamePrefix} -> connectorNamePrefix) (\s@ListConnectors' {} a -> s {connectorNamePrefix = a} :: ListConnectors)

-- | If the response of a ListConnectors operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where the previous operation left off.
listConnectors_nextToken :: Lens.Lens' ListConnectors (Prelude.Maybe Prelude.Text)
listConnectors_nextToken = Lens.lens (\ListConnectors' {nextToken} -> nextToken) (\s@ListConnectors' {} a -> s {nextToken = a} :: ListConnectors)

-- | The maximum number of connectors to list in one response.
listConnectors_maxResults :: Lens.Lens' ListConnectors (Prelude.Maybe Prelude.Natural)
listConnectors_maxResults = Lens.lens (\ListConnectors' {maxResults} -> maxResults) (\s@ListConnectors' {} a -> s {maxResults = a} :: ListConnectors)

instance Core.AWSPager ListConnectors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConnectorsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConnectorsResponse_connectors
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConnectors_nextToken
          Lens..~ rs
          Lens.^? listConnectorsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListConnectors where
  type
    AWSResponse ListConnectors =
      ListConnectorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "connectors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConnectors where
  hashWithSalt _salt ListConnectors' {..} =
    _salt `Prelude.hashWithSalt` connectorNamePrefix
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListConnectors where
  rnf ListConnectors' {..} =
    Prelude.rnf connectorNamePrefix
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListConnectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConnectors where
  toPath = Prelude.const "/v1/connectors"

instance Data.ToQuery ListConnectors where
  toQuery ListConnectors' {..} =
    Prelude.mconcat
      [ "connectorNamePrefix" Data.=: connectorNamePrefix,
        "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListConnectorsResponse' smart constructor.
data ListConnectorsResponse = ListConnectorsResponse'
  { -- | If the response of a ListConnectors operation is truncated, it will
    -- include a NextToken. Send this NextToken in a subsequent request to
    -- continue listing from where it left off.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of connector descriptions.
    connectors :: Prelude.Maybe [ConnectorSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listConnectorsResponse_nextToken' - If the response of a ListConnectors operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where it left off.
--
-- 'connectors', 'listConnectorsResponse_connectors' - An array of connector descriptions.
--
-- 'httpStatus', 'listConnectorsResponse_httpStatus' - The response's http status code.
newListConnectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectorsResponse
newListConnectorsResponse pHttpStatus_ =
  ListConnectorsResponse'
    { nextToken =
        Prelude.Nothing,
      connectors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response of a ListConnectors operation is truncated, it will
-- include a NextToken. Send this NextToken in a subsequent request to
-- continue listing from where it left off.
listConnectorsResponse_nextToken :: Lens.Lens' ListConnectorsResponse (Prelude.Maybe Prelude.Text)
listConnectorsResponse_nextToken = Lens.lens (\ListConnectorsResponse' {nextToken} -> nextToken) (\s@ListConnectorsResponse' {} a -> s {nextToken = a} :: ListConnectorsResponse)

-- | An array of connector descriptions.
listConnectorsResponse_connectors :: Lens.Lens' ListConnectorsResponse (Prelude.Maybe [ConnectorSummary])
listConnectorsResponse_connectors = Lens.lens (\ListConnectorsResponse' {connectors} -> connectors) (\s@ListConnectorsResponse' {} a -> s {connectors = a} :: ListConnectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listConnectorsResponse_httpStatus :: Lens.Lens' ListConnectorsResponse Prelude.Int
listConnectorsResponse_httpStatus = Lens.lens (\ListConnectorsResponse' {httpStatus} -> httpStatus) (\s@ListConnectorsResponse' {} a -> s {httpStatus = a} :: ListConnectorsResponse)

instance Prelude.NFData ListConnectorsResponse where
  rnf ListConnectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf connectors
      `Prelude.seq` Prelude.rnf httpStatus
