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
-- Module      : Amazonka.Greengrass.ListConnectorDefinitionVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a connector definition, which are containers for
-- connectors. Connectors run on the Greengrass core and contain built-in
-- integration with local infrastructure, device protocols, AWS, and other
-- cloud services.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListConnectorDefinitionVersions
  ( -- * Creating a Request
    ListConnectorDefinitionVersions (..),
    newListConnectorDefinitionVersions,

    -- * Request Lenses
    listConnectorDefinitionVersions_nextToken,
    listConnectorDefinitionVersions_maxResults,
    listConnectorDefinitionVersions_connectorDefinitionId,

    -- * Destructuring the Response
    ListConnectorDefinitionVersionsResponse (..),
    newListConnectorDefinitionVersionsResponse,

    -- * Response Lenses
    listConnectorDefinitionVersionsResponse_nextToken,
    listConnectorDefinitionVersionsResponse_versions,
    listConnectorDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnectorDefinitionVersions' smart constructor.
data ListConnectorDefinitionVersions = ListConnectorDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connector definition.
    connectorDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectorDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectorDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listConnectorDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'connectorDefinitionId', 'listConnectorDefinitionVersions_connectorDefinitionId' - The ID of the connector definition.
newListConnectorDefinitionVersions ::
  -- | 'connectorDefinitionId'
  Prelude.Text ->
  ListConnectorDefinitionVersions
newListConnectorDefinitionVersions
  pConnectorDefinitionId_ =
    ListConnectorDefinitionVersions'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        connectorDefinitionId =
          pConnectorDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listConnectorDefinitionVersions_nextToken :: Lens.Lens' ListConnectorDefinitionVersions (Prelude.Maybe Prelude.Text)
listConnectorDefinitionVersions_nextToken = Lens.lens (\ListConnectorDefinitionVersions' {nextToken} -> nextToken) (\s@ListConnectorDefinitionVersions' {} a -> s {nextToken = a} :: ListConnectorDefinitionVersions)

-- | The maximum number of results to be returned per request.
listConnectorDefinitionVersions_maxResults :: Lens.Lens' ListConnectorDefinitionVersions (Prelude.Maybe Prelude.Text)
listConnectorDefinitionVersions_maxResults = Lens.lens (\ListConnectorDefinitionVersions' {maxResults} -> maxResults) (\s@ListConnectorDefinitionVersions' {} a -> s {maxResults = a} :: ListConnectorDefinitionVersions)

-- | The ID of the connector definition.
listConnectorDefinitionVersions_connectorDefinitionId :: Lens.Lens' ListConnectorDefinitionVersions Prelude.Text
listConnectorDefinitionVersions_connectorDefinitionId = Lens.lens (\ListConnectorDefinitionVersions' {connectorDefinitionId} -> connectorDefinitionId) (\s@ListConnectorDefinitionVersions' {} a -> s {connectorDefinitionId = a} :: ListConnectorDefinitionVersions)

instance
  Core.AWSPager
    ListConnectorDefinitionVersions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConnectorDefinitionVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listConnectorDefinitionVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listConnectorDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listConnectorDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListConnectorDefinitionVersions
  where
  type
    AWSResponse ListConnectorDefinitionVersions =
      ListConnectorDefinitionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorDefinitionVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListConnectorDefinitionVersions
  where
  hashWithSalt
    _salt
    ListConnectorDefinitionVersions' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` connectorDefinitionId

instance
  Prelude.NFData
    ListConnectorDefinitionVersions
  where
  rnf ListConnectorDefinitionVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf connectorDefinitionId

instance
  Data.ToHeaders
    ListConnectorDefinitionVersions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConnectorDefinitionVersions where
  toPath ListConnectorDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Data.toBS connectorDefinitionId,
        "/versions"
      ]

instance Data.ToQuery ListConnectorDefinitionVersions where
  toQuery ListConnectorDefinitionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListConnectorDefinitionVersionsResponse' smart constructor.
data ListConnectorDefinitionVersionsResponse = ListConnectorDefinitionVersionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about a version.
    versions :: Prelude.Maybe [VersionInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectorDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectorDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listConnectorDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listConnectorDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListConnectorDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectorDefinitionVersionsResponse
newListConnectorDefinitionVersionsResponse
  pHttpStatus_ =
    ListConnectorDefinitionVersionsResponse'
      { nextToken =
          Prelude.Nothing,
        versions = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listConnectorDefinitionVersionsResponse_nextToken :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listConnectorDefinitionVersionsResponse_nextToken = Lens.lens (\ListConnectorDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListConnectorDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListConnectorDefinitionVersionsResponse)

-- | Information about a version.
listConnectorDefinitionVersionsResponse_versions :: Lens.Lens' ListConnectorDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listConnectorDefinitionVersionsResponse_versions = Lens.lens (\ListConnectorDefinitionVersionsResponse' {versions} -> versions) (\s@ListConnectorDefinitionVersionsResponse' {} a -> s {versions = a} :: ListConnectorDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listConnectorDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListConnectorDefinitionVersionsResponse Prelude.Int
listConnectorDefinitionVersionsResponse_httpStatus = Lens.lens (\ListConnectorDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListConnectorDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListConnectorDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListConnectorDefinitionVersionsResponse
  where
  rnf ListConnectorDefinitionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
