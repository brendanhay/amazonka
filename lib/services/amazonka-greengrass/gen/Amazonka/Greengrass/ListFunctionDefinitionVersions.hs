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
-- Module      : Amazonka.Greengrass.ListFunctionDefinitionVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a Lambda function definition.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListFunctionDefinitionVersions
  ( -- * Creating a Request
    ListFunctionDefinitionVersions (..),
    newListFunctionDefinitionVersions,

    -- * Request Lenses
    listFunctionDefinitionVersions_maxResults,
    listFunctionDefinitionVersions_nextToken,
    listFunctionDefinitionVersions_functionDefinitionId,

    -- * Destructuring the Response
    ListFunctionDefinitionVersionsResponse (..),
    newListFunctionDefinitionVersionsResponse,

    -- * Response Lenses
    listFunctionDefinitionVersionsResponse_nextToken,
    listFunctionDefinitionVersionsResponse_versions,
    listFunctionDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFunctionDefinitionVersions' smart constructor.
data ListFunctionDefinitionVersions = ListFunctionDefinitionVersions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFunctionDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFunctionDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listFunctionDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'functionDefinitionId', 'listFunctionDefinitionVersions_functionDefinitionId' - The ID of the Lambda function definition.
newListFunctionDefinitionVersions ::
  -- | 'functionDefinitionId'
  Prelude.Text ->
  ListFunctionDefinitionVersions
newListFunctionDefinitionVersions
  pFunctionDefinitionId_ =
    ListFunctionDefinitionVersions'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        functionDefinitionId =
          pFunctionDefinitionId_
      }

-- | The maximum number of results to be returned per request.
listFunctionDefinitionVersions_maxResults :: Lens.Lens' ListFunctionDefinitionVersions (Prelude.Maybe Prelude.Text)
listFunctionDefinitionVersions_maxResults = Lens.lens (\ListFunctionDefinitionVersions' {maxResults} -> maxResults) (\s@ListFunctionDefinitionVersions' {} a -> s {maxResults = a} :: ListFunctionDefinitionVersions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitionVersions_nextToken :: Lens.Lens' ListFunctionDefinitionVersions (Prelude.Maybe Prelude.Text)
listFunctionDefinitionVersions_nextToken = Lens.lens (\ListFunctionDefinitionVersions' {nextToken} -> nextToken) (\s@ListFunctionDefinitionVersions' {} a -> s {nextToken = a} :: ListFunctionDefinitionVersions)

-- | The ID of the Lambda function definition.
listFunctionDefinitionVersions_functionDefinitionId :: Lens.Lens' ListFunctionDefinitionVersions Prelude.Text
listFunctionDefinitionVersions_functionDefinitionId = Lens.lens (\ListFunctionDefinitionVersions' {functionDefinitionId} -> functionDefinitionId) (\s@ListFunctionDefinitionVersions' {} a -> s {functionDefinitionId = a} :: ListFunctionDefinitionVersions)

instance Core.AWSPager ListFunctionDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionVersionsResponse_versions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFunctionDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listFunctionDefinitionVersionsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListFunctionDefinitionVersions
  where
  type
    AWSResponse ListFunctionDefinitionVersions =
      ListFunctionDefinitionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionDefinitionVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFunctionDefinitionVersions
  where
  hashWithSalt
    _salt
    ListFunctionDefinitionVersions' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` functionDefinitionId

instance
  Prelude.NFData
    ListFunctionDefinitionVersions
  where
  rnf ListFunctionDefinitionVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf functionDefinitionId

instance
  Data.ToHeaders
    ListFunctionDefinitionVersions
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

instance Data.ToPath ListFunctionDefinitionVersions where
  toPath ListFunctionDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Data.toBS functionDefinitionId,
        "/versions"
      ]

instance Data.ToQuery ListFunctionDefinitionVersions where
  toQuery ListFunctionDefinitionVersions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFunctionDefinitionVersionsResponse' smart constructor.
data ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse'
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
-- Create a value of 'ListFunctionDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFunctionDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listFunctionDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listFunctionDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListFunctionDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFunctionDefinitionVersionsResponse
newListFunctionDefinitionVersionsResponse
  pHttpStatus_ =
    ListFunctionDefinitionVersionsResponse'
      { nextToken =
          Prelude.Nothing,
        versions = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitionVersionsResponse_nextToken :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listFunctionDefinitionVersionsResponse_nextToken = Lens.lens (\ListFunctionDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListFunctionDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListFunctionDefinitionVersionsResponse)

-- | Information about a version.
listFunctionDefinitionVersionsResponse_versions :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listFunctionDefinitionVersionsResponse_versions = Lens.lens (\ListFunctionDefinitionVersionsResponse' {versions} -> versions) (\s@ListFunctionDefinitionVersionsResponse' {} a -> s {versions = a} :: ListFunctionDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFunctionDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListFunctionDefinitionVersionsResponse Prelude.Int
listFunctionDefinitionVersionsResponse_httpStatus = Lens.lens (\ListFunctionDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListFunctionDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListFunctionDefinitionVersionsResponse
  where
  rnf ListFunctionDefinitionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
