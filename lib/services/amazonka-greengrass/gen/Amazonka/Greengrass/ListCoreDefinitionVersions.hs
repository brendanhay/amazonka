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
-- Module      : Amazonka.Greengrass.ListCoreDefinitionVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a core definition.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListCoreDefinitionVersions
  ( -- * Creating a Request
    ListCoreDefinitionVersions (..),
    newListCoreDefinitionVersions,

    -- * Request Lenses
    listCoreDefinitionVersions_maxResults,
    listCoreDefinitionVersions_nextToken,
    listCoreDefinitionVersions_coreDefinitionId,

    -- * Destructuring the Response
    ListCoreDefinitionVersionsResponse (..),
    newListCoreDefinitionVersionsResponse,

    -- * Response Lenses
    listCoreDefinitionVersionsResponse_nextToken,
    listCoreDefinitionVersionsResponse_versions,
    listCoreDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoreDefinitionVersions' smart constructor.
data ListCoreDefinitionVersions = ListCoreDefinitionVersions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the core definition.
    coreDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCoreDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listCoreDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'coreDefinitionId', 'listCoreDefinitionVersions_coreDefinitionId' - The ID of the core definition.
newListCoreDefinitionVersions ::
  -- | 'coreDefinitionId'
  Prelude.Text ->
  ListCoreDefinitionVersions
newListCoreDefinitionVersions pCoreDefinitionId_ =
  ListCoreDefinitionVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | The maximum number of results to be returned per request.
listCoreDefinitionVersions_maxResults :: Lens.Lens' ListCoreDefinitionVersions (Prelude.Maybe Prelude.Text)
listCoreDefinitionVersions_maxResults = Lens.lens (\ListCoreDefinitionVersions' {maxResults} -> maxResults) (\s@ListCoreDefinitionVersions' {} a -> s {maxResults = a} :: ListCoreDefinitionVersions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionVersions_nextToken :: Lens.Lens' ListCoreDefinitionVersions (Prelude.Maybe Prelude.Text)
listCoreDefinitionVersions_nextToken = Lens.lens (\ListCoreDefinitionVersions' {nextToken} -> nextToken) (\s@ListCoreDefinitionVersions' {} a -> s {nextToken = a} :: ListCoreDefinitionVersions)

-- | The ID of the core definition.
listCoreDefinitionVersions_coreDefinitionId :: Lens.Lens' ListCoreDefinitionVersions Prelude.Text
listCoreDefinitionVersions_coreDefinitionId = Lens.lens (\ListCoreDefinitionVersions' {coreDefinitionId} -> coreDefinitionId) (\s@ListCoreDefinitionVersions' {} a -> s {coreDefinitionId = a} :: ListCoreDefinitionVersions)

instance Core.AWSPager ListCoreDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionVersionsResponse_versions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCoreDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listCoreDefinitionVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCoreDefinitionVersions where
  type
    AWSResponse ListCoreDefinitionVersions =
      ListCoreDefinitionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDefinitionVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCoreDefinitionVersions where
  hashWithSalt _salt ListCoreDefinitionVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` coreDefinitionId

instance Prelude.NFData ListCoreDefinitionVersions where
  rnf ListCoreDefinitionVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf coreDefinitionId

instance Data.ToHeaders ListCoreDefinitionVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCoreDefinitionVersions where
  toPath ListCoreDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Data.toBS coreDefinitionId,
        "/versions"
      ]

instance Data.ToQuery ListCoreDefinitionVersions where
  toQuery ListCoreDefinitionVersions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCoreDefinitionVersionsResponse' smart constructor.
data ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse'
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
-- Create a value of 'ListCoreDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listCoreDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listCoreDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListCoreDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoreDefinitionVersionsResponse
newListCoreDefinitionVersionsResponse pHttpStatus_ =
  ListCoreDefinitionVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionVersionsResponse_nextToken :: Lens.Lens' ListCoreDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listCoreDefinitionVersionsResponse_nextToken = Lens.lens (\ListCoreDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListCoreDefinitionVersionsResponse)

-- | Information about a version.
listCoreDefinitionVersionsResponse_versions :: Lens.Lens' ListCoreDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listCoreDefinitionVersionsResponse_versions = Lens.lens (\ListCoreDefinitionVersionsResponse' {versions} -> versions) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {versions = a} :: ListCoreDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCoreDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListCoreDefinitionVersionsResponse Prelude.Int
listCoreDefinitionVersionsResponse_httpStatus = Lens.lens (\ListCoreDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListCoreDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListCoreDefinitionVersionsResponse
  where
  rnf ListCoreDefinitionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
