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
-- Module      : Amazonka.Greengrass.ListResourceDefinitionVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a resource definition.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListResourceDefinitionVersions
  ( -- * Creating a Request
    ListResourceDefinitionVersions (..),
    newListResourceDefinitionVersions,

    -- * Request Lenses
    listResourceDefinitionVersions_nextToken,
    listResourceDefinitionVersions_maxResults,
    listResourceDefinitionVersions_resourceDefinitionId,

    -- * Destructuring the Response
    ListResourceDefinitionVersionsResponse (..),
    newListResourceDefinitionVersionsResponse,

    -- * Response Lenses
    listResourceDefinitionVersionsResponse_nextToken,
    listResourceDefinitionVersionsResponse_versions,
    listResourceDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceDefinitionVersions' smart constructor.
data ListResourceDefinitionVersions = ListResourceDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listResourceDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'resourceDefinitionId', 'listResourceDefinitionVersions_resourceDefinitionId' - The ID of the resource definition.
newListResourceDefinitionVersions ::
  -- | 'resourceDefinitionId'
  Prelude.Text ->
  ListResourceDefinitionVersions
newListResourceDefinitionVersions
  pResourceDefinitionId_ =
    ListResourceDefinitionVersions'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        resourceDefinitionId =
          pResourceDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitionVersions_nextToken :: Lens.Lens' ListResourceDefinitionVersions (Prelude.Maybe Prelude.Text)
listResourceDefinitionVersions_nextToken = Lens.lens (\ListResourceDefinitionVersions' {nextToken} -> nextToken) (\s@ListResourceDefinitionVersions' {} a -> s {nextToken = a} :: ListResourceDefinitionVersions)

-- | The maximum number of results to be returned per request.
listResourceDefinitionVersions_maxResults :: Lens.Lens' ListResourceDefinitionVersions (Prelude.Maybe Prelude.Text)
listResourceDefinitionVersions_maxResults = Lens.lens (\ListResourceDefinitionVersions' {maxResults} -> maxResults) (\s@ListResourceDefinitionVersions' {} a -> s {maxResults = a} :: ListResourceDefinitionVersions)

-- | The ID of the resource definition.
listResourceDefinitionVersions_resourceDefinitionId :: Lens.Lens' ListResourceDefinitionVersions Prelude.Text
listResourceDefinitionVersions_resourceDefinitionId = Lens.lens (\ListResourceDefinitionVersions' {resourceDefinitionId} -> resourceDefinitionId) (\s@ListResourceDefinitionVersions' {} a -> s {resourceDefinitionId = a} :: ListResourceDefinitionVersions)

instance Core.AWSPager ListResourceDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listResourceDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListResourceDefinitionVersions
  where
  type
    AWSResponse ListResourceDefinitionVersions =
      ListResourceDefinitionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDefinitionVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResourceDefinitionVersions
  where
  hashWithSalt
    _salt
    ListResourceDefinitionVersions' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` resourceDefinitionId

instance
  Prelude.NFData
    ListResourceDefinitionVersions
  where
  rnf ListResourceDefinitionVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resourceDefinitionId

instance
  Core.ToHeaders
    ListResourceDefinitionVersions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListResourceDefinitionVersions where
  toPath ListResourceDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListResourceDefinitionVersions where
  toQuery ListResourceDefinitionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListResourceDefinitionVersionsResponse' smart constructor.
data ListResourceDefinitionVersionsResponse = ListResourceDefinitionVersionsResponse'
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
-- Create a value of 'ListResourceDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listResourceDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listResourceDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListResourceDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceDefinitionVersionsResponse
newListResourceDefinitionVersionsResponse
  pHttpStatus_ =
    ListResourceDefinitionVersionsResponse'
      { nextToken =
          Prelude.Nothing,
        versions = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitionVersionsResponse_nextToken :: Lens.Lens' ListResourceDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listResourceDefinitionVersionsResponse_nextToken = Lens.lens (\ListResourceDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListResourceDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListResourceDefinitionVersionsResponse)

-- | Information about a version.
listResourceDefinitionVersionsResponse_versions :: Lens.Lens' ListResourceDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listResourceDefinitionVersionsResponse_versions = Lens.lens (\ListResourceDefinitionVersionsResponse' {versions} -> versions) (\s@ListResourceDefinitionVersionsResponse' {} a -> s {versions = a} :: ListResourceDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListResourceDefinitionVersionsResponse Prelude.Int
listResourceDefinitionVersionsResponse_httpStatus = Lens.lens (\ListResourceDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListResourceDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListResourceDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListResourceDefinitionVersionsResponse
  where
  rnf ListResourceDefinitionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
