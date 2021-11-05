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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listCoreDefinitionVersions_nextToken,
    listCoreDefinitionVersions_maxResults,
    listCoreDefinitionVersions_coreDefinitionId,

    -- * Destructuring the Response
    ListCoreDefinitionVersionsResponse (..),
    newListCoreDefinitionVersionsResponse,

    -- * Response Lenses
    listCoreDefinitionVersionsResponse_versions,
    listCoreDefinitionVersionsResponse_nextToken,
    listCoreDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Greengrass.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoreDefinitionVersions' smart constructor.
data ListCoreDefinitionVersions = ListCoreDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listCoreDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listCoreDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'coreDefinitionId', 'listCoreDefinitionVersions_coreDefinitionId' - The ID of the core definition.
newListCoreDefinitionVersions ::
  -- | 'coreDefinitionId'
  Prelude.Text ->
  ListCoreDefinitionVersions
newListCoreDefinitionVersions pCoreDefinitionId_ =
  ListCoreDefinitionVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionVersions_nextToken :: Lens.Lens' ListCoreDefinitionVersions (Prelude.Maybe Prelude.Text)
listCoreDefinitionVersions_nextToken = Lens.lens (\ListCoreDefinitionVersions' {nextToken} -> nextToken) (\s@ListCoreDefinitionVersions' {} a -> s {nextToken = a} :: ListCoreDefinitionVersions)

-- | The maximum number of results to be returned per request.
listCoreDefinitionVersions_maxResults :: Lens.Lens' ListCoreDefinitionVersions (Prelude.Maybe Prelude.Text)
listCoreDefinitionVersions_maxResults = Lens.lens (\ListCoreDefinitionVersions' {maxResults} -> maxResults) (\s@ListCoreDefinitionVersions' {} a -> s {maxResults = a} :: ListCoreDefinitionVersions)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listCoreDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listCoreDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCoreDefinitionVersions where
  type
    AWSResponse ListCoreDefinitionVersions =
      ListCoreDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDefinitionVersionsResponse'
            Prelude.<$> (x Core..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCoreDefinitionVersions

instance Prelude.NFData ListCoreDefinitionVersions

instance Core.ToHeaders ListCoreDefinitionVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListCoreDefinitionVersions where
  toPath ListCoreDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Core.toBS coreDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListCoreDefinitionVersions where
  toQuery ListCoreDefinitionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCoreDefinitionVersionsResponse' smart constructor.
data ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse'
  { -- | Information about a version.
    versions :: Prelude.Maybe [VersionInformation],
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'versions', 'listCoreDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'nextToken', 'listCoreDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'httpStatus', 'listCoreDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListCoreDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoreDefinitionVersionsResponse
newListCoreDefinitionVersionsResponse pHttpStatus_ =
  ListCoreDefinitionVersionsResponse'
    { versions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a version.
listCoreDefinitionVersionsResponse_versions :: Lens.Lens' ListCoreDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listCoreDefinitionVersionsResponse_versions = Lens.lens (\ListCoreDefinitionVersionsResponse' {versions} -> versions) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {versions = a} :: ListCoreDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionVersionsResponse_nextToken :: Lens.Lens' ListCoreDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listCoreDefinitionVersionsResponse_nextToken = Lens.lens (\ListCoreDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListCoreDefinitionVersionsResponse)

-- | The response's http status code.
listCoreDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListCoreDefinitionVersionsResponse Prelude.Int
listCoreDefinitionVersionsResponse_httpStatus = Lens.lens (\ListCoreDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListCoreDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListCoreDefinitionVersionsResponse
