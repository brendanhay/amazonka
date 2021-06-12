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
-- Module      : Network.AWS.CloudDirectory.ListFacetNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of facets that exist in a schema.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetNames
  ( -- * Creating a Request
    ListFacetNames (..),
    newListFacetNames,

    -- * Request Lenses
    listFacetNames_nextToken,
    listFacetNames_maxResults,
    listFacetNames_schemaArn,

    -- * Destructuring the Response
    ListFacetNamesResponse (..),
    newListFacetNamesResponse,

    -- * Response Lenses
    listFacetNamesResponse_nextToken,
    listFacetNamesResponse_facetNames,
    listFacetNamesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFacetNames' smart constructor.
data ListFacetNames = ListFacetNames'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) to retrieve facet names from.
    schemaArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFacetNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFacetNames_nextToken' - The pagination token.
--
-- 'maxResults', 'listFacetNames_maxResults' - The maximum number of results to retrieve.
--
-- 'schemaArn', 'listFacetNames_schemaArn' - The Amazon Resource Name (ARN) to retrieve facet names from.
newListFacetNames ::
  -- | 'schemaArn'
  Core.Text ->
  ListFacetNames
newListFacetNames pSchemaArn_ =
  ListFacetNames'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      schemaArn = pSchemaArn_
    }

-- | The pagination token.
listFacetNames_nextToken :: Lens.Lens' ListFacetNames (Core.Maybe Core.Text)
listFacetNames_nextToken = Lens.lens (\ListFacetNames' {nextToken} -> nextToken) (\s@ListFacetNames' {} a -> s {nextToken = a} :: ListFacetNames)

-- | The maximum number of results to retrieve.
listFacetNames_maxResults :: Lens.Lens' ListFacetNames (Core.Maybe Core.Natural)
listFacetNames_maxResults = Lens.lens (\ListFacetNames' {maxResults} -> maxResults) (\s@ListFacetNames' {} a -> s {maxResults = a} :: ListFacetNames)

-- | The Amazon Resource Name (ARN) to retrieve facet names from.
listFacetNames_schemaArn :: Lens.Lens' ListFacetNames Core.Text
listFacetNames_schemaArn = Lens.lens (\ListFacetNames' {schemaArn} -> schemaArn) (\s@ListFacetNames' {} a -> s {schemaArn = a} :: ListFacetNames)

instance Core.AWSPager ListFacetNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFacetNamesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listFacetNamesResponse_facetNames Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFacetNames_nextToken
          Lens..~ rs
          Lens.^? listFacetNamesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListFacetNames where
  type
    AWSResponse ListFacetNames =
      ListFacetNamesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacetNamesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "FacetNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListFacetNames

instance Core.NFData ListFacetNames

instance Core.ToHeaders ListFacetNames where
  toHeaders ListFacetNames' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON ListFacetNames where
  toJSON ListFacetNames' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListFacetNames where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/facet/list"

instance Core.ToQuery ListFacetNames where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListFacetNamesResponse' smart constructor.
data ListFacetNamesResponse = ListFacetNamesResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The names of facets that exist within the schema.
    facetNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFacetNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFacetNamesResponse_nextToken' - The pagination token.
--
-- 'facetNames', 'listFacetNamesResponse_facetNames' - The names of facets that exist within the schema.
--
-- 'httpStatus', 'listFacetNamesResponse_httpStatus' - The response's http status code.
newListFacetNamesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFacetNamesResponse
newListFacetNamesResponse pHttpStatus_ =
  ListFacetNamesResponse'
    { nextToken = Core.Nothing,
      facetNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listFacetNamesResponse_nextToken :: Lens.Lens' ListFacetNamesResponse (Core.Maybe Core.Text)
listFacetNamesResponse_nextToken = Lens.lens (\ListFacetNamesResponse' {nextToken} -> nextToken) (\s@ListFacetNamesResponse' {} a -> s {nextToken = a} :: ListFacetNamesResponse)

-- | The names of facets that exist within the schema.
listFacetNamesResponse_facetNames :: Lens.Lens' ListFacetNamesResponse (Core.Maybe [Core.Text])
listFacetNamesResponse_facetNames = Lens.lens (\ListFacetNamesResponse' {facetNames} -> facetNames) (\s@ListFacetNamesResponse' {} a -> s {facetNames = a} :: ListFacetNamesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFacetNamesResponse_httpStatus :: Lens.Lens' ListFacetNamesResponse Core.Int
listFacetNamesResponse_httpStatus = Lens.lens (\ListFacetNamesResponse' {httpStatus} -> httpStatus) (\s@ListFacetNamesResponse' {} a -> s {httpStatus = a} :: ListFacetNamesResponse)

instance Core.NFData ListFacetNamesResponse
