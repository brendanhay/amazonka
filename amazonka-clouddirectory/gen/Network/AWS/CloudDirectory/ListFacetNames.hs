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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFacetNames' smart constructor.
data ListFacetNames = ListFacetNames'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) to retrieve facet names from.
    schemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListFacetNames
newListFacetNames pSchemaArn_ =
  ListFacetNames'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      schemaArn = pSchemaArn_
    }

-- | The pagination token.
listFacetNames_nextToken :: Lens.Lens' ListFacetNames (Prelude.Maybe Prelude.Text)
listFacetNames_nextToken = Lens.lens (\ListFacetNames' {nextToken} -> nextToken) (\s@ListFacetNames' {} a -> s {nextToken = a} :: ListFacetNames)

-- | The maximum number of results to retrieve.
listFacetNames_maxResults :: Lens.Lens' ListFacetNames (Prelude.Maybe Prelude.Natural)
listFacetNames_maxResults = Lens.lens (\ListFacetNames' {maxResults} -> maxResults) (\s@ListFacetNames' {} a -> s {maxResults = a} :: ListFacetNames)

-- | The Amazon Resource Name (ARN) to retrieve facet names from.
listFacetNames_schemaArn :: Lens.Lens' ListFacetNames Prelude.Text
listFacetNames_schemaArn = Lens.lens (\ListFacetNames' {schemaArn} -> schemaArn) (\s@ListFacetNames' {} a -> s {schemaArn = a} :: ListFacetNames)

instance Core.AWSPager ListFacetNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFacetNamesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFacetNamesResponse_facetNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFacetNames_nextToken
          Lens..~ rs
          Lens.^? listFacetNamesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFacetNames where
  type
    AWSResponse ListFacetNames =
      ListFacetNamesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacetNamesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "FacetNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFacetNames

instance Prelude.NFData ListFacetNames

instance Core.ToHeaders ListFacetNames where
  toHeaders ListFacetNames' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON ListFacetNames where
  toJSON ListFacetNames' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListFacetNames where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet/list"

instance Core.ToQuery ListFacetNames where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFacetNamesResponse' smart constructor.
data ListFacetNamesResponse = ListFacetNamesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of facets that exist within the schema.
    facetNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListFacetNamesResponse
newListFacetNamesResponse pHttpStatus_ =
  ListFacetNamesResponse'
    { nextToken =
        Prelude.Nothing,
      facetNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listFacetNamesResponse_nextToken :: Lens.Lens' ListFacetNamesResponse (Prelude.Maybe Prelude.Text)
listFacetNamesResponse_nextToken = Lens.lens (\ListFacetNamesResponse' {nextToken} -> nextToken) (\s@ListFacetNamesResponse' {} a -> s {nextToken = a} :: ListFacetNamesResponse)

-- | The names of facets that exist within the schema.
listFacetNamesResponse_facetNames :: Lens.Lens' ListFacetNamesResponse (Prelude.Maybe [Prelude.Text])
listFacetNamesResponse_facetNames = Lens.lens (\ListFacetNamesResponse' {facetNames} -> facetNames) (\s@ListFacetNamesResponse' {} a -> s {facetNames = a} :: ListFacetNamesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFacetNamesResponse_httpStatus :: Lens.Lens' ListFacetNamesResponse Prelude.Int
listFacetNamesResponse_httpStatus = Lens.lens (\ListFacetNamesResponse' {httpStatus} -> httpStatus) (\s@ListFacetNamesResponse' {} a -> s {httpStatus = a} :: ListFacetNamesResponse)

instance Prelude.NFData ListFacetNamesResponse
