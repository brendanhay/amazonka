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
-- Module      : Amazonka.CloudDirectory.ListFacetNames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of facets that exist in a schema.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListFacetNames
  ( -- * Creating a Request
    ListFacetNames (..),
    newListFacetNames,

    -- * Request Lenses
    listFacetNames_maxResults,
    listFacetNames_nextToken,
    listFacetNames_schemaArn,

    -- * Destructuring the Response
    ListFacetNamesResponse (..),
    newListFacetNamesResponse,

    -- * Response Lenses
    listFacetNamesResponse_facetNames,
    listFacetNamesResponse_nextToken,
    listFacetNamesResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFacetNames' smart constructor.
data ListFacetNames = ListFacetNames'
  { -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listFacetNames_maxResults' - The maximum number of results to retrieve.
--
-- 'nextToken', 'listFacetNames_nextToken' - The pagination token.
--
-- 'schemaArn', 'listFacetNames_schemaArn' - The Amazon Resource Name (ARN) to retrieve facet names from.
newListFacetNames ::
  -- | 'schemaArn'
  Prelude.Text ->
  ListFacetNames
newListFacetNames pSchemaArn_ =
  ListFacetNames'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaArn = pSchemaArn_
    }

-- | The maximum number of results to retrieve.
listFacetNames_maxResults :: Lens.Lens' ListFacetNames (Prelude.Maybe Prelude.Natural)
listFacetNames_maxResults = Lens.lens (\ListFacetNames' {maxResults} -> maxResults) (\s@ListFacetNames' {} a -> s {maxResults = a} :: ListFacetNames)

-- | The pagination token.
listFacetNames_nextToken :: Lens.Lens' ListFacetNames (Prelude.Maybe Prelude.Text)
listFacetNames_nextToken = Lens.lens (\ListFacetNames' {nextToken} -> nextToken) (\s@ListFacetNames' {} a -> s {nextToken = a} :: ListFacetNames)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacetNamesResponse'
            Prelude.<$> (x Data..?> "FacetNames" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFacetNames where
  hashWithSalt _salt ListFacetNames' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData ListFacetNames where
  rnf ListFacetNames' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaArn

instance Data.ToHeaders ListFacetNames where
  toHeaders ListFacetNames' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON ListFacetNames where
  toJSON ListFacetNames' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListFacetNames where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet/list"

instance Data.ToQuery ListFacetNames where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFacetNamesResponse' smart constructor.
data ListFacetNamesResponse = ListFacetNamesResponse'
  { -- | The names of facets that exist within the schema.
    facetNames :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'facetNames', 'listFacetNamesResponse_facetNames' - The names of facets that exist within the schema.
--
-- 'nextToken', 'listFacetNamesResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listFacetNamesResponse_httpStatus' - The response's http status code.
newListFacetNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFacetNamesResponse
newListFacetNamesResponse pHttpStatus_ =
  ListFacetNamesResponse'
    { facetNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of facets that exist within the schema.
listFacetNamesResponse_facetNames :: Lens.Lens' ListFacetNamesResponse (Prelude.Maybe [Prelude.Text])
listFacetNamesResponse_facetNames = Lens.lens (\ListFacetNamesResponse' {facetNames} -> facetNames) (\s@ListFacetNamesResponse' {} a -> s {facetNames = a} :: ListFacetNamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
listFacetNamesResponse_nextToken :: Lens.Lens' ListFacetNamesResponse (Prelude.Maybe Prelude.Text)
listFacetNamesResponse_nextToken = Lens.lens (\ListFacetNamesResponse' {nextToken} -> nextToken) (\s@ListFacetNamesResponse' {} a -> s {nextToken = a} :: ListFacetNamesResponse)

-- | The response's http status code.
listFacetNamesResponse_httpStatus :: Lens.Lens' ListFacetNamesResponse Prelude.Int
listFacetNamesResponse_httpStatus = Lens.lens (\ListFacetNamesResponse' {httpStatus} -> httpStatus) (\s@ListFacetNamesResponse' {} a -> s {httpStatus = a} :: ListFacetNamesResponse)

instance Prelude.NFData ListFacetNamesResponse where
  rnf ListFacetNamesResponse' {..} =
    Prelude.rnf facetNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
