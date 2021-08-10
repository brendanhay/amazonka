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
-- Module      : Network.AWS.CloudDirectory.ListTypedLinkFacetNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of @TypedLink@ facet names for a particular
-- schema. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTypedLinkFacetNames
  ( -- * Creating a Request
    ListTypedLinkFacetNames (..),
    newListTypedLinkFacetNames,

    -- * Request Lenses
    listTypedLinkFacetNames_nextToken,
    listTypedLinkFacetNames_maxResults,
    listTypedLinkFacetNames_schemaArn,

    -- * Destructuring the Response
    ListTypedLinkFacetNamesResponse (..),
    newListTypedLinkFacetNamesResponse,

    -- * Response Lenses
    listTypedLinkFacetNamesResponse_nextToken,
    listTypedLinkFacetNamesResponse_facetNames,
    listTypedLinkFacetNamesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTypedLinkFacetNames' smart constructor.
data ListTypedLinkFacetNames = ListTypedLinkFacetNames'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypedLinkFacetNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypedLinkFacetNames_nextToken' - The pagination token.
--
-- 'maxResults', 'listTypedLinkFacetNames_maxResults' - The maximum number of results to retrieve.
--
-- 'schemaArn', 'listTypedLinkFacetNames_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
newListTypedLinkFacetNames ::
  -- | 'schemaArn'
  Prelude.Text ->
  ListTypedLinkFacetNames
newListTypedLinkFacetNames pSchemaArn_ =
  ListTypedLinkFacetNames'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      schemaArn = pSchemaArn_
    }

-- | The pagination token.
listTypedLinkFacetNames_nextToken :: Lens.Lens' ListTypedLinkFacetNames (Prelude.Maybe Prelude.Text)
listTypedLinkFacetNames_nextToken = Lens.lens (\ListTypedLinkFacetNames' {nextToken} -> nextToken) (\s@ListTypedLinkFacetNames' {} a -> s {nextToken = a} :: ListTypedLinkFacetNames)

-- | The maximum number of results to retrieve.
listTypedLinkFacetNames_maxResults :: Lens.Lens' ListTypedLinkFacetNames (Prelude.Maybe Prelude.Natural)
listTypedLinkFacetNames_maxResults = Lens.lens (\ListTypedLinkFacetNames' {maxResults} -> maxResults) (\s@ListTypedLinkFacetNames' {} a -> s {maxResults = a} :: ListTypedLinkFacetNames)

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
listTypedLinkFacetNames_schemaArn :: Lens.Lens' ListTypedLinkFacetNames Prelude.Text
listTypedLinkFacetNames_schemaArn = Lens.lens (\ListTypedLinkFacetNames' {schemaArn} -> schemaArn) (\s@ListTypedLinkFacetNames' {} a -> s {schemaArn = a} :: ListTypedLinkFacetNames)

instance Core.AWSPager ListTypedLinkFacetNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTypedLinkFacetNamesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTypedLinkFacetNamesResponse_facetNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTypedLinkFacetNames_nextToken
          Lens..~ rs
          Lens.^? listTypedLinkFacetNamesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTypedLinkFacetNames where
  type
    AWSResponse ListTypedLinkFacetNames =
      ListTypedLinkFacetNamesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTypedLinkFacetNamesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "FacetNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTypedLinkFacetNames

instance Prelude.NFData ListTypedLinkFacetNames

instance Core.ToHeaders ListTypedLinkFacetNames where
  toHeaders ListTypedLinkFacetNames' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON ListTypedLinkFacetNames where
  toJSON ListTypedLinkFacetNames' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListTypedLinkFacetNames where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/list"

instance Core.ToQuery ListTypedLinkFacetNames where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTypedLinkFacetNamesResponse' smart constructor.
data ListTypedLinkFacetNamesResponse = ListTypedLinkFacetNamesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The names of typed link facets that exist within the schema.
    facetNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTypedLinkFacetNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypedLinkFacetNamesResponse_nextToken' - The pagination token.
--
-- 'facetNames', 'listTypedLinkFacetNamesResponse_facetNames' - The names of typed link facets that exist within the schema.
--
-- 'httpStatus', 'listTypedLinkFacetNamesResponse_httpStatus' - The response's http status code.
newListTypedLinkFacetNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTypedLinkFacetNamesResponse
newListTypedLinkFacetNamesResponse pHttpStatus_ =
  ListTypedLinkFacetNamesResponse'
    { nextToken =
        Prelude.Nothing,
      facetNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listTypedLinkFacetNamesResponse_nextToken :: Lens.Lens' ListTypedLinkFacetNamesResponse (Prelude.Maybe Prelude.Text)
listTypedLinkFacetNamesResponse_nextToken = Lens.lens (\ListTypedLinkFacetNamesResponse' {nextToken} -> nextToken) (\s@ListTypedLinkFacetNamesResponse' {} a -> s {nextToken = a} :: ListTypedLinkFacetNamesResponse)

-- | The names of typed link facets that exist within the schema.
listTypedLinkFacetNamesResponse_facetNames :: Lens.Lens' ListTypedLinkFacetNamesResponse (Prelude.Maybe [Prelude.Text])
listTypedLinkFacetNamesResponse_facetNames = Lens.lens (\ListTypedLinkFacetNamesResponse' {facetNames} -> facetNames) (\s@ListTypedLinkFacetNamesResponse' {} a -> s {facetNames = a} :: ListTypedLinkFacetNamesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTypedLinkFacetNamesResponse_httpStatus :: Lens.Lens' ListTypedLinkFacetNamesResponse Prelude.Int
listTypedLinkFacetNamesResponse_httpStatus = Lens.lens (\ListTypedLinkFacetNamesResponse' {httpStatus} -> httpStatus) (\s@ListTypedLinkFacetNamesResponse' {} a -> s {httpStatus = a} :: ListTypedLinkFacetNamesResponse)

instance
  Prelude.NFData
    ListTypedLinkFacetNamesResponse
