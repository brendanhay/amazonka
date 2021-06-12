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
-- Module      : Network.AWS.CloudDirectory.ListFacetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes attached to the facet.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetAttributes
  ( -- * Creating a Request
    ListFacetAttributes (..),
    newListFacetAttributes,

    -- * Request Lenses
    listFacetAttributes_nextToken,
    listFacetAttributes_maxResults,
    listFacetAttributes_schemaArn,
    listFacetAttributes_name,

    -- * Destructuring the Response
    ListFacetAttributesResponse (..),
    newListFacetAttributesResponse,

    -- * Response Lenses
    listFacetAttributesResponse_nextToken,
    listFacetAttributesResponse_attributes,
    listFacetAttributesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFacetAttributes' smart constructor.
data ListFacetAttributes = ListFacetAttributes'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ARN of the schema where the facet resides.
    schemaArn :: Core.Text,
    -- | The name of the facet whose attributes will be retrieved.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFacetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFacetAttributes_nextToken' - The pagination token.
--
-- 'maxResults', 'listFacetAttributes_maxResults' - The maximum number of results to retrieve.
--
-- 'schemaArn', 'listFacetAttributes_schemaArn' - The ARN of the schema where the facet resides.
--
-- 'name', 'listFacetAttributes_name' - The name of the facet whose attributes will be retrieved.
newListFacetAttributes ::
  -- | 'schemaArn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  ListFacetAttributes
newListFacetAttributes pSchemaArn_ pName_ =
  ListFacetAttributes'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The pagination token.
listFacetAttributes_nextToken :: Lens.Lens' ListFacetAttributes (Core.Maybe Core.Text)
listFacetAttributes_nextToken = Lens.lens (\ListFacetAttributes' {nextToken} -> nextToken) (\s@ListFacetAttributes' {} a -> s {nextToken = a} :: ListFacetAttributes)

-- | The maximum number of results to retrieve.
listFacetAttributes_maxResults :: Lens.Lens' ListFacetAttributes (Core.Maybe Core.Natural)
listFacetAttributes_maxResults = Lens.lens (\ListFacetAttributes' {maxResults} -> maxResults) (\s@ListFacetAttributes' {} a -> s {maxResults = a} :: ListFacetAttributes)

-- | The ARN of the schema where the facet resides.
listFacetAttributes_schemaArn :: Lens.Lens' ListFacetAttributes Core.Text
listFacetAttributes_schemaArn = Lens.lens (\ListFacetAttributes' {schemaArn} -> schemaArn) (\s@ListFacetAttributes' {} a -> s {schemaArn = a} :: ListFacetAttributes)

-- | The name of the facet whose attributes will be retrieved.
listFacetAttributes_name :: Lens.Lens' ListFacetAttributes Core.Text
listFacetAttributes_name = Lens.lens (\ListFacetAttributes' {name} -> name) (\s@ListFacetAttributes' {} a -> s {name = a} :: ListFacetAttributes)

instance Core.AWSPager ListFacetAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFacetAttributesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listFacetAttributesResponse_attributes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFacetAttributes_nextToken
          Lens..~ rs
          Lens.^? listFacetAttributesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListFacetAttributes where
  type
    AWSResponse ListFacetAttributes =
      ListFacetAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacetAttributesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListFacetAttributes

instance Core.NFData ListFacetAttributes

instance Core.ToHeaders ListFacetAttributes where
  toHeaders ListFacetAttributes' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON ListFacetAttributes where
  toJSON ListFacetAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath ListFacetAttributes where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/facet/attributes"

instance Core.ToQuery ListFacetAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListFacetAttributesResponse' smart constructor.
data ListFacetAttributesResponse = ListFacetAttributesResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The attributes attached to the facet.
    attributes :: Core.Maybe [FacetAttribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFacetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFacetAttributesResponse_nextToken' - The pagination token.
--
-- 'attributes', 'listFacetAttributesResponse_attributes' - The attributes attached to the facet.
--
-- 'httpStatus', 'listFacetAttributesResponse_httpStatus' - The response's http status code.
newListFacetAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFacetAttributesResponse
newListFacetAttributesResponse pHttpStatus_ =
  ListFacetAttributesResponse'
    { nextToken =
        Core.Nothing,
      attributes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listFacetAttributesResponse_nextToken :: Lens.Lens' ListFacetAttributesResponse (Core.Maybe Core.Text)
listFacetAttributesResponse_nextToken = Lens.lens (\ListFacetAttributesResponse' {nextToken} -> nextToken) (\s@ListFacetAttributesResponse' {} a -> s {nextToken = a} :: ListFacetAttributesResponse)

-- | The attributes attached to the facet.
listFacetAttributesResponse_attributes :: Lens.Lens' ListFacetAttributesResponse (Core.Maybe [FacetAttribute])
listFacetAttributesResponse_attributes = Lens.lens (\ListFacetAttributesResponse' {attributes} -> attributes) (\s@ListFacetAttributesResponse' {} a -> s {attributes = a} :: ListFacetAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFacetAttributesResponse_httpStatus :: Lens.Lens' ListFacetAttributesResponse Core.Int
listFacetAttributesResponse_httpStatus = Lens.lens (\ListFacetAttributesResponse' {httpStatus} -> httpStatus) (\s@ListFacetAttributesResponse' {} a -> s {httpStatus = a} :: ListFacetAttributesResponse)

instance Core.NFData ListFacetAttributesResponse
