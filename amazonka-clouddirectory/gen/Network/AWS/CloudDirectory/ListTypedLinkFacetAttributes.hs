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
-- Module      : Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute definitions for a particular
-- TypedLinkFacet. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
  ( -- * Creating a Request
    ListTypedLinkFacetAttributes (..),
    newListTypedLinkFacetAttributes,

    -- * Request Lenses
    listTypedLinkFacetAttributes_nextToken,
    listTypedLinkFacetAttributes_maxResults,
    listTypedLinkFacetAttributes_schemaArn,
    listTypedLinkFacetAttributes_name,

    -- * Destructuring the Response
    ListTypedLinkFacetAttributesResponse (..),
    newListTypedLinkFacetAttributesResponse,

    -- * Response Lenses
    listTypedLinkFacetAttributesResponse_nextToken,
    listTypedLinkFacetAttributesResponse_attributes,
    listTypedLinkFacetAttributesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTypedLinkFacetAttributes' smart constructor.
data ListTypedLinkFacetAttributes = ListTypedLinkFacetAttributes'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Core.Text,
    -- | The unique name of the typed link facet.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTypedLinkFacetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypedLinkFacetAttributes_nextToken' - The pagination token.
--
-- 'maxResults', 'listTypedLinkFacetAttributes_maxResults' - The maximum number of results to retrieve.
--
-- 'schemaArn', 'listTypedLinkFacetAttributes_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
--
-- 'name', 'listTypedLinkFacetAttributes_name' - The unique name of the typed link facet.
newListTypedLinkFacetAttributes ::
  -- | 'schemaArn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  ListTypedLinkFacetAttributes
newListTypedLinkFacetAttributes pSchemaArn_ pName_ =
  ListTypedLinkFacetAttributes'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The pagination token.
listTypedLinkFacetAttributes_nextToken :: Lens.Lens' ListTypedLinkFacetAttributes (Core.Maybe Core.Text)
listTypedLinkFacetAttributes_nextToken = Lens.lens (\ListTypedLinkFacetAttributes' {nextToken} -> nextToken) (\s@ListTypedLinkFacetAttributes' {} a -> s {nextToken = a} :: ListTypedLinkFacetAttributes)

-- | The maximum number of results to retrieve.
listTypedLinkFacetAttributes_maxResults :: Lens.Lens' ListTypedLinkFacetAttributes (Core.Maybe Core.Natural)
listTypedLinkFacetAttributes_maxResults = Lens.lens (\ListTypedLinkFacetAttributes' {maxResults} -> maxResults) (\s@ListTypedLinkFacetAttributes' {} a -> s {maxResults = a} :: ListTypedLinkFacetAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
listTypedLinkFacetAttributes_schemaArn :: Lens.Lens' ListTypedLinkFacetAttributes Core.Text
listTypedLinkFacetAttributes_schemaArn = Lens.lens (\ListTypedLinkFacetAttributes' {schemaArn} -> schemaArn) (\s@ListTypedLinkFacetAttributes' {} a -> s {schemaArn = a} :: ListTypedLinkFacetAttributes)

-- | The unique name of the typed link facet.
listTypedLinkFacetAttributes_name :: Lens.Lens' ListTypedLinkFacetAttributes Core.Text
listTypedLinkFacetAttributes_name = Lens.lens (\ListTypedLinkFacetAttributes' {name} -> name) (\s@ListTypedLinkFacetAttributes' {} a -> s {name = a} :: ListTypedLinkFacetAttributes)

instance Core.AWSPager ListTypedLinkFacetAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTypedLinkFacetAttributesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTypedLinkFacetAttributesResponse_attributes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTypedLinkFacetAttributes_nextToken
          Lens..~ rs
          Lens.^? listTypedLinkFacetAttributesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListTypedLinkFacetAttributes where
  type
    AWSResponse ListTypedLinkFacetAttributes =
      ListTypedLinkFacetAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTypedLinkFacetAttributesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTypedLinkFacetAttributes

instance Core.NFData ListTypedLinkFacetAttributes

instance Core.ToHeaders ListTypedLinkFacetAttributes where
  toHeaders ListTypedLinkFacetAttributes' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON ListTypedLinkFacetAttributes where
  toJSON ListTypedLinkFacetAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath ListTypedLinkFacetAttributes where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/attributes"

instance Core.ToQuery ListTypedLinkFacetAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTypedLinkFacetAttributesResponse' smart constructor.
data ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | An ordered set of attributes associate with the typed link.
    attributes :: Core.Maybe [TypedLinkAttributeDefinition],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTypedLinkFacetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypedLinkFacetAttributesResponse_nextToken' - The pagination token.
--
-- 'attributes', 'listTypedLinkFacetAttributesResponse_attributes' - An ordered set of attributes associate with the typed link.
--
-- 'httpStatus', 'listTypedLinkFacetAttributesResponse_httpStatus' - The response's http status code.
newListTypedLinkFacetAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTypedLinkFacetAttributesResponse
newListTypedLinkFacetAttributesResponse pHttpStatus_ =
  ListTypedLinkFacetAttributesResponse'
    { nextToken =
        Core.Nothing,
      attributes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listTypedLinkFacetAttributesResponse_nextToken :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Core.Maybe Core.Text)
listTypedLinkFacetAttributesResponse_nextToken = Lens.lens (\ListTypedLinkFacetAttributesResponse' {nextToken} -> nextToken) (\s@ListTypedLinkFacetAttributesResponse' {} a -> s {nextToken = a} :: ListTypedLinkFacetAttributesResponse)

-- | An ordered set of attributes associate with the typed link.
listTypedLinkFacetAttributesResponse_attributes :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Core.Maybe [TypedLinkAttributeDefinition])
listTypedLinkFacetAttributesResponse_attributes = Lens.lens (\ListTypedLinkFacetAttributesResponse' {attributes} -> attributes) (\s@ListTypedLinkFacetAttributesResponse' {} a -> s {attributes = a} :: ListTypedLinkFacetAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTypedLinkFacetAttributesResponse_httpStatus :: Lens.Lens' ListTypedLinkFacetAttributesResponse Core.Int
listTypedLinkFacetAttributesResponse_httpStatus = Lens.lens (\ListTypedLinkFacetAttributesResponse' {httpStatus} -> httpStatus) (\s@ListTypedLinkFacetAttributesResponse' {} a -> s {httpStatus = a} :: ListTypedLinkFacetAttributesResponse)

instance
  Core.NFData
    ListTypedLinkFacetAttributesResponse
