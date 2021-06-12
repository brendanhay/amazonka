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
-- Module      : Network.AWS.CloudDirectory.ListObjectAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all attributes that are associated with an object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectAttributes
  ( -- * Creating a Request
    ListObjectAttributes (..),
    newListObjectAttributes,

    -- * Request Lenses
    listObjectAttributes_nextToken,
    listObjectAttributes_maxResults,
    listObjectAttributes_consistencyLevel,
    listObjectAttributes_facetFilter,
    listObjectAttributes_directoryArn,
    listObjectAttributes_objectReference,

    -- * Destructuring the Response
    ListObjectAttributesResponse (..),
    newListObjectAttributesResponse,

    -- * Response Lenses
    listObjectAttributesResponse_nextToken,
    listObjectAttributesResponse_attributes,
    listObjectAttributesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListObjectAttributes' smart constructor.
data ListObjectAttributes = ListObjectAttributes'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | Used to filter the list of object attributes that are associated with a
    -- certain facet.
    facetFilter :: Core.Maybe SchemaFacet,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Core.Text,
    -- | The reference that identifies the object whose attributes will be
    -- listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectAttributes_nextToken' - The pagination token.
--
-- 'maxResults', 'listObjectAttributes_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'consistencyLevel', 'listObjectAttributes_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'facetFilter', 'listObjectAttributes_facetFilter' - Used to filter the list of object attributes that are associated with a
-- certain facet.
--
-- 'directoryArn', 'listObjectAttributes_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'objectReference', 'listObjectAttributes_objectReference' - The reference that identifies the object whose attributes will be
-- listed.
newListObjectAttributes ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectAttributes
newListObjectAttributes
  pDirectoryArn_
  pObjectReference_ =
    ListObjectAttributes'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        consistencyLevel = Core.Nothing,
        facetFilter = Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listObjectAttributes_nextToken :: Lens.Lens' ListObjectAttributes (Core.Maybe Core.Text)
listObjectAttributes_nextToken = Lens.lens (\ListObjectAttributes' {nextToken} -> nextToken) (\s@ListObjectAttributes' {} a -> s {nextToken = a} :: ListObjectAttributes)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectAttributes_maxResults :: Lens.Lens' ListObjectAttributes (Core.Maybe Core.Natural)
listObjectAttributes_maxResults = Lens.lens (\ListObjectAttributes' {maxResults} -> maxResults) (\s@ListObjectAttributes' {} a -> s {maxResults = a} :: ListObjectAttributes)

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectAttributes_consistencyLevel :: Lens.Lens' ListObjectAttributes (Core.Maybe ConsistencyLevel)
listObjectAttributes_consistencyLevel = Lens.lens (\ListObjectAttributes' {consistencyLevel} -> consistencyLevel) (\s@ListObjectAttributes' {} a -> s {consistencyLevel = a} :: ListObjectAttributes)

-- | Used to filter the list of object attributes that are associated with a
-- certain facet.
listObjectAttributes_facetFilter :: Lens.Lens' ListObjectAttributes (Core.Maybe SchemaFacet)
listObjectAttributes_facetFilter = Lens.lens (\ListObjectAttributes' {facetFilter} -> facetFilter) (\s@ListObjectAttributes' {} a -> s {facetFilter = a} :: ListObjectAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
listObjectAttributes_directoryArn :: Lens.Lens' ListObjectAttributes Core.Text
listObjectAttributes_directoryArn = Lens.lens (\ListObjectAttributes' {directoryArn} -> directoryArn) (\s@ListObjectAttributes' {} a -> s {directoryArn = a} :: ListObjectAttributes)

-- | The reference that identifies the object whose attributes will be
-- listed.
listObjectAttributes_objectReference :: Lens.Lens' ListObjectAttributes ObjectReference
listObjectAttributes_objectReference = Lens.lens (\ListObjectAttributes' {objectReference} -> objectReference) (\s@ListObjectAttributes' {} a -> s {objectReference = a} :: ListObjectAttributes)

instance Core.AWSPager ListObjectAttributes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listObjectAttributesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listObjectAttributesResponse_attributes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listObjectAttributes_nextToken
          Lens..~ rs
          Lens.^? listObjectAttributesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListObjectAttributes where
  type
    AWSResponse ListObjectAttributes =
      ListObjectAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectAttributesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListObjectAttributes

instance Core.NFData ListObjectAttributes

instance Core.ToHeaders ListObjectAttributes where
  toHeaders ListObjectAttributes' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListObjectAttributes where
  toJSON ListObjectAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("FacetFilter" Core..=) Core.<$> facetFilter,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListObjectAttributes where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/attributes"

instance Core.ToQuery ListObjectAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListObjectAttributesResponse' smart constructor.
data ListObjectAttributesResponse = ListObjectAttributesResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Attributes map that is associated with the object. @AttributeArn@ is the
    -- key, and attribute value is the value.
    attributes :: Core.Maybe [AttributeKeyAndValue],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObjectAttributesResponse_nextToken' - The pagination token.
--
-- 'attributes', 'listObjectAttributesResponse_attributes' - Attributes map that is associated with the object. @AttributeArn@ is the
-- key, and attribute value is the value.
--
-- 'httpStatus', 'listObjectAttributesResponse_httpStatus' - The response's http status code.
newListObjectAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListObjectAttributesResponse
newListObjectAttributesResponse pHttpStatus_ =
  ListObjectAttributesResponse'
    { nextToken =
        Core.Nothing,
      attributes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listObjectAttributesResponse_nextToken :: Lens.Lens' ListObjectAttributesResponse (Core.Maybe Core.Text)
listObjectAttributesResponse_nextToken = Lens.lens (\ListObjectAttributesResponse' {nextToken} -> nextToken) (\s@ListObjectAttributesResponse' {} a -> s {nextToken = a} :: ListObjectAttributesResponse)

-- | Attributes map that is associated with the object. @AttributeArn@ is the
-- key, and attribute value is the value.
listObjectAttributesResponse_attributes :: Lens.Lens' ListObjectAttributesResponse (Core.Maybe [AttributeKeyAndValue])
listObjectAttributesResponse_attributes = Lens.lens (\ListObjectAttributesResponse' {attributes} -> attributes) (\s@ListObjectAttributesResponse' {} a -> s {attributes = a} :: ListObjectAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listObjectAttributesResponse_httpStatus :: Lens.Lens' ListObjectAttributesResponse Core.Int
listObjectAttributesResponse_httpStatus = Lens.lens (\ListObjectAttributesResponse' {httpStatus} -> httpStatus) (\s@ListObjectAttributesResponse' {} a -> s {httpStatus = a} :: ListObjectAttributesResponse)

instance Core.NFData ListObjectAttributesResponse
