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
-- Module      : Amazonka.CloudDirectory.ListObjectAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all attributes that are associated with an object.
--
-- This operation returns paginated results.
module Amazonka.CloudDirectory.ListObjectAttributes
  ( -- * Creating a Request
    ListObjectAttributes (..),
    newListObjectAttributes,

    -- * Request Lenses
    listObjectAttributes_nextToken,
    listObjectAttributes_consistencyLevel,
    listObjectAttributes_maxResults,
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListObjectAttributes' smart constructor.
data ListObjectAttributes = ListObjectAttributes'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the manner and timing in which the successful write or update
    -- of an object is reflected in a subsequent read operation of that same
    -- object.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Used to filter the list of object attributes that are associated with a
    -- certain facet.
    facetFilter :: Prelude.Maybe SchemaFacet,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The reference that identifies the object whose attributes will be
    -- listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'consistencyLevel', 'listObjectAttributes_consistencyLevel' - Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
--
-- 'maxResults', 'listObjectAttributes_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
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
  Prelude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectAttributes
newListObjectAttributes
  pDirectoryArn_
  pObjectReference_ =
    ListObjectAttributes'
      { nextToken = Prelude.Nothing,
        consistencyLevel = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        facetFilter = Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_
      }

-- | The pagination token.
listObjectAttributes_nextToken :: Lens.Lens' ListObjectAttributes (Prelude.Maybe Prelude.Text)
listObjectAttributes_nextToken = Lens.lens (\ListObjectAttributes' {nextToken} -> nextToken) (\s@ListObjectAttributes' {} a -> s {nextToken = a} :: ListObjectAttributes)

-- | Represents the manner and timing in which the successful write or update
-- of an object is reflected in a subsequent read operation of that same
-- object.
listObjectAttributes_consistencyLevel :: Lens.Lens' ListObjectAttributes (Prelude.Maybe ConsistencyLevel)
listObjectAttributes_consistencyLevel = Lens.lens (\ListObjectAttributes' {consistencyLevel} -> consistencyLevel) (\s@ListObjectAttributes' {} a -> s {consistencyLevel = a} :: ListObjectAttributes)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
listObjectAttributes_maxResults :: Lens.Lens' ListObjectAttributes (Prelude.Maybe Prelude.Natural)
listObjectAttributes_maxResults = Lens.lens (\ListObjectAttributes' {maxResults} -> maxResults) (\s@ListObjectAttributes' {} a -> s {maxResults = a} :: ListObjectAttributes)

-- | Used to filter the list of object attributes that are associated with a
-- certain facet.
listObjectAttributes_facetFilter :: Lens.Lens' ListObjectAttributes (Prelude.Maybe SchemaFacet)
listObjectAttributes_facetFilter = Lens.lens (\ListObjectAttributes' {facetFilter} -> facetFilter) (\s@ListObjectAttributes' {} a -> s {facetFilter = a} :: ListObjectAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
listObjectAttributes_directoryArn :: Lens.Lens' ListObjectAttributes Prelude.Text
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
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listObjectAttributesResponse_attributes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listObjectAttributes_nextToken
          Lens..~ rs
          Lens.^? listObjectAttributesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListObjectAttributes where
  type
    AWSResponse ListObjectAttributes =
      ListObjectAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectAttributesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjectAttributes where
  hashWithSalt _salt ListObjectAttributes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` facetFilter
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData ListObjectAttributes where
  rnf ListObjectAttributes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf facetFilter
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference

instance Core.ToHeaders ListObjectAttributes where
  toHeaders ListObjectAttributes' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON ListObjectAttributes where
  toJSON ListObjectAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("FacetFilter" Core..=) Prelude.<$> facetFilter,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )

instance Core.ToPath ListObjectAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/attributes"

instance Core.ToQuery ListObjectAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListObjectAttributesResponse' smart constructor.
data ListObjectAttributesResponse = ListObjectAttributesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Attributes map that is associated with the object. @AttributeArn@ is the
    -- key, and attribute value is the value.
    attributes :: Prelude.Maybe [AttributeKeyAndValue],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListObjectAttributesResponse
newListObjectAttributesResponse pHttpStatus_ =
  ListObjectAttributesResponse'
    { nextToken =
        Prelude.Nothing,
      attributes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listObjectAttributesResponse_nextToken :: Lens.Lens' ListObjectAttributesResponse (Prelude.Maybe Prelude.Text)
listObjectAttributesResponse_nextToken = Lens.lens (\ListObjectAttributesResponse' {nextToken} -> nextToken) (\s@ListObjectAttributesResponse' {} a -> s {nextToken = a} :: ListObjectAttributesResponse)

-- | Attributes map that is associated with the object. @AttributeArn@ is the
-- key, and attribute value is the value.
listObjectAttributesResponse_attributes :: Lens.Lens' ListObjectAttributesResponse (Prelude.Maybe [AttributeKeyAndValue])
listObjectAttributesResponse_attributes = Lens.lens (\ListObjectAttributesResponse' {attributes} -> attributes) (\s@ListObjectAttributesResponse' {} a -> s {attributes = a} :: ListObjectAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listObjectAttributesResponse_httpStatus :: Lens.Lens' ListObjectAttributesResponse Prelude.Int
listObjectAttributesResponse_httpStatus = Lens.lens (\ListObjectAttributesResponse' {httpStatus} -> httpStatus) (\s@ListObjectAttributesResponse' {} a -> s {httpStatus = a} :: ListObjectAttributesResponse)

instance Prelude.NFData ListObjectAttributesResponse where
  rnf ListObjectAttributesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
