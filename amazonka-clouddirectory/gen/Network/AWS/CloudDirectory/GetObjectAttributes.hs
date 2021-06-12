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
-- Module      : Network.AWS.CloudDirectory.GetObjectAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes within a facet that are associated with an object.
module Network.AWS.CloudDirectory.GetObjectAttributes
  ( -- * Creating a Request
    GetObjectAttributes (..),
    newGetObjectAttributes,

    -- * Request Lenses
    getObjectAttributes_consistencyLevel,
    getObjectAttributes_directoryArn,
    getObjectAttributes_objectReference,
    getObjectAttributes_schemaFacet,
    getObjectAttributes_attributeNames,

    -- * Destructuring the Response
    GetObjectAttributesResponse (..),
    newGetObjectAttributesResponse,

    -- * Response Lenses
    getObjectAttributesResponse_attributes,
    getObjectAttributesResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetObjectAttributes' smart constructor.
data GetObjectAttributes = GetObjectAttributes'
  { -- | The consistency level at which to retrieve the attributes on an object.
    consistencyLevel :: Core.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides.
    directoryArn :: Core.Text,
    -- | Reference that identifies the object whose attributes will be retrieved.
    objectReference :: ObjectReference,
    -- | Identifier for the facet whose attributes will be retrieved. See
    -- SchemaFacet for details.
    schemaFacet :: SchemaFacet,
    -- | List of attribute names whose values will be retrieved.
    attributeNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistencyLevel', 'getObjectAttributes_consistencyLevel' - The consistency level at which to retrieve the attributes on an object.
--
-- 'directoryArn', 'getObjectAttributes_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides.
--
-- 'objectReference', 'getObjectAttributes_objectReference' - Reference that identifies the object whose attributes will be retrieved.
--
-- 'schemaFacet', 'getObjectAttributes_schemaFacet' - Identifier for the facet whose attributes will be retrieved. See
-- SchemaFacet for details.
--
-- 'attributeNames', 'getObjectAttributes_attributeNames' - List of attribute names whose values will be retrieved.
newGetObjectAttributes ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'objectReference'
  ObjectReference ->
  -- | 'schemaFacet'
  SchemaFacet ->
  GetObjectAttributes
newGetObjectAttributes
  pDirectoryArn_
  pObjectReference_
  pSchemaFacet_ =
    GetObjectAttributes'
      { consistencyLevel =
          Core.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_,
        schemaFacet = pSchemaFacet_,
        attributeNames = Core.mempty
      }

-- | The consistency level at which to retrieve the attributes on an object.
getObjectAttributes_consistencyLevel :: Lens.Lens' GetObjectAttributes (Core.Maybe ConsistencyLevel)
getObjectAttributes_consistencyLevel = Lens.lens (\GetObjectAttributes' {consistencyLevel} -> consistencyLevel) (\s@GetObjectAttributes' {} a -> s {consistencyLevel = a} :: GetObjectAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides.
getObjectAttributes_directoryArn :: Lens.Lens' GetObjectAttributes Core.Text
getObjectAttributes_directoryArn = Lens.lens (\GetObjectAttributes' {directoryArn} -> directoryArn) (\s@GetObjectAttributes' {} a -> s {directoryArn = a} :: GetObjectAttributes)

-- | Reference that identifies the object whose attributes will be retrieved.
getObjectAttributes_objectReference :: Lens.Lens' GetObjectAttributes ObjectReference
getObjectAttributes_objectReference = Lens.lens (\GetObjectAttributes' {objectReference} -> objectReference) (\s@GetObjectAttributes' {} a -> s {objectReference = a} :: GetObjectAttributes)

-- | Identifier for the facet whose attributes will be retrieved. See
-- SchemaFacet for details.
getObjectAttributes_schemaFacet :: Lens.Lens' GetObjectAttributes SchemaFacet
getObjectAttributes_schemaFacet = Lens.lens (\GetObjectAttributes' {schemaFacet} -> schemaFacet) (\s@GetObjectAttributes' {} a -> s {schemaFacet = a} :: GetObjectAttributes)

-- | List of attribute names whose values will be retrieved.
getObjectAttributes_attributeNames :: Lens.Lens' GetObjectAttributes [Core.Text]
getObjectAttributes_attributeNames = Lens.lens (\GetObjectAttributes' {attributeNames} -> attributeNames) (\s@GetObjectAttributes' {} a -> s {attributeNames = a} :: GetObjectAttributes) Core.. Lens._Coerce

instance Core.AWSRequest GetObjectAttributes where
  type
    AWSResponse GetObjectAttributes =
      GetObjectAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetObjectAttributesResponse'
            Core.<$> (x Core..?> "Attributes" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetObjectAttributes

instance Core.NFData GetObjectAttributes

instance Core.ToHeaders GetObjectAttributes where
  toHeaders GetObjectAttributes' {..} =
    Core.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON GetObjectAttributes where
  toJSON GetObjectAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ObjectReference" Core..= objectReference),
            Core.Just ("SchemaFacet" Core..= schemaFacet),
            Core.Just ("AttributeNames" Core..= attributeNames)
          ]
      )

instance Core.ToPath GetObjectAttributes where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/attributes/get"

instance Core.ToQuery GetObjectAttributes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetObjectAttributesResponse' smart constructor.
data GetObjectAttributesResponse = GetObjectAttributesResponse'
  { -- | The attributes that are associated with the object.
    attributes :: Core.Maybe [AttributeKeyAndValue],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getObjectAttributesResponse_attributes' - The attributes that are associated with the object.
--
-- 'httpStatus', 'getObjectAttributesResponse_httpStatus' - The response's http status code.
newGetObjectAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetObjectAttributesResponse
newGetObjectAttributesResponse pHttpStatus_ =
  GetObjectAttributesResponse'
    { attributes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes that are associated with the object.
getObjectAttributesResponse_attributes :: Lens.Lens' GetObjectAttributesResponse (Core.Maybe [AttributeKeyAndValue])
getObjectAttributesResponse_attributes = Lens.lens (\GetObjectAttributesResponse' {attributes} -> attributes) (\s@GetObjectAttributesResponse' {} a -> s {attributes = a} :: GetObjectAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getObjectAttributesResponse_httpStatus :: Lens.Lens' GetObjectAttributesResponse Core.Int
getObjectAttributesResponse_httpStatus = Lens.lens (\GetObjectAttributesResponse' {httpStatus} -> httpStatus) (\s@GetObjectAttributesResponse' {} a -> s {httpStatus = a} :: GetObjectAttributesResponse)

instance Core.NFData GetObjectAttributesResponse
