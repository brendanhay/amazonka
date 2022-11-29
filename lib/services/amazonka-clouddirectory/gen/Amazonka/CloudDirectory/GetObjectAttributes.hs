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
-- Module      : Amazonka.CloudDirectory.GetObjectAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes within a facet that are associated with an object.
module Amazonka.CloudDirectory.GetObjectAttributes
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetObjectAttributes' smart constructor.
data GetObjectAttributes = GetObjectAttributes'
  { -- | The consistency level at which to retrieve the attributes on an object.
    consistencyLevel :: Prelude.Maybe ConsistencyLevel,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides.
    directoryArn :: Prelude.Text,
    -- | Reference that identifies the object whose attributes will be retrieved.
    objectReference :: ObjectReference,
    -- | Identifier for the facet whose attributes will be retrieved. See
    -- SchemaFacet for details.
    schemaFacet :: SchemaFacet,
    -- | List of attribute names whose values will be retrieved.
    attributeNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
          Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        objectReference = pObjectReference_,
        schemaFacet = pSchemaFacet_,
        attributeNames = Prelude.mempty
      }

-- | The consistency level at which to retrieve the attributes on an object.
getObjectAttributes_consistencyLevel :: Lens.Lens' GetObjectAttributes (Prelude.Maybe ConsistencyLevel)
getObjectAttributes_consistencyLevel = Lens.lens (\GetObjectAttributes' {consistencyLevel} -> consistencyLevel) (\s@GetObjectAttributes' {} a -> s {consistencyLevel = a} :: GetObjectAttributes)

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides.
getObjectAttributes_directoryArn :: Lens.Lens' GetObjectAttributes Prelude.Text
getObjectAttributes_directoryArn = Lens.lens (\GetObjectAttributes' {directoryArn} -> directoryArn) (\s@GetObjectAttributes' {} a -> s {directoryArn = a} :: GetObjectAttributes)

-- | Reference that identifies the object whose attributes will be retrieved.
getObjectAttributes_objectReference :: Lens.Lens' GetObjectAttributes ObjectReference
getObjectAttributes_objectReference = Lens.lens (\GetObjectAttributes' {objectReference} -> objectReference) (\s@GetObjectAttributes' {} a -> s {objectReference = a} :: GetObjectAttributes)

-- | Identifier for the facet whose attributes will be retrieved. See
-- SchemaFacet for details.
getObjectAttributes_schemaFacet :: Lens.Lens' GetObjectAttributes SchemaFacet
getObjectAttributes_schemaFacet = Lens.lens (\GetObjectAttributes' {schemaFacet} -> schemaFacet) (\s@GetObjectAttributes' {} a -> s {schemaFacet = a} :: GetObjectAttributes)

-- | List of attribute names whose values will be retrieved.
getObjectAttributes_attributeNames :: Lens.Lens' GetObjectAttributes [Prelude.Text]
getObjectAttributes_attributeNames = Lens.lens (\GetObjectAttributes' {attributeNames} -> attributeNames) (\s@GetObjectAttributes' {} a -> s {attributeNames = a} :: GetObjectAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest GetObjectAttributes where
  type
    AWSResponse GetObjectAttributes =
      GetObjectAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetObjectAttributesResponse'
            Prelude.<$> (x Core..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectAttributes where
  hashWithSalt _salt GetObjectAttributes' {..} =
    _salt `Prelude.hashWithSalt` consistencyLevel
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` objectReference
      `Prelude.hashWithSalt` schemaFacet
      `Prelude.hashWithSalt` attributeNames

instance Prelude.NFData GetObjectAttributes where
  rnf GetObjectAttributes' {..} =
    Prelude.rnf consistencyLevel
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf objectReference
      `Prelude.seq` Prelude.rnf schemaFacet
      `Prelude.seq` Prelude.rnf attributeNames

instance Core.ToHeaders GetObjectAttributes where
  toHeaders GetObjectAttributes' {..} =
    Prelude.mconcat
      [ "x-amz-consistency-level" Core.=# consistencyLevel,
        "x-amz-data-partition" Core.=# directoryArn
      ]

instance Core.ToJSON GetObjectAttributes where
  toJSON GetObjectAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Core..= objectReference),
            Prelude.Just ("SchemaFacet" Core..= schemaFacet),
            Prelude.Just
              ("AttributeNames" Core..= attributeNames)
          ]
      )

instance Core.ToPath GetObjectAttributes where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/attributes/get"

instance Core.ToQuery GetObjectAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetObjectAttributesResponse' smart constructor.
data GetObjectAttributesResponse = GetObjectAttributesResponse'
  { -- | The attributes that are associated with the object.
    attributes :: Prelude.Maybe [AttributeKeyAndValue],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetObjectAttributesResponse
newGetObjectAttributesResponse pHttpStatus_ =
  GetObjectAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes that are associated with the object.
getObjectAttributesResponse_attributes :: Lens.Lens' GetObjectAttributesResponse (Prelude.Maybe [AttributeKeyAndValue])
getObjectAttributesResponse_attributes = Lens.lens (\GetObjectAttributesResponse' {attributes} -> attributes) (\s@GetObjectAttributesResponse' {} a -> s {attributes = a} :: GetObjectAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getObjectAttributesResponse_httpStatus :: Lens.Lens' GetObjectAttributesResponse Prelude.Int
getObjectAttributesResponse_httpStatus = Lens.lens (\GetObjectAttributesResponse' {httpStatus} -> httpStatus) (\s@GetObjectAttributesResponse' {} a -> s {httpStatus = a} :: GetObjectAttributesResponse)

instance Prelude.NFData GetObjectAttributesResponse where
  rnf GetObjectAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
