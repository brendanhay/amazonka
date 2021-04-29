{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.AddFacetToObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new Facet to an object. An object can have more than one facet
-- applied on it.
module Network.AWS.CloudDirectory.AddFacetToObject
  ( -- * Creating a Request
    AddFacetToObject (..),
    newAddFacetToObject,

    -- * Request Lenses
    addFacetToObject_objectAttributeList,
    addFacetToObject_directoryArn,
    addFacetToObject_schemaFacet,
    addFacetToObject_objectReference,

    -- * Destructuring the Response
    AddFacetToObjectResponse (..),
    newAddFacetToObjectResponse,

    -- * Response Lenses
    addFacetToObjectResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddFacetToObject' smart constructor.
data AddFacetToObject = AddFacetToObject'
  { -- | Attributes on the facet that you are adding to the object.
    objectAttributeList :: Prelude.Maybe [AttributeKeyAndValue],
    -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where the object resides. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | Identifiers for the facet that you are adding to the object. See
    -- SchemaFacet for details.
    schemaFacet :: SchemaFacet,
    -- | A reference to the object you are adding the specified facet to.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddFacetToObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectAttributeList', 'addFacetToObject_objectAttributeList' - Attributes on the facet that you are adding to the object.
--
-- 'directoryArn', 'addFacetToObject_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
--
-- 'schemaFacet', 'addFacetToObject_schemaFacet' - Identifiers for the facet that you are adding to the object. See
-- SchemaFacet for details.
--
-- 'objectReference', 'addFacetToObject_objectReference' - A reference to the object you are adding the specified facet to.
newAddFacetToObject ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  AddFacetToObject
newAddFacetToObject
  pDirectoryArn_
  pSchemaFacet_
  pObjectReference_ =
    AddFacetToObject'
      { objectAttributeList =
          Prelude.Nothing,
        directoryArn = pDirectoryArn_,
        schemaFacet = pSchemaFacet_,
        objectReference = pObjectReference_
      }

-- | Attributes on the facet that you are adding to the object.
addFacetToObject_objectAttributeList :: Lens.Lens' AddFacetToObject (Prelude.Maybe [AttributeKeyAndValue])
addFacetToObject_objectAttributeList = Lens.lens (\AddFacetToObject' {objectAttributeList} -> objectAttributeList) (\s@AddFacetToObject' {} a -> s {objectAttributeList = a} :: AddFacetToObject) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where the object resides. For more information, see arns.
addFacetToObject_directoryArn :: Lens.Lens' AddFacetToObject Prelude.Text
addFacetToObject_directoryArn = Lens.lens (\AddFacetToObject' {directoryArn} -> directoryArn) (\s@AddFacetToObject' {} a -> s {directoryArn = a} :: AddFacetToObject)

-- | Identifiers for the facet that you are adding to the object. See
-- SchemaFacet for details.
addFacetToObject_schemaFacet :: Lens.Lens' AddFacetToObject SchemaFacet
addFacetToObject_schemaFacet = Lens.lens (\AddFacetToObject' {schemaFacet} -> schemaFacet) (\s@AddFacetToObject' {} a -> s {schemaFacet = a} :: AddFacetToObject)

-- | A reference to the object you are adding the specified facet to.
addFacetToObject_objectReference :: Lens.Lens' AddFacetToObject ObjectReference
addFacetToObject_objectReference = Lens.lens (\AddFacetToObject' {objectReference} -> objectReference) (\s@AddFacetToObject' {} a -> s {objectReference = a} :: AddFacetToObject)

instance Prelude.AWSRequest AddFacetToObject where
  type Rs AddFacetToObject = AddFacetToObjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddFacetToObjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddFacetToObject

instance Prelude.NFData AddFacetToObject

instance Prelude.ToHeaders AddFacetToObject where
  toHeaders AddFacetToObject' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON AddFacetToObject where
  toJSON AddFacetToObject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ObjectAttributeList" Prelude..=)
              Prelude.<$> objectAttributeList,
            Prelude.Just ("SchemaFacet" Prelude..= schemaFacet),
            Prelude.Just
              ("ObjectReference" Prelude..= objectReference)
          ]
      )

instance Prelude.ToPath AddFacetToObject where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/facets"

instance Prelude.ToQuery AddFacetToObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddFacetToObjectResponse' smart constructor.
data AddFacetToObjectResponse = AddFacetToObjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddFacetToObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addFacetToObjectResponse_httpStatus' - The response's http status code.
newAddFacetToObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddFacetToObjectResponse
newAddFacetToObjectResponse pHttpStatus_ =
  AddFacetToObjectResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
addFacetToObjectResponse_httpStatus :: Lens.Lens' AddFacetToObjectResponse Prelude.Int
addFacetToObjectResponse_httpStatus = Lens.lens (\AddFacetToObjectResponse' {httpStatus} -> httpStatus) (\s@AddFacetToObjectResponse' {} a -> s {httpStatus = a} :: AddFacetToObjectResponse)

instance Prelude.NFData AddFacetToObjectResponse
