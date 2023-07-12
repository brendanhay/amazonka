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
-- Module      : Amazonka.CloudDirectory.CreateObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an object in a Directory. Additionally attaches the object to a
-- parent, if a parent reference and @LinkName@ is specified. An object is
-- simply a collection of Facet attributes. You can also use this API call
-- to create a policy object, if the facet from which you create the object
-- is a policy facet.
module Amazonka.CloudDirectory.CreateObject
  ( -- * Creating a Request
    CreateObject (..),
    newCreateObject,

    -- * Request Lenses
    createObject_linkName,
    createObject_objectAttributeList,
    createObject_parentReference,
    createObject_directoryArn,
    createObject_schemaFacets,

    -- * Destructuring the Response
    CreateObjectResponse (..),
    newCreateObjectResponse,

    -- * Response Lenses
    createObjectResponse_objectIdentifier,
    createObjectResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateObject' smart constructor.
data CreateObject = CreateObject'
  { -- | The name of link that is used to attach this object to a parent.
    linkName :: Prelude.Maybe Prelude.Text,
    -- | The attribute map whose attribute ARN contains the key and attribute
    -- value as the map value.
    objectAttributeList :: Prelude.Maybe [AttributeKeyAndValue],
    -- | If specified, the parent reference to which this object will be
    -- attached.
    parentReference :: Prelude.Maybe ObjectReference,
    -- | The Amazon Resource Name (ARN) that is associated with the Directory in
    -- which the object will be created. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | A list of schema facets to be associated with the object. Do not provide
    -- minor version components. See SchemaFacet for details.
    schemaFacets :: [SchemaFacet]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkName', 'createObject_linkName' - The name of link that is used to attach this object to a parent.
--
-- 'objectAttributeList', 'createObject_objectAttributeList' - The attribute map whose attribute ARN contains the key and attribute
-- value as the map value.
--
-- 'parentReference', 'createObject_parentReference' - If specified, the parent reference to which this object will be
-- attached.
--
-- 'directoryArn', 'createObject_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory in
-- which the object will be created. For more information, see arns.
--
-- 'schemaFacets', 'createObject_schemaFacets' - A list of schema facets to be associated with the object. Do not provide
-- minor version components. See SchemaFacet for details.
newCreateObject ::
  -- | 'directoryArn'
  Prelude.Text ->
  CreateObject
newCreateObject pDirectoryArn_ =
  CreateObject'
    { linkName = Prelude.Nothing,
      objectAttributeList = Prelude.Nothing,
      parentReference = Prelude.Nothing,
      directoryArn = pDirectoryArn_,
      schemaFacets = Prelude.mempty
    }

-- | The name of link that is used to attach this object to a parent.
createObject_linkName :: Lens.Lens' CreateObject (Prelude.Maybe Prelude.Text)
createObject_linkName = Lens.lens (\CreateObject' {linkName} -> linkName) (\s@CreateObject' {} a -> s {linkName = a} :: CreateObject)

-- | The attribute map whose attribute ARN contains the key and attribute
-- value as the map value.
createObject_objectAttributeList :: Lens.Lens' CreateObject (Prelude.Maybe [AttributeKeyAndValue])
createObject_objectAttributeList = Lens.lens (\CreateObject' {objectAttributeList} -> objectAttributeList) (\s@CreateObject' {} a -> s {objectAttributeList = a} :: CreateObject) Prelude.. Lens.mapping Lens.coerced

-- | If specified, the parent reference to which this object will be
-- attached.
createObject_parentReference :: Lens.Lens' CreateObject (Prelude.Maybe ObjectReference)
createObject_parentReference = Lens.lens (\CreateObject' {parentReference} -> parentReference) (\s@CreateObject' {} a -> s {parentReference = a} :: CreateObject)

-- | The Amazon Resource Name (ARN) that is associated with the Directory in
-- which the object will be created. For more information, see arns.
createObject_directoryArn :: Lens.Lens' CreateObject Prelude.Text
createObject_directoryArn = Lens.lens (\CreateObject' {directoryArn} -> directoryArn) (\s@CreateObject' {} a -> s {directoryArn = a} :: CreateObject)

-- | A list of schema facets to be associated with the object. Do not provide
-- minor version components. See SchemaFacet for details.
createObject_schemaFacets :: Lens.Lens' CreateObject [SchemaFacet]
createObject_schemaFacets = Lens.lens (\CreateObject' {schemaFacets} -> schemaFacets) (\s@CreateObject' {} a -> s {schemaFacets = a} :: CreateObject) Prelude.. Lens.coerced

instance Core.AWSRequest CreateObject where
  type AWSResponse CreateObject = CreateObjectResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateObjectResponse'
            Prelude.<$> (x Data..?> "ObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateObject where
  hashWithSalt _salt CreateObject' {..} =
    _salt
      `Prelude.hashWithSalt` linkName
      `Prelude.hashWithSalt` objectAttributeList
      `Prelude.hashWithSalt` parentReference
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` schemaFacets

instance Prelude.NFData CreateObject where
  rnf CreateObject' {..} =
    Prelude.rnf linkName
      `Prelude.seq` Prelude.rnf objectAttributeList
      `Prelude.seq` Prelude.rnf parentReference
      `Prelude.seq` Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf schemaFacets

instance Data.ToHeaders CreateObject where
  toHeaders CreateObject' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON CreateObject where
  toJSON CreateObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LinkName" Data..=) Prelude.<$> linkName,
            ("ObjectAttributeList" Data..=)
              Prelude.<$> objectAttributeList,
            ("ParentReference" Data..=)
              Prelude.<$> parentReference,
            Prelude.Just ("SchemaFacets" Data..= schemaFacets)
          ]
      )

instance Data.ToPath CreateObject where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object"

instance Data.ToQuery CreateObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateObjectResponse' smart constructor.
data CreateObjectResponse = CreateObjectResponse'
  { -- | The identifier that is associated with the object.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'createObjectResponse_objectIdentifier' - The identifier that is associated with the object.
--
-- 'httpStatus', 'createObjectResponse_httpStatus' - The response's http status code.
newCreateObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateObjectResponse
newCreateObjectResponse pHttpStatus_ =
  CreateObjectResponse'
    { objectIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier that is associated with the object.
createObjectResponse_objectIdentifier :: Lens.Lens' CreateObjectResponse (Prelude.Maybe Prelude.Text)
createObjectResponse_objectIdentifier = Lens.lens (\CreateObjectResponse' {objectIdentifier} -> objectIdentifier) (\s@CreateObjectResponse' {} a -> s {objectIdentifier = a} :: CreateObjectResponse)

-- | The response's http status code.
createObjectResponse_httpStatus :: Lens.Lens' CreateObjectResponse Prelude.Int
createObjectResponse_httpStatus = Lens.lens (\CreateObjectResponse' {httpStatus} -> httpStatus) (\s@CreateObjectResponse' {} a -> s {httpStatus = a} :: CreateObjectResponse)

instance Prelude.NFData CreateObjectResponse where
  rnf CreateObjectResponse' {..} =
    Prelude.rnf objectIdentifier
      `Prelude.seq` Prelude.rnf httpStatus
