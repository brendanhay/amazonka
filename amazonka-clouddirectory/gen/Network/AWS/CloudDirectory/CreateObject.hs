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
-- Module      : Network.AWS.CloudDirectory.CreateObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an object in a Directory. Additionally attaches the object to a
-- parent, if a parent reference and @LinkName@ is specified. An object is
-- simply a collection of Facet attributes. You can also use this API call
-- to create a policy object, if the facet from which you create the object
-- is a policy facet.
module Network.AWS.CloudDirectory.CreateObject
  ( -- * Creating a Request
    CreateObject (..),
    newCreateObject,

    -- * Request Lenses
    createObject_parentReference,
    createObject_linkName,
    createObject_objectAttributeList,
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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateObject' smart constructor.
data CreateObject = CreateObject'
  { -- | If specified, the parent reference to which this object will be
    -- attached.
    parentReference :: Core.Maybe ObjectReference,
    -- | The name of link that is used to attach this object to a parent.
    linkName :: Core.Maybe Core.Text,
    -- | The attribute map whose attribute ARN contains the key and attribute
    -- value as the map value.
    objectAttributeList :: Core.Maybe [AttributeKeyAndValue],
    -- | The Amazon Resource Name (ARN) that is associated with the Directory in
    -- which the object will be created. For more information, see arns.
    directoryArn :: Core.Text,
    -- | A list of schema facets to be associated with the object. Do not provide
    -- minor version components. See SchemaFacet for details.
    schemaFacets :: [SchemaFacet]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentReference', 'createObject_parentReference' - If specified, the parent reference to which this object will be
-- attached.
--
-- 'linkName', 'createObject_linkName' - The name of link that is used to attach this object to a parent.
--
-- 'objectAttributeList', 'createObject_objectAttributeList' - The attribute map whose attribute ARN contains the key and attribute
-- value as the map value.
--
-- 'directoryArn', 'createObject_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory in
-- which the object will be created. For more information, see arns.
--
-- 'schemaFacets', 'createObject_schemaFacets' - A list of schema facets to be associated with the object. Do not provide
-- minor version components. See SchemaFacet for details.
newCreateObject ::
  -- | 'directoryArn'
  Core.Text ->
  CreateObject
newCreateObject pDirectoryArn_ =
  CreateObject'
    { parentReference = Core.Nothing,
      linkName = Core.Nothing,
      objectAttributeList = Core.Nothing,
      directoryArn = pDirectoryArn_,
      schemaFacets = Core.mempty
    }

-- | If specified, the parent reference to which this object will be
-- attached.
createObject_parentReference :: Lens.Lens' CreateObject (Core.Maybe ObjectReference)
createObject_parentReference = Lens.lens (\CreateObject' {parentReference} -> parentReference) (\s@CreateObject' {} a -> s {parentReference = a} :: CreateObject)

-- | The name of link that is used to attach this object to a parent.
createObject_linkName :: Lens.Lens' CreateObject (Core.Maybe Core.Text)
createObject_linkName = Lens.lens (\CreateObject' {linkName} -> linkName) (\s@CreateObject' {} a -> s {linkName = a} :: CreateObject)

-- | The attribute map whose attribute ARN contains the key and attribute
-- value as the map value.
createObject_objectAttributeList :: Lens.Lens' CreateObject (Core.Maybe [AttributeKeyAndValue])
createObject_objectAttributeList = Lens.lens (\CreateObject' {objectAttributeList} -> objectAttributeList) (\s@CreateObject' {} a -> s {objectAttributeList = a} :: CreateObject) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) that is associated with the Directory in
-- which the object will be created. For more information, see arns.
createObject_directoryArn :: Lens.Lens' CreateObject Core.Text
createObject_directoryArn = Lens.lens (\CreateObject' {directoryArn} -> directoryArn) (\s@CreateObject' {} a -> s {directoryArn = a} :: CreateObject)

-- | A list of schema facets to be associated with the object. Do not provide
-- minor version components. See SchemaFacet for details.
createObject_schemaFacets :: Lens.Lens' CreateObject [SchemaFacet]
createObject_schemaFacets = Lens.lens (\CreateObject' {schemaFacets} -> schemaFacets) (\s@CreateObject' {} a -> s {schemaFacets = a} :: CreateObject) Core.. Lens._Coerce

instance Core.AWSRequest CreateObject where
  type AWSResponse CreateObject = CreateObjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateObjectResponse'
            Core.<$> (x Core..?> "ObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateObject

instance Core.NFData CreateObject

instance Core.ToHeaders CreateObject where
  toHeaders CreateObject' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON CreateObject where
  toJSON CreateObject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ParentReference" Core..=)
              Core.<$> parentReference,
            ("LinkName" Core..=) Core.<$> linkName,
            ("ObjectAttributeList" Core..=)
              Core.<$> objectAttributeList,
            Core.Just ("SchemaFacets" Core..= schemaFacets)
          ]
      )

instance Core.ToPath CreateObject where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object"

instance Core.ToQuery CreateObject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateObjectResponse' smart constructor.
data CreateObjectResponse = CreateObjectResponse'
  { -- | The identifier that is associated with the object.
    objectIdentifier :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateObjectResponse
newCreateObjectResponse pHttpStatus_ =
  CreateObjectResponse'
    { objectIdentifier =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier that is associated with the object.
createObjectResponse_objectIdentifier :: Lens.Lens' CreateObjectResponse (Core.Maybe Core.Text)
createObjectResponse_objectIdentifier = Lens.lens (\CreateObjectResponse' {objectIdentifier} -> objectIdentifier) (\s@CreateObjectResponse' {} a -> s {objectIdentifier = a} :: CreateObjectResponse)

-- | The response's http status code.
createObjectResponse_httpStatus :: Lens.Lens' CreateObjectResponse Core.Int
createObjectResponse_httpStatus = Lens.lens (\CreateObjectResponse' {httpStatus} -> httpStatus) (\s@CreateObjectResponse' {} a -> s {httpStatus = a} :: CreateObjectResponse)

instance Core.NFData CreateObjectResponse
