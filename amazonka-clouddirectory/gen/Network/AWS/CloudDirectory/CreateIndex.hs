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
-- Module      : Network.AWS.CloudDirectory.CreateIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an index object. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.html Indexing and search>
-- for more information.
module Network.AWS.CloudDirectory.CreateIndex
  ( -- * Creating a Request
    CreateIndex (..),
    newCreateIndex,

    -- * Request Lenses
    createIndex_parentReference,
    createIndex_linkName,
    createIndex_directoryArn,
    createIndex_orderedIndexedAttributeList,
    createIndex_isUnique,

    -- * Destructuring the Response
    CreateIndexResponse (..),
    newCreateIndexResponse,

    -- * Response Lenses
    createIndexResponse_objectIdentifier,
    createIndexResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIndex' smart constructor.
data CreateIndex = CreateIndex'
  { -- | A reference to the parent object that contains the index object.
    parentReference :: Core.Maybe ObjectReference,
    -- | The name of the link between the parent object and the index object.
    linkName :: Core.Maybe Core.Text,
    -- | The ARN of the directory where the index should be created.
    directoryArn :: Core.Text,
    -- | Specifies the attributes that should be indexed on. Currently only a
    -- single attribute is supported.
    orderedIndexedAttributeList :: [AttributeKey],
    -- | Indicates whether the attribute that is being indexed has unique values
    -- or not.
    isUnique :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentReference', 'createIndex_parentReference' - A reference to the parent object that contains the index object.
--
-- 'linkName', 'createIndex_linkName' - The name of the link between the parent object and the index object.
--
-- 'directoryArn', 'createIndex_directoryArn' - The ARN of the directory where the index should be created.
--
-- 'orderedIndexedAttributeList', 'createIndex_orderedIndexedAttributeList' - Specifies the attributes that should be indexed on. Currently only a
-- single attribute is supported.
--
-- 'isUnique', 'createIndex_isUnique' - Indicates whether the attribute that is being indexed has unique values
-- or not.
newCreateIndex ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'isUnique'
  Core.Bool ->
  CreateIndex
newCreateIndex pDirectoryArn_ pIsUnique_ =
  CreateIndex'
    { parentReference = Core.Nothing,
      linkName = Core.Nothing,
      directoryArn = pDirectoryArn_,
      orderedIndexedAttributeList = Core.mempty,
      isUnique = pIsUnique_
    }

-- | A reference to the parent object that contains the index object.
createIndex_parentReference :: Lens.Lens' CreateIndex (Core.Maybe ObjectReference)
createIndex_parentReference = Lens.lens (\CreateIndex' {parentReference} -> parentReference) (\s@CreateIndex' {} a -> s {parentReference = a} :: CreateIndex)

-- | The name of the link between the parent object and the index object.
createIndex_linkName :: Lens.Lens' CreateIndex (Core.Maybe Core.Text)
createIndex_linkName = Lens.lens (\CreateIndex' {linkName} -> linkName) (\s@CreateIndex' {} a -> s {linkName = a} :: CreateIndex)

-- | The ARN of the directory where the index should be created.
createIndex_directoryArn :: Lens.Lens' CreateIndex Core.Text
createIndex_directoryArn = Lens.lens (\CreateIndex' {directoryArn} -> directoryArn) (\s@CreateIndex' {} a -> s {directoryArn = a} :: CreateIndex)

-- | Specifies the attributes that should be indexed on. Currently only a
-- single attribute is supported.
createIndex_orderedIndexedAttributeList :: Lens.Lens' CreateIndex [AttributeKey]
createIndex_orderedIndexedAttributeList = Lens.lens (\CreateIndex' {orderedIndexedAttributeList} -> orderedIndexedAttributeList) (\s@CreateIndex' {} a -> s {orderedIndexedAttributeList = a} :: CreateIndex) Core.. Lens._Coerce

-- | Indicates whether the attribute that is being indexed has unique values
-- or not.
createIndex_isUnique :: Lens.Lens' CreateIndex Core.Bool
createIndex_isUnique = Lens.lens (\CreateIndex' {isUnique} -> isUnique) (\s@CreateIndex' {} a -> s {isUnique = a} :: CreateIndex)

instance Core.AWSRequest CreateIndex where
  type AWSResponse CreateIndex = CreateIndexResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIndexResponse'
            Core.<$> (x Core..?> "ObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateIndex

instance Core.NFData CreateIndex

instance Core.ToHeaders CreateIndex where
  toHeaders CreateIndex' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON CreateIndex where
  toJSON CreateIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ParentReference" Core..=)
              Core.<$> parentReference,
            ("LinkName" Core..=) Core.<$> linkName,
            Core.Just
              ( "OrderedIndexedAttributeList"
                  Core..= orderedIndexedAttributeList
              ),
            Core.Just ("IsUnique" Core..= isUnique)
          ]
      )

instance Core.ToPath CreateIndex where
  toPath =
    Core.const "/amazonclouddirectory/2017-01-11/index"

instance Core.ToQuery CreateIndex where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateIndexResponse' smart constructor.
data CreateIndexResponse = CreateIndexResponse'
  { -- | The @ObjectIdentifier@ of the index created by this operation.
    objectIdentifier :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'createIndexResponse_objectIdentifier' - The @ObjectIdentifier@ of the index created by this operation.
--
-- 'httpStatus', 'createIndexResponse_httpStatus' - The response's http status code.
newCreateIndexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateIndexResponse
newCreateIndexResponse pHttpStatus_ =
  CreateIndexResponse'
    { objectIdentifier =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ of the index created by this operation.
createIndexResponse_objectIdentifier :: Lens.Lens' CreateIndexResponse (Core.Maybe Core.Text)
createIndexResponse_objectIdentifier = Lens.lens (\CreateIndexResponse' {objectIdentifier} -> objectIdentifier) (\s@CreateIndexResponse' {} a -> s {objectIdentifier = a} :: CreateIndexResponse)

-- | The response's http status code.
createIndexResponse_httpStatus :: Lens.Lens' CreateIndexResponse Core.Int
createIndexResponse_httpStatus = Lens.lens (\CreateIndexResponse' {httpStatus} -> httpStatus) (\s@CreateIndexResponse' {} a -> s {httpStatus = a} :: CreateIndexResponse)

instance Core.NFData CreateIndexResponse
