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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIndex' smart constructor.
data CreateIndex = CreateIndex'
  { -- | A reference to the parent object that contains the index object.
    parentReference :: Prelude.Maybe ObjectReference,
    -- | The name of the link between the parent object and the index object.
    linkName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the directory where the index should be created.
    directoryArn :: Prelude.Text,
    -- | Specifies the attributes that should be indexed on. Currently only a
    -- single attribute is supported.
    orderedIndexedAttributeList :: [AttributeKey],
    -- | Indicates whether the attribute that is being indexed has unique values
    -- or not.
    isUnique :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'isUnique'
  Prelude.Bool ->
  CreateIndex
newCreateIndex pDirectoryArn_ pIsUnique_ =
  CreateIndex'
    { parentReference = Prelude.Nothing,
      linkName = Prelude.Nothing,
      directoryArn = pDirectoryArn_,
      orderedIndexedAttributeList = Prelude.mempty,
      isUnique = pIsUnique_
    }

-- | A reference to the parent object that contains the index object.
createIndex_parentReference :: Lens.Lens' CreateIndex (Prelude.Maybe ObjectReference)
createIndex_parentReference = Lens.lens (\CreateIndex' {parentReference} -> parentReference) (\s@CreateIndex' {} a -> s {parentReference = a} :: CreateIndex)

-- | The name of the link between the parent object and the index object.
createIndex_linkName :: Lens.Lens' CreateIndex (Prelude.Maybe Prelude.Text)
createIndex_linkName = Lens.lens (\CreateIndex' {linkName} -> linkName) (\s@CreateIndex' {} a -> s {linkName = a} :: CreateIndex)

-- | The ARN of the directory where the index should be created.
createIndex_directoryArn :: Lens.Lens' CreateIndex Prelude.Text
createIndex_directoryArn = Lens.lens (\CreateIndex' {directoryArn} -> directoryArn) (\s@CreateIndex' {} a -> s {directoryArn = a} :: CreateIndex)

-- | Specifies the attributes that should be indexed on. Currently only a
-- single attribute is supported.
createIndex_orderedIndexedAttributeList :: Lens.Lens' CreateIndex [AttributeKey]
createIndex_orderedIndexedAttributeList = Lens.lens (\CreateIndex' {orderedIndexedAttributeList} -> orderedIndexedAttributeList) (\s@CreateIndex' {} a -> s {orderedIndexedAttributeList = a} :: CreateIndex) Prelude.. Prelude._Coerce

-- | Indicates whether the attribute that is being indexed has unique values
-- or not.
createIndex_isUnique :: Lens.Lens' CreateIndex Prelude.Bool
createIndex_isUnique = Lens.lens (\CreateIndex' {isUnique} -> isUnique) (\s@CreateIndex' {} a -> s {isUnique = a} :: CreateIndex)

instance Prelude.AWSRequest CreateIndex where
  type Rs CreateIndex = CreateIndexResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIndexResponse'
            Prelude.<$> (x Prelude..?> "ObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIndex

instance Prelude.NFData CreateIndex

instance Prelude.ToHeaders CreateIndex where
  toHeaders CreateIndex' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON CreateIndex where
  toJSON CreateIndex' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ParentReference" Prelude..=)
              Prelude.<$> parentReference,
            ("LinkName" Prelude..=) Prelude.<$> linkName,
            Prelude.Just
              ( "OrderedIndexedAttributeList"
                  Prelude..= orderedIndexedAttributeList
              ),
            Prelude.Just ("IsUnique" Prelude..= isUnique)
          ]
      )

instance Prelude.ToPath CreateIndex where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/index"

instance Prelude.ToQuery CreateIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIndexResponse' smart constructor.
data CreateIndexResponse = CreateIndexResponse'
  { -- | The @ObjectIdentifier@ of the index created by this operation.
    objectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateIndexResponse
newCreateIndexResponse pHttpStatus_ =
  CreateIndexResponse'
    { objectIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ of the index created by this operation.
createIndexResponse_objectIdentifier :: Lens.Lens' CreateIndexResponse (Prelude.Maybe Prelude.Text)
createIndexResponse_objectIdentifier = Lens.lens (\CreateIndexResponse' {objectIdentifier} -> objectIdentifier) (\s@CreateIndexResponse' {} a -> s {objectIdentifier = a} :: CreateIndexResponse)

-- | The response's http status code.
createIndexResponse_httpStatus :: Lens.Lens' CreateIndexResponse Prelude.Int
createIndexResponse_httpStatus = Lens.lens (\CreateIndexResponse' {httpStatus} -> httpStatus) (\s@CreateIndexResponse' {} a -> s {httpStatus = a} :: CreateIndexResponse)

instance Prelude.NFData CreateIndexResponse
