{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateIndex where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Creates an index object inside of a BatchRead operation. For more
-- information, see CreateIndex and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchCreateIndex' smart constructor.
data BatchCreateIndex = BatchCreateIndex'
  { -- | A reference to the parent object that contains the index object.
    parentReference :: Core.Maybe ObjectReference,
    -- | The name of the link between the parent object and the index object.
    linkName :: Core.Maybe Core.Text,
    -- | The batch reference name. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
    -- for more information.
    batchReferenceName :: Core.Maybe Core.Text,
    -- | Specifies the attributes that should be indexed on. Currently only a
    -- single attribute is supported.
    orderedIndexedAttributeList :: [AttributeKey],
    -- | Indicates whether the attribute that is being indexed has unique values
    -- or not.
    isUnique :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchCreateIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentReference', 'batchCreateIndex_parentReference' - A reference to the parent object that contains the index object.
--
-- 'linkName', 'batchCreateIndex_linkName' - The name of the link between the parent object and the index object.
--
-- 'batchReferenceName', 'batchCreateIndex_batchReferenceName' - The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
--
-- 'orderedIndexedAttributeList', 'batchCreateIndex_orderedIndexedAttributeList' - Specifies the attributes that should be indexed on. Currently only a
-- single attribute is supported.
--
-- 'isUnique', 'batchCreateIndex_isUnique' - Indicates whether the attribute that is being indexed has unique values
-- or not.
newBatchCreateIndex ::
  -- | 'isUnique'
  Core.Bool ->
  BatchCreateIndex
newBatchCreateIndex pIsUnique_ =
  BatchCreateIndex'
    { parentReference = Core.Nothing,
      linkName = Core.Nothing,
      batchReferenceName = Core.Nothing,
      orderedIndexedAttributeList = Core.mempty,
      isUnique = pIsUnique_
    }

-- | A reference to the parent object that contains the index object.
batchCreateIndex_parentReference :: Lens.Lens' BatchCreateIndex (Core.Maybe ObjectReference)
batchCreateIndex_parentReference = Lens.lens (\BatchCreateIndex' {parentReference} -> parentReference) (\s@BatchCreateIndex' {} a -> s {parentReference = a} :: BatchCreateIndex)

-- | The name of the link between the parent object and the index object.
batchCreateIndex_linkName :: Lens.Lens' BatchCreateIndex (Core.Maybe Core.Text)
batchCreateIndex_linkName = Lens.lens (\BatchCreateIndex' {linkName} -> linkName) (\s@BatchCreateIndex' {} a -> s {linkName = a} :: BatchCreateIndex)

-- | The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
batchCreateIndex_batchReferenceName :: Lens.Lens' BatchCreateIndex (Core.Maybe Core.Text)
batchCreateIndex_batchReferenceName = Lens.lens (\BatchCreateIndex' {batchReferenceName} -> batchReferenceName) (\s@BatchCreateIndex' {} a -> s {batchReferenceName = a} :: BatchCreateIndex)

-- | Specifies the attributes that should be indexed on. Currently only a
-- single attribute is supported.
batchCreateIndex_orderedIndexedAttributeList :: Lens.Lens' BatchCreateIndex [AttributeKey]
batchCreateIndex_orderedIndexedAttributeList = Lens.lens (\BatchCreateIndex' {orderedIndexedAttributeList} -> orderedIndexedAttributeList) (\s@BatchCreateIndex' {} a -> s {orderedIndexedAttributeList = a} :: BatchCreateIndex) Core.. Lens._Coerce

-- | Indicates whether the attribute that is being indexed has unique values
-- or not.
batchCreateIndex_isUnique :: Lens.Lens' BatchCreateIndex Core.Bool
batchCreateIndex_isUnique = Lens.lens (\BatchCreateIndex' {isUnique} -> isUnique) (\s@BatchCreateIndex' {} a -> s {isUnique = a} :: BatchCreateIndex)

instance Core.Hashable BatchCreateIndex

instance Core.NFData BatchCreateIndex

instance Core.ToJSON BatchCreateIndex where
  toJSON BatchCreateIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ParentReference" Core..=)
              Core.<$> parentReference,
            ("LinkName" Core..=) Core.<$> linkName,
            ("BatchReferenceName" Core..=)
              Core.<$> batchReferenceName,
            Core.Just
              ( "OrderedIndexedAttributeList"
                  Core..= orderedIndexedAttributeList
              ),
            Core.Just ("IsUnique" Core..= isUnique)
          ]
      )
