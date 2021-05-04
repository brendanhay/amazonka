{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Creates an index object inside of a BatchRead operation. For more
-- information, see CreateIndex and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchCreateIndex' smart constructor.
data BatchCreateIndex = BatchCreateIndex'
  { -- | A reference to the parent object that contains the index object.
    parentReference :: Prelude.Maybe ObjectReference,
    -- | The name of the link between the parent object and the index object.
    linkName :: Prelude.Maybe Prelude.Text,
    -- | The batch reference name. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
    -- for more information.
    batchReferenceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the attributes that should be indexed on. Currently only a
    -- single attribute is supported.
    orderedIndexedAttributeList :: [AttributeKey],
    -- | Indicates whether the attribute that is being indexed has unique values
    -- or not.
    isUnique :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Bool ->
  BatchCreateIndex
newBatchCreateIndex pIsUnique_ =
  BatchCreateIndex'
    { parentReference =
        Prelude.Nothing,
      linkName = Prelude.Nothing,
      batchReferenceName = Prelude.Nothing,
      orderedIndexedAttributeList = Prelude.mempty,
      isUnique = pIsUnique_
    }

-- | A reference to the parent object that contains the index object.
batchCreateIndex_parentReference :: Lens.Lens' BatchCreateIndex (Prelude.Maybe ObjectReference)
batchCreateIndex_parentReference = Lens.lens (\BatchCreateIndex' {parentReference} -> parentReference) (\s@BatchCreateIndex' {} a -> s {parentReference = a} :: BatchCreateIndex)

-- | The name of the link between the parent object and the index object.
batchCreateIndex_linkName :: Lens.Lens' BatchCreateIndex (Prelude.Maybe Prelude.Text)
batchCreateIndex_linkName = Lens.lens (\BatchCreateIndex' {linkName} -> linkName) (\s@BatchCreateIndex' {} a -> s {linkName = a} :: BatchCreateIndex)

-- | The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
batchCreateIndex_batchReferenceName :: Lens.Lens' BatchCreateIndex (Prelude.Maybe Prelude.Text)
batchCreateIndex_batchReferenceName = Lens.lens (\BatchCreateIndex' {batchReferenceName} -> batchReferenceName) (\s@BatchCreateIndex' {} a -> s {batchReferenceName = a} :: BatchCreateIndex)

-- | Specifies the attributes that should be indexed on. Currently only a
-- single attribute is supported.
batchCreateIndex_orderedIndexedAttributeList :: Lens.Lens' BatchCreateIndex [AttributeKey]
batchCreateIndex_orderedIndexedAttributeList = Lens.lens (\BatchCreateIndex' {orderedIndexedAttributeList} -> orderedIndexedAttributeList) (\s@BatchCreateIndex' {} a -> s {orderedIndexedAttributeList = a} :: BatchCreateIndex) Prelude.. Prelude._Coerce

-- | Indicates whether the attribute that is being indexed has unique values
-- or not.
batchCreateIndex_isUnique :: Lens.Lens' BatchCreateIndex Prelude.Bool
batchCreateIndex_isUnique = Lens.lens (\BatchCreateIndex' {isUnique} -> isUnique) (\s@BatchCreateIndex' {} a -> s {isUnique = a} :: BatchCreateIndex)

instance Prelude.Hashable BatchCreateIndex

instance Prelude.NFData BatchCreateIndex

instance Prelude.ToJSON BatchCreateIndex where
  toJSON BatchCreateIndex' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ParentReference" Prelude..=)
              Prelude.<$> parentReference,
            ("LinkName" Prelude..=) Prelude.<$> linkName,
            ("BatchReferenceName" Prelude..=)
              Prelude.<$> batchReferenceName,
            Prelude.Just
              ( "OrderedIndexedAttributeList"
                  Prelude..= orderedIndexedAttributeList
              ),
            Prelude.Just ("IsUnique" Prelude..= isUnique)
          ]
      )
