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
-- Module      : Amazonka.CloudDirectory.Types.BatchCreateObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchCreateObject where

import Amazonka.CloudDirectory.Types.AttributeKeyAndValue
import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.SchemaFacet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a CreateObject operation.
--
-- /See:/ 'newBatchCreateObject' smart constructor.
data BatchCreateObject = BatchCreateObject'
  { -- | The batch reference name. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
    -- for more information.
    batchReferenceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the link.
    linkName :: Prelude.Maybe Prelude.Text,
    -- | If specified, the parent reference to which this object will be
    -- attached.
    parentReference :: Prelude.Maybe ObjectReference,
    -- | A list of @FacetArns@ that will be associated with the object. For more
    -- information, see arns.
    schemaFacet :: [SchemaFacet],
    -- | An attribute map, which contains an attribute ARN as the key and
    -- attribute value as the map value.
    objectAttributeList :: [AttributeKeyAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchReferenceName', 'batchCreateObject_batchReferenceName' - The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
--
-- 'linkName', 'batchCreateObject_linkName' - The name of the link.
--
-- 'parentReference', 'batchCreateObject_parentReference' - If specified, the parent reference to which this object will be
-- attached.
--
-- 'schemaFacet', 'batchCreateObject_schemaFacet' - A list of @FacetArns@ that will be associated with the object. For more
-- information, see arns.
--
-- 'objectAttributeList', 'batchCreateObject_objectAttributeList' - An attribute map, which contains an attribute ARN as the key and
-- attribute value as the map value.
newBatchCreateObject ::
  BatchCreateObject
newBatchCreateObject =
  BatchCreateObject'
    { batchReferenceName =
        Prelude.Nothing,
      linkName = Prelude.Nothing,
      parentReference = Prelude.Nothing,
      schemaFacet = Prelude.mempty,
      objectAttributeList = Prelude.mempty
    }

-- | The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
batchCreateObject_batchReferenceName :: Lens.Lens' BatchCreateObject (Prelude.Maybe Prelude.Text)
batchCreateObject_batchReferenceName = Lens.lens (\BatchCreateObject' {batchReferenceName} -> batchReferenceName) (\s@BatchCreateObject' {} a -> s {batchReferenceName = a} :: BatchCreateObject)

-- | The name of the link.
batchCreateObject_linkName :: Lens.Lens' BatchCreateObject (Prelude.Maybe Prelude.Text)
batchCreateObject_linkName = Lens.lens (\BatchCreateObject' {linkName} -> linkName) (\s@BatchCreateObject' {} a -> s {linkName = a} :: BatchCreateObject)

-- | If specified, the parent reference to which this object will be
-- attached.
batchCreateObject_parentReference :: Lens.Lens' BatchCreateObject (Prelude.Maybe ObjectReference)
batchCreateObject_parentReference = Lens.lens (\BatchCreateObject' {parentReference} -> parentReference) (\s@BatchCreateObject' {} a -> s {parentReference = a} :: BatchCreateObject)

-- | A list of @FacetArns@ that will be associated with the object. For more
-- information, see arns.
batchCreateObject_schemaFacet :: Lens.Lens' BatchCreateObject [SchemaFacet]
batchCreateObject_schemaFacet = Lens.lens (\BatchCreateObject' {schemaFacet} -> schemaFacet) (\s@BatchCreateObject' {} a -> s {schemaFacet = a} :: BatchCreateObject) Prelude.. Lens.coerced

-- | An attribute map, which contains an attribute ARN as the key and
-- attribute value as the map value.
batchCreateObject_objectAttributeList :: Lens.Lens' BatchCreateObject [AttributeKeyAndValue]
batchCreateObject_objectAttributeList = Lens.lens (\BatchCreateObject' {objectAttributeList} -> objectAttributeList) (\s@BatchCreateObject' {} a -> s {objectAttributeList = a} :: BatchCreateObject) Prelude.. Lens.coerced

instance Prelude.Hashable BatchCreateObject where
  hashWithSalt _salt BatchCreateObject' {..} =
    _salt
      `Prelude.hashWithSalt` batchReferenceName
      `Prelude.hashWithSalt` linkName
      `Prelude.hashWithSalt` parentReference
      `Prelude.hashWithSalt` schemaFacet
      `Prelude.hashWithSalt` objectAttributeList

instance Prelude.NFData BatchCreateObject where
  rnf BatchCreateObject' {..} =
    Prelude.rnf batchReferenceName
      `Prelude.seq` Prelude.rnf linkName
      `Prelude.seq` Prelude.rnf parentReference
      `Prelude.seq` Prelude.rnf schemaFacet
      `Prelude.seq` Prelude.rnf objectAttributeList

instance Data.ToJSON BatchCreateObject where
  toJSON BatchCreateObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchReferenceName" Data..=)
              Prelude.<$> batchReferenceName,
            ("LinkName" Data..=) Prelude.<$> linkName,
            ("ParentReference" Data..=)
              Prelude.<$> parentReference,
            Prelude.Just ("SchemaFacet" Data..= schemaFacet),
            Prelude.Just
              ("ObjectAttributeList" Data..= objectAttributeList)
          ]
      )
