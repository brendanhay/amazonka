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
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateObject where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a CreateObject operation.
--
-- /See:/ 'newBatchCreateObject' smart constructor.
data BatchCreateObject = BatchCreateObject'
  { -- | If specified, the parent reference to which this object will be
    -- attached.
    parentReference :: Prelude.Maybe ObjectReference,
    -- | The name of the link.
    linkName :: Prelude.Maybe Prelude.Text,
    -- | The batch reference name. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
    -- for more information.
    batchReferenceName :: Prelude.Maybe Prelude.Text,
    -- | A list of @FacetArns@ that will be associated with the object. For more
    -- information, see arns.
    schemaFacet :: [SchemaFacet],
    -- | An attribute map, which contains an attribute ARN as the key and
    -- attribute value as the map value.
    objectAttributeList :: [AttributeKeyAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentReference', 'batchCreateObject_parentReference' - If specified, the parent reference to which this object will be
-- attached.
--
-- 'linkName', 'batchCreateObject_linkName' - The name of the link.
--
-- 'batchReferenceName', 'batchCreateObject_batchReferenceName' - The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
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
    { parentReference =
        Prelude.Nothing,
      linkName = Prelude.Nothing,
      batchReferenceName = Prelude.Nothing,
      schemaFacet = Prelude.mempty,
      objectAttributeList = Prelude.mempty
    }

-- | If specified, the parent reference to which this object will be
-- attached.
batchCreateObject_parentReference :: Lens.Lens' BatchCreateObject (Prelude.Maybe ObjectReference)
batchCreateObject_parentReference = Lens.lens (\BatchCreateObject' {parentReference} -> parentReference) (\s@BatchCreateObject' {} a -> s {parentReference = a} :: BatchCreateObject)

-- | The name of the link.
batchCreateObject_linkName :: Lens.Lens' BatchCreateObject (Prelude.Maybe Prelude.Text)
batchCreateObject_linkName = Lens.lens (\BatchCreateObject' {linkName} -> linkName) (\s@BatchCreateObject' {} a -> s {linkName = a} :: BatchCreateObject)

-- | The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
batchCreateObject_batchReferenceName :: Lens.Lens' BatchCreateObject (Prelude.Maybe Prelude.Text)
batchCreateObject_batchReferenceName = Lens.lens (\BatchCreateObject' {batchReferenceName} -> batchReferenceName) (\s@BatchCreateObject' {} a -> s {batchReferenceName = a} :: BatchCreateObject)

-- | A list of @FacetArns@ that will be associated with the object. For more
-- information, see arns.
batchCreateObject_schemaFacet :: Lens.Lens' BatchCreateObject [SchemaFacet]
batchCreateObject_schemaFacet = Lens.lens (\BatchCreateObject' {schemaFacet} -> schemaFacet) (\s@BatchCreateObject' {} a -> s {schemaFacet = a} :: BatchCreateObject) Prelude.. Prelude._Coerce

-- | An attribute map, which contains an attribute ARN as the key and
-- attribute value as the map value.
batchCreateObject_objectAttributeList :: Lens.Lens' BatchCreateObject [AttributeKeyAndValue]
batchCreateObject_objectAttributeList = Lens.lens (\BatchCreateObject' {objectAttributeList} -> objectAttributeList) (\s@BatchCreateObject' {} a -> s {objectAttributeList = a} :: BatchCreateObject) Prelude.. Prelude._Coerce

instance Prelude.Hashable BatchCreateObject

instance Prelude.NFData BatchCreateObject

instance Prelude.ToJSON BatchCreateObject where
  toJSON BatchCreateObject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ParentReference" Prelude..=)
              Prelude.<$> parentReference,
            ("LinkName" Prelude..=) Prelude.<$> linkName,
            ("BatchReferenceName" Prelude..=)
              Prelude.<$> batchReferenceName,
            Prelude.Just ("SchemaFacet" Prelude..= schemaFacet),
            Prelude.Just
              ( "ObjectAttributeList"
                  Prelude..= objectAttributeList
              )
          ]
      )
