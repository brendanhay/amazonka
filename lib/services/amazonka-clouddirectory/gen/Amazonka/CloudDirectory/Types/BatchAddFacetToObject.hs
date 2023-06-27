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
-- Module      : Amazonka.CloudDirectory.Types.BatchAddFacetToObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchAddFacetToObject where

import Amazonka.CloudDirectory.Types.AttributeKeyAndValue
import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.SchemaFacet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a batch add facet to object operation.
--
-- /See:/ 'newBatchAddFacetToObject' smart constructor.
data BatchAddFacetToObject = BatchAddFacetToObject'
  { -- | Represents the facet being added to the object.
    schemaFacet :: SchemaFacet,
    -- | The attributes to set on the object.
    objectAttributeList :: [AttributeKeyAndValue],
    -- | A reference to the object being mutated.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAddFacetToObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaFacet', 'batchAddFacetToObject_schemaFacet' - Represents the facet being added to the object.
--
-- 'objectAttributeList', 'batchAddFacetToObject_objectAttributeList' - The attributes to set on the object.
--
-- 'objectReference', 'batchAddFacetToObject_objectReference' - A reference to the object being mutated.
newBatchAddFacetToObject ::
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  BatchAddFacetToObject
newBatchAddFacetToObject
  pSchemaFacet_
  pObjectReference_ =
    BatchAddFacetToObject'
      { schemaFacet = pSchemaFacet_,
        objectAttributeList = Prelude.mempty,
        objectReference = pObjectReference_
      }

-- | Represents the facet being added to the object.
batchAddFacetToObject_schemaFacet :: Lens.Lens' BatchAddFacetToObject SchemaFacet
batchAddFacetToObject_schemaFacet = Lens.lens (\BatchAddFacetToObject' {schemaFacet} -> schemaFacet) (\s@BatchAddFacetToObject' {} a -> s {schemaFacet = a} :: BatchAddFacetToObject)

-- | The attributes to set on the object.
batchAddFacetToObject_objectAttributeList :: Lens.Lens' BatchAddFacetToObject [AttributeKeyAndValue]
batchAddFacetToObject_objectAttributeList = Lens.lens (\BatchAddFacetToObject' {objectAttributeList} -> objectAttributeList) (\s@BatchAddFacetToObject' {} a -> s {objectAttributeList = a} :: BatchAddFacetToObject) Prelude.. Lens.coerced

-- | A reference to the object being mutated.
batchAddFacetToObject_objectReference :: Lens.Lens' BatchAddFacetToObject ObjectReference
batchAddFacetToObject_objectReference = Lens.lens (\BatchAddFacetToObject' {objectReference} -> objectReference) (\s@BatchAddFacetToObject' {} a -> s {objectReference = a} :: BatchAddFacetToObject)

instance Prelude.Hashable BatchAddFacetToObject where
  hashWithSalt _salt BatchAddFacetToObject' {..} =
    _salt
      `Prelude.hashWithSalt` schemaFacet
      `Prelude.hashWithSalt` objectAttributeList
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchAddFacetToObject where
  rnf BatchAddFacetToObject' {..} =
    Prelude.rnf schemaFacet
      `Prelude.seq` Prelude.rnf objectAttributeList
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchAddFacetToObject where
  toJSON BatchAddFacetToObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaFacet" Data..= schemaFacet),
            Prelude.Just
              ("ObjectAttributeList" Data..= objectAttributeList),
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
