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
-- Module      : Amazonka.CloudDirectory.Types.BatchGetObjectAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchGetObjectAttributes where

import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.SchemaFacet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Retrieves attributes within a facet that are associated with an object
-- inside an BatchRead operation. For more information, see
-- GetObjectAttributes and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchGetObjectAttributes' smart constructor.
data BatchGetObjectAttributes = BatchGetObjectAttributes'
  { -- | Reference that identifies the object whose attributes will be retrieved.
    objectReference :: ObjectReference,
    -- | Identifier for the facet whose attributes will be retrieved. See
    -- SchemaFacet for details.
    schemaFacet :: SchemaFacet,
    -- | List of attribute names whose values will be retrieved.
    attributeNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectReference', 'batchGetObjectAttributes_objectReference' - Reference that identifies the object whose attributes will be retrieved.
--
-- 'schemaFacet', 'batchGetObjectAttributes_schemaFacet' - Identifier for the facet whose attributes will be retrieved. See
-- SchemaFacet for details.
--
-- 'attributeNames', 'batchGetObjectAttributes_attributeNames' - List of attribute names whose values will be retrieved.
newBatchGetObjectAttributes ::
  -- | 'objectReference'
  ObjectReference ->
  -- | 'schemaFacet'
  SchemaFacet ->
  BatchGetObjectAttributes
newBatchGetObjectAttributes
  pObjectReference_
  pSchemaFacet_ =
    BatchGetObjectAttributes'
      { objectReference =
          pObjectReference_,
        schemaFacet = pSchemaFacet_,
        attributeNames = Prelude.mempty
      }

-- | Reference that identifies the object whose attributes will be retrieved.
batchGetObjectAttributes_objectReference :: Lens.Lens' BatchGetObjectAttributes ObjectReference
batchGetObjectAttributes_objectReference = Lens.lens (\BatchGetObjectAttributes' {objectReference} -> objectReference) (\s@BatchGetObjectAttributes' {} a -> s {objectReference = a} :: BatchGetObjectAttributes)

-- | Identifier for the facet whose attributes will be retrieved. See
-- SchemaFacet for details.
batchGetObjectAttributes_schemaFacet :: Lens.Lens' BatchGetObjectAttributes SchemaFacet
batchGetObjectAttributes_schemaFacet = Lens.lens (\BatchGetObjectAttributes' {schemaFacet} -> schemaFacet) (\s@BatchGetObjectAttributes' {} a -> s {schemaFacet = a} :: BatchGetObjectAttributes)

-- | List of attribute names whose values will be retrieved.
batchGetObjectAttributes_attributeNames :: Lens.Lens' BatchGetObjectAttributes [Prelude.Text]
batchGetObjectAttributes_attributeNames = Lens.lens (\BatchGetObjectAttributes' {attributeNames} -> attributeNames) (\s@BatchGetObjectAttributes' {} a -> s {attributeNames = a} :: BatchGetObjectAttributes) Prelude.. Lens.coerced

instance Prelude.Hashable BatchGetObjectAttributes where
  hashWithSalt _salt BatchGetObjectAttributes' {..} =
    _salt `Prelude.hashWithSalt` objectReference
      `Prelude.hashWithSalt` schemaFacet
      `Prelude.hashWithSalt` attributeNames

instance Prelude.NFData BatchGetObjectAttributes where
  rnf BatchGetObjectAttributes' {..} =
    Prelude.rnf objectReference
      `Prelude.seq` Prelude.rnf schemaFacet
      `Prelude.seq` Prelude.rnf attributeNames

instance Core.ToJSON BatchGetObjectAttributes where
  toJSON BatchGetObjectAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Core..= objectReference),
            Prelude.Just ("SchemaFacet" Core..= schemaFacet),
            Prelude.Just
              ("AttributeNames" Core..= attributeNames)
          ]
      )
