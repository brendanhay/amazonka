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
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchGetObjectAttributes_attributeNames = Lens.lens (\BatchGetObjectAttributes' {attributeNames} -> attributeNames) (\s@BatchGetObjectAttributes' {} a -> s {attributeNames = a} :: BatchGetObjectAttributes) Prelude.. Prelude._Coerce

instance Prelude.Hashable BatchGetObjectAttributes

instance Prelude.NFData BatchGetObjectAttributes

instance Prelude.ToJSON BatchGetObjectAttributes where
  toJSON BatchGetObjectAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Prelude..= objectReference),
            Prelude.Just ("SchemaFacet" Prelude..= schemaFacet),
            Prelude.Just
              ("AttributeNames" Prelude..= attributeNames)
          ]
      )
