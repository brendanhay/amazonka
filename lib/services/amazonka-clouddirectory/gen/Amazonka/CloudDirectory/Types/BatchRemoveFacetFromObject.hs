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
-- Module      : Amazonka.CloudDirectory.Types.BatchRemoveFacetFromObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchRemoveFacetFromObject where

import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.SchemaFacet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A batch operation to remove a facet from an object.
--
-- /See:/ 'newBatchRemoveFacetFromObject' smart constructor.
data BatchRemoveFacetFromObject = BatchRemoveFacetFromObject'
  { -- | The facet to remove from the object.
    schemaFacet :: SchemaFacet,
    -- | A reference to the object whose facet will be removed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchRemoveFacetFromObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaFacet', 'batchRemoveFacetFromObject_schemaFacet' - The facet to remove from the object.
--
-- 'objectReference', 'batchRemoveFacetFromObject_objectReference' - A reference to the object whose facet will be removed.
newBatchRemoveFacetFromObject ::
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  BatchRemoveFacetFromObject
newBatchRemoveFacetFromObject
  pSchemaFacet_
  pObjectReference_ =
    BatchRemoveFacetFromObject'
      { schemaFacet =
          pSchemaFacet_,
        objectReference = pObjectReference_
      }

-- | The facet to remove from the object.
batchRemoveFacetFromObject_schemaFacet :: Lens.Lens' BatchRemoveFacetFromObject SchemaFacet
batchRemoveFacetFromObject_schemaFacet = Lens.lens (\BatchRemoveFacetFromObject' {schemaFacet} -> schemaFacet) (\s@BatchRemoveFacetFromObject' {} a -> s {schemaFacet = a} :: BatchRemoveFacetFromObject)

-- | A reference to the object whose facet will be removed.
batchRemoveFacetFromObject_objectReference :: Lens.Lens' BatchRemoveFacetFromObject ObjectReference
batchRemoveFacetFromObject_objectReference = Lens.lens (\BatchRemoveFacetFromObject' {objectReference} -> objectReference) (\s@BatchRemoveFacetFromObject' {} a -> s {objectReference = a} :: BatchRemoveFacetFromObject)

instance Prelude.Hashable BatchRemoveFacetFromObject where
  hashWithSalt _salt BatchRemoveFacetFromObject' {..} =
    _salt
      `Prelude.hashWithSalt` schemaFacet
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchRemoveFacetFromObject where
  rnf BatchRemoveFacetFromObject' {..} =
    Prelude.rnf schemaFacet
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchRemoveFacetFromObject where
  toJSON BatchRemoveFacetFromObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaFacet" Data..= schemaFacet),
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
