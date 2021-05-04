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
-- Module      : Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A batch operation to remove a facet from an object.
--
-- /See:/ 'newBatchRemoveFacetFromObject' smart constructor.
data BatchRemoveFacetFromObject = BatchRemoveFacetFromObject'
  { -- | The facet to remove from the object.
    schemaFacet :: SchemaFacet,
    -- | A reference to the object whose facet will be removed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable BatchRemoveFacetFromObject

instance Prelude.NFData BatchRemoveFacetFromObject

instance Prelude.ToJSON BatchRemoveFacetFromObject where
  toJSON BatchRemoveFacetFromObject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaFacet" Prelude..= schemaFacet),
            Prelude.Just
              ("ObjectReference" Prelude..= objectReference)
          ]
      )
