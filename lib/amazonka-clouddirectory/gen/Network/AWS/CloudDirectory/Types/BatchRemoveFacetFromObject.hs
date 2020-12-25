{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject
  ( BatchRemoveFacetFromObject (..),

    -- * Smart constructor
    mkBatchRemoveFacetFromObject,

    -- * Lenses
    brffoSchemaFacet,
    brffoObjectReference,
  )
where

import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.CloudDirectory.Types.SchemaFacet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A batch operation to remove a facet from an object.
--
-- /See:/ 'mkBatchRemoveFacetFromObject' smart constructor.
data BatchRemoveFacetFromObject = BatchRemoveFacetFromObject'
  { -- | The facet to remove from the object.
    schemaFacet :: Types.SchemaFacet,
    -- | A reference to the object whose facet will be removed.
    objectReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchRemoveFacetFromObject' value with any optional fields omitted.
mkBatchRemoveFacetFromObject ::
  -- | 'schemaFacet'
  Types.SchemaFacet ->
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchRemoveFacetFromObject
mkBatchRemoveFacetFromObject schemaFacet objectReference =
  BatchRemoveFacetFromObject' {schemaFacet, objectReference}

-- | The facet to remove from the object.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brffoSchemaFacet :: Lens.Lens' BatchRemoveFacetFromObject Types.SchemaFacet
brffoSchemaFacet = Lens.field @"schemaFacet"
{-# DEPRECATED brffoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | A reference to the object whose facet will be removed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brffoObjectReference :: Lens.Lens' BatchRemoveFacetFromObject Types.ObjectReference
brffoObjectReference = Lens.field @"objectReference"
{-# DEPRECATED brffoObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Core.FromJSON BatchRemoveFacetFromObject where
  toJSON BatchRemoveFacetFromObject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaFacet" Core..= schemaFacet),
            Core.Just ("ObjectReference" Core..= objectReference)
          ]
      )
