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

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A batch operation to remove a facet from an object.
--
-- /See:/ 'mkBatchRemoveFacetFromObject' smart constructor.
data BatchRemoveFacetFromObject = BatchRemoveFacetFromObject'
  { -- | The facet to remove from the object.
    schemaFacet :: SchemaFacet,
    -- | A reference to the object whose facet will be removed.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchRemoveFacetFromObject' with the minimum fields required to make a request.
--
-- * 'schemaFacet' - The facet to remove from the object.
-- * 'objectReference' - A reference to the object whose facet will be removed.
mkBatchRemoveFacetFromObject ::
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  BatchRemoveFacetFromObject
mkBatchRemoveFacetFromObject pSchemaFacet_ pObjectReference_ =
  BatchRemoveFacetFromObject'
    { schemaFacet = pSchemaFacet_,
      objectReference = pObjectReference_
    }

-- | The facet to remove from the object.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brffoSchemaFacet :: Lens.Lens' BatchRemoveFacetFromObject SchemaFacet
brffoSchemaFacet = Lens.lens (schemaFacet :: BatchRemoveFacetFromObject -> SchemaFacet) (\s a -> s {schemaFacet = a} :: BatchRemoveFacetFromObject)
{-# DEPRECATED brffoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | A reference to the object whose facet will be removed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brffoObjectReference :: Lens.Lens' BatchRemoveFacetFromObject ObjectReference
brffoObjectReference = Lens.lens (objectReference :: BatchRemoveFacetFromObject -> ObjectReference) (\s a -> s {objectReference = a} :: BatchRemoveFacetFromObject)
{-# DEPRECATED brffoObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchRemoveFacetFromObject where
  toJSON BatchRemoveFacetFromObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaFacet" Lude..= schemaFacet),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
