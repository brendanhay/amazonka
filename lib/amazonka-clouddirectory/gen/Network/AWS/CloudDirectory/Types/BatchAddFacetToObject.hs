{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAddFacetToObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAddFacetToObject
  ( BatchAddFacetToObject (..),

    -- * Smart constructor
    mkBatchAddFacetToObject,

    -- * Lenses
    baftoSchemaFacet,
    baftoObjectAttributeList,
    baftoObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a batch add facet to object operation.
--
-- /See:/ 'mkBatchAddFacetToObject' smart constructor.
data BatchAddFacetToObject = BatchAddFacetToObject'
  { -- | Represents the facet being added to the object.
    schemaFacet :: SchemaFacet,
    -- | The attributes to set on the object.
    objectAttributeList :: [AttributeKeyAndValue],
    -- | A reference to the object being mutated.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAddFacetToObject' with the minimum fields required to make a request.
--
-- * 'schemaFacet' - Represents the facet being added to the object.
-- * 'objectAttributeList' - The attributes to set on the object.
-- * 'objectReference' - A reference to the object being mutated.
mkBatchAddFacetToObject ::
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  BatchAddFacetToObject
mkBatchAddFacetToObject pSchemaFacet_ pObjectReference_ =
  BatchAddFacetToObject'
    { schemaFacet = pSchemaFacet_,
      objectAttributeList = Lude.mempty,
      objectReference = pObjectReference_
    }

-- | Represents the facet being added to the object.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baftoSchemaFacet :: Lens.Lens' BatchAddFacetToObject SchemaFacet
baftoSchemaFacet = Lens.lens (schemaFacet :: BatchAddFacetToObject -> SchemaFacet) (\s a -> s {schemaFacet = a} :: BatchAddFacetToObject)
{-# DEPRECATED baftoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | The attributes to set on the object.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baftoObjectAttributeList :: Lens.Lens' BatchAddFacetToObject [AttributeKeyAndValue]
baftoObjectAttributeList = Lens.lens (objectAttributeList :: BatchAddFacetToObject -> [AttributeKeyAndValue]) (\s a -> s {objectAttributeList = a} :: BatchAddFacetToObject)
{-# DEPRECATED baftoObjectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead." #-}

-- | A reference to the object being mutated.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baftoObjectReference :: Lens.Lens' BatchAddFacetToObject ObjectReference
baftoObjectReference = Lens.lens (objectReference :: BatchAddFacetToObject -> ObjectReference) (\s a -> s {objectReference = a} :: BatchAddFacetToObject)
{-# DEPRECATED baftoObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchAddFacetToObject where
  toJSON BatchAddFacetToObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaFacet" Lude..= schemaFacet),
            Lude.Just ("ObjectAttributeList" Lude..= objectAttributeList),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
