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

import qualified Network.AWS.CloudDirectory.Types.AttributeKeyAndValue as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.CloudDirectory.Types.SchemaFacet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a batch add facet to object operation.
--
-- /See:/ 'mkBatchAddFacetToObject' smart constructor.
data BatchAddFacetToObject = BatchAddFacetToObject'
  { -- | Represents the facet being added to the object.
    schemaFacet :: Types.SchemaFacet,
    -- | The attributes to set on the object.
    objectAttributeList :: [Types.AttributeKeyAndValue],
    -- | A reference to the object being mutated.
    objectReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchAddFacetToObject' value with any optional fields omitted.
mkBatchAddFacetToObject ::
  -- | 'schemaFacet'
  Types.SchemaFacet ->
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchAddFacetToObject
mkBatchAddFacetToObject schemaFacet objectReference =
  BatchAddFacetToObject'
    { schemaFacet,
      objectAttributeList = Core.mempty,
      objectReference
    }

-- | Represents the facet being added to the object.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baftoSchemaFacet :: Lens.Lens' BatchAddFacetToObject Types.SchemaFacet
baftoSchemaFacet = Lens.field @"schemaFacet"
{-# DEPRECATED baftoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | The attributes to set on the object.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baftoObjectAttributeList :: Lens.Lens' BatchAddFacetToObject [Types.AttributeKeyAndValue]
baftoObjectAttributeList = Lens.field @"objectAttributeList"
{-# DEPRECATED baftoObjectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead." #-}

-- | A reference to the object being mutated.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baftoObjectReference :: Lens.Lens' BatchAddFacetToObject Types.ObjectReference
baftoObjectReference = Lens.field @"objectReference"
{-# DEPRECATED baftoObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Core.FromJSON BatchAddFacetToObject where
  toJSON BatchAddFacetToObject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaFacet" Core..= schemaFacet),
            Core.Just ("ObjectAttributeList" Core..= objectAttributeList),
            Core.Just ("ObjectReference" Core..= objectReference)
          ]
      )
