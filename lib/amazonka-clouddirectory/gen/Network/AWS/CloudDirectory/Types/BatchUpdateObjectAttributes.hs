{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes
  ( BatchUpdateObjectAttributes (..),

    -- * Smart constructor
    mkBatchUpdateObjectAttributes,

    -- * Lenses
    buoaObjectReference,
    buoaAttributeUpdates,
  )
where

import qualified Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @BatchUpdate@ operation.
--
-- /See:/ 'mkBatchUpdateObjectAttributes' smart constructor.
data BatchUpdateObjectAttributes = BatchUpdateObjectAttributes'
  { -- | Reference that identifies the object.
    objectReference :: Types.ObjectReference,
    -- | Attributes update structure.
    attributeUpdates :: [Types.ObjectAttributeUpdate]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchUpdateObjectAttributes' value with any optional fields omitted.
mkBatchUpdateObjectAttributes ::
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchUpdateObjectAttributes
mkBatchUpdateObjectAttributes objectReference =
  BatchUpdateObjectAttributes'
    { objectReference,
      attributeUpdates = Core.mempty
    }

-- | Reference that identifies the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buoaObjectReference :: Lens.Lens' BatchUpdateObjectAttributes Types.ObjectReference
buoaObjectReference = Lens.field @"objectReference"
{-# DEPRECATED buoaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buoaAttributeUpdates :: Lens.Lens' BatchUpdateObjectAttributes [Types.ObjectAttributeUpdate]
buoaAttributeUpdates = Lens.field @"attributeUpdates"
{-# DEPRECATED buoaAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

instance Core.FromJSON BatchUpdateObjectAttributes where
  toJSON BatchUpdateObjectAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            Core.Just ("AttributeUpdates" Core..= attributeUpdates)
          ]
      )
