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
    buoaAttributeUpdates,
    buoaObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @BatchUpdate@ operation.
--
-- /See:/ 'mkBatchUpdateObjectAttributes' smart constructor.
data BatchUpdateObjectAttributes = BatchUpdateObjectAttributes'
  { -- | Attributes update structure.
    attributeUpdates :: [ObjectAttributeUpdate],
    -- | Reference that identifies the object.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdateObjectAttributes' with the minimum fields required to make a request.
--
-- * 'attributeUpdates' - Attributes update structure.
-- * 'objectReference' - Reference that identifies the object.
mkBatchUpdateObjectAttributes ::
  -- | 'objectReference'
  ObjectReference ->
  BatchUpdateObjectAttributes
mkBatchUpdateObjectAttributes pObjectReference_ =
  BatchUpdateObjectAttributes'
    { attributeUpdates = Lude.mempty,
      objectReference = pObjectReference_
    }

-- | Attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buoaAttributeUpdates :: Lens.Lens' BatchUpdateObjectAttributes [ObjectAttributeUpdate]
buoaAttributeUpdates = Lens.lens (attributeUpdates :: BatchUpdateObjectAttributes -> [ObjectAttributeUpdate]) (\s a -> s {attributeUpdates = a} :: BatchUpdateObjectAttributes)
{-# DEPRECATED buoaAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

-- | Reference that identifies the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buoaObjectReference :: Lens.Lens' BatchUpdateObjectAttributes ObjectReference
buoaObjectReference = Lens.lens (objectReference :: BatchUpdateObjectAttributes -> ObjectReference) (\s a -> s {objectReference = a} :: BatchUpdateObjectAttributes)
{-# DEPRECATED buoaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchUpdateObjectAttributes where
  toJSON BatchUpdateObjectAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeUpdates" Lude..= attributeUpdates),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
