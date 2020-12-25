{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeRange
  ( ObjectAttributeRange (..),

    -- * Smart constructor
    mkObjectAttributeRange,

    -- * Lenses
    oarAttributeKey,
    oarRange,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeKey as Types
import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValueRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A range of attributes.
--
-- /See:/ 'mkObjectAttributeRange' smart constructor.
data ObjectAttributeRange = ObjectAttributeRange'
  { -- | The key of the attribute that the attribute range covers.
    attributeKey :: Core.Maybe Types.AttributeKey,
    -- | The range of attribute values being selected.
    range :: Core.Maybe Types.TypedAttributeValueRange
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ObjectAttributeRange' value with any optional fields omitted.
mkObjectAttributeRange ::
  ObjectAttributeRange
mkObjectAttributeRange =
  ObjectAttributeRange'
    { attributeKey = Core.Nothing,
      range = Core.Nothing
    }

-- | The key of the attribute that the attribute range covers.
--
-- /Note:/ Consider using 'attributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oarAttributeKey :: Lens.Lens' ObjectAttributeRange (Core.Maybe Types.AttributeKey)
oarAttributeKey = Lens.field @"attributeKey"
{-# DEPRECATED oarAttributeKey "Use generic-lens or generic-optics with 'attributeKey' instead." #-}

-- | The range of attribute values being selected.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oarRange :: Lens.Lens' ObjectAttributeRange (Core.Maybe Types.TypedAttributeValueRange)
oarRange = Lens.field @"range"
{-# DEPRECATED oarRange "Use generic-lens or generic-optics with 'range' instead." #-}

instance Core.FromJSON ObjectAttributeRange where
  toJSON ObjectAttributeRange {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttributeKey" Core..=) Core.<$> attributeKey,
            ("Range" Core..=) Core.<$> range
          ]
      )
