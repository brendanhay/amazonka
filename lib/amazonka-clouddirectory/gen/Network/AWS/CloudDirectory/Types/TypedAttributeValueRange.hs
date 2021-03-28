{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
  ( TypedAttributeValueRange (..)
  -- * Smart constructor
  , mkTypedAttributeValueRange
  -- * Lenses
  , tavrStartMode
  , tavrEndMode
  , tavrEndValue
  , tavrStartValue
  ) where

import qualified Network.AWS.CloudDirectory.Types.RangeMode as Types
import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A range of attribute values. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_range_filters.html Range Filters> .
--
-- /See:/ 'mkTypedAttributeValueRange' smart constructor.
data TypedAttributeValueRange = TypedAttributeValueRange'
  { startMode :: Types.RangeMode
    -- ^ The inclusive or exclusive range start.
  , endMode :: Types.RangeMode
    -- ^ The inclusive or exclusive range end.
  , endValue :: Core.Maybe Types.TypedAttributeValue
    -- ^ The attribute value to terminate the range at.
  , startValue :: Core.Maybe Types.TypedAttributeValue
    -- ^ The value to start the range at.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TypedAttributeValueRange' value with any optional fields omitted.
mkTypedAttributeValueRange
    :: Types.RangeMode -- ^ 'startMode'
    -> Types.RangeMode -- ^ 'endMode'
    -> TypedAttributeValueRange
mkTypedAttributeValueRange startMode endMode
  = TypedAttributeValueRange'{startMode, endMode,
                              endValue = Core.Nothing, startValue = Core.Nothing}

-- | The inclusive or exclusive range start.
--
-- /Note:/ Consider using 'startMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrStartMode :: Lens.Lens' TypedAttributeValueRange Types.RangeMode
tavrStartMode = Lens.field @"startMode"
{-# INLINEABLE tavrStartMode #-}
{-# DEPRECATED startMode "Use generic-lens or generic-optics with 'startMode' instead"  #-}

-- | The inclusive or exclusive range end.
--
-- /Note:/ Consider using 'endMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrEndMode :: Lens.Lens' TypedAttributeValueRange Types.RangeMode
tavrEndMode = Lens.field @"endMode"
{-# INLINEABLE tavrEndMode #-}
{-# DEPRECATED endMode "Use generic-lens or generic-optics with 'endMode' instead"  #-}

-- | The attribute value to terminate the range at.
--
-- /Note:/ Consider using 'endValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrEndValue :: Lens.Lens' TypedAttributeValueRange (Core.Maybe Types.TypedAttributeValue)
tavrEndValue = Lens.field @"endValue"
{-# INLINEABLE tavrEndValue #-}
{-# DEPRECATED endValue "Use generic-lens or generic-optics with 'endValue' instead"  #-}

-- | The value to start the range at.
--
-- /Note:/ Consider using 'startValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrStartValue :: Lens.Lens' TypedAttributeValueRange (Core.Maybe Types.TypedAttributeValue)
tavrStartValue = Lens.field @"startValue"
{-# INLINEABLE tavrStartValue #-}
{-# DEPRECATED startValue "Use generic-lens or generic-optics with 'startValue' instead"  #-}

instance Core.FromJSON TypedAttributeValueRange where
        toJSON TypedAttributeValueRange{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StartMode" Core..= startMode),
                  Core.Just ("EndMode" Core..= endMode),
                  ("EndValue" Core..=) Core.<$> endValue,
                  ("StartValue" Core..=) Core.<$> startValue])
