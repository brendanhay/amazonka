{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.AdvancedFieldSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.AdvancedFieldSelector
  ( AdvancedFieldSelector (..),

    -- * Smart constructor
    mkAdvancedFieldSelector,

    -- * Lenses
    afsField,
    afsEndsWith,
    afsEquals,
    afsNotEndsWith,
    afsNotEquals,
    afsNotStartsWith,
    afsStartsWith,
  )
where

import qualified Network.AWS.CloudTrail.Types.OperatorValue as Types
import qualified Network.AWS.CloudTrail.Types.SelectorField as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkAdvancedFieldSelector' smart constructor.
data AdvancedFieldSelector = AdvancedFieldSelector'
  { field :: Types.SelectorField,
    endsWith :: Core.Maybe (Core.NonEmpty Types.OperatorValue),
    equals :: Core.Maybe (Core.NonEmpty Types.OperatorValue),
    notEndsWith :: Core.Maybe (Core.NonEmpty Types.OperatorValue),
    notEquals :: Core.Maybe (Core.NonEmpty Types.OperatorValue),
    notStartsWith :: Core.Maybe (Core.NonEmpty Types.OperatorValue),
    startsWith :: Core.Maybe (Core.NonEmpty Types.OperatorValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdvancedFieldSelector' value with any optional fields omitted.
mkAdvancedFieldSelector ::
  -- | 'field'
  Types.SelectorField ->
  AdvancedFieldSelector
mkAdvancedFieldSelector field =
  AdvancedFieldSelector'
    { field,
      endsWith = Core.Nothing,
      equals = Core.Nothing,
      notEndsWith = Core.Nothing,
      notEquals = Core.Nothing,
      notStartsWith = Core.Nothing,
      startsWith = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'field' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsField :: Lens.Lens' AdvancedFieldSelector Types.SelectorField
afsField = Lens.field @"field"
{-# DEPRECATED afsField "Use generic-lens or generic-optics with 'field' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsEndsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Types.OperatorValue))
afsEndsWith = Lens.field @"endsWith"
{-# DEPRECATED afsEndsWith "Use generic-lens or generic-optics with 'endsWith' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'equals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsEquals :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Types.OperatorValue))
afsEquals = Lens.field @"equals"
{-# DEPRECATED afsEquals "Use generic-lens or generic-optics with 'equals' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notEndsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsNotEndsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Types.OperatorValue))
afsNotEndsWith = Lens.field @"notEndsWith"
{-# DEPRECATED afsNotEndsWith "Use generic-lens or generic-optics with 'notEndsWith' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsNotEquals :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Types.OperatorValue))
afsNotEquals = Lens.field @"notEquals"
{-# DEPRECATED afsNotEquals "Use generic-lens or generic-optics with 'notEquals' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notStartsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsNotStartsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Types.OperatorValue))
afsNotStartsWith = Lens.field @"notStartsWith"
{-# DEPRECATED afsNotStartsWith "Use generic-lens or generic-optics with 'notStartsWith' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afsStartsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Types.OperatorValue))
afsStartsWith = Lens.field @"startsWith"
{-# DEPRECATED afsStartsWith "Use generic-lens or generic-optics with 'startsWith' instead." #-}

instance Core.FromJSON AdvancedFieldSelector where
  toJSON AdvancedFieldSelector {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Field" Core..= field),
            ("EndsWith" Core..=) Core.<$> endsWith,
            ("Equals" Core..=) Core.<$> equals,
            ("NotEndsWith" Core..=) Core.<$> notEndsWith,
            ("NotEquals" Core..=) Core.<$> notEquals,
            ("NotStartsWith" Core..=) Core.<$> notStartsWith,
            ("StartsWith" Core..=) Core.<$> startsWith
          ]
      )

instance Core.FromJSON AdvancedFieldSelector where
  parseJSON =
    Core.withObject "AdvancedFieldSelector" Core.$
      \x ->
        AdvancedFieldSelector'
          Core.<$> (x Core..: "Field")
          Core.<*> (x Core..:? "EndsWith")
          Core.<*> (x Core..:? "Equals")
          Core.<*> (x Core..:? "NotEndsWith")
          Core.<*> (x Core..:? "NotEquals")
          Core.<*> (x Core..:? "NotStartsWith")
          Core.<*> (x Core..:? "StartsWith")
