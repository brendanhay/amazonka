{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeConfiguration
  ( SlotTypeConfiguration (..),

    -- * Smart constructor
    mkSlotTypeConfiguration,

    -- * Lenses
    stcRegexConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.SlotTypeRegexConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Provides configuration information for a slot type.
--
-- /See:/ 'mkSlotTypeConfiguration' smart constructor.
newtype SlotTypeConfiguration = SlotTypeConfiguration'
  { -- | A regular expression used to validate the value of a slot.
    regexConfiguration :: Core.Maybe Types.SlotTypeRegexConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SlotTypeConfiguration' value with any optional fields omitted.
mkSlotTypeConfiguration ::
  SlotTypeConfiguration
mkSlotTypeConfiguration =
  SlotTypeConfiguration' {regexConfiguration = Core.Nothing}

-- | A regular expression used to validate the value of a slot.
--
-- /Note:/ Consider using 'regexConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stcRegexConfiguration :: Lens.Lens' SlotTypeConfiguration (Core.Maybe Types.SlotTypeRegexConfiguration)
stcRegexConfiguration = Lens.field @"regexConfiguration"
{-# DEPRECATED stcRegexConfiguration "Use generic-lens or generic-optics with 'regexConfiguration' instead." #-}

instance Core.FromJSON SlotTypeConfiguration where
  toJSON SlotTypeConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [("regexConfiguration" Core..=) Core.<$> regexConfiguration]
      )

instance Core.FromJSON SlotTypeConfiguration where
  parseJSON =
    Core.withObject "SlotTypeConfiguration" Core.$
      \x ->
        SlotTypeConfiguration' Core.<$> (x Core..:? "regexConfiguration")
