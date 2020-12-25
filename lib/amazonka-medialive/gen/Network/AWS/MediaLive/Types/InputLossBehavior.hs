{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossBehavior
  ( InputLossBehavior (..),

    -- * Smart constructor
    mkInputLossBehavior,

    -- * Lenses
    ilbBlackFrameMsec,
    ilbInputLossImageColor,
    ilbInputLossImageSlate,
    ilbInputLossImageType,
    ilbRepeatFrameMsec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputLocation as Types
import qualified Network.AWS.MediaLive.Types.InputLossImageType as Types
import qualified Network.AWS.Prelude as Core

-- | Input Loss Behavior
--
-- /See:/ 'mkInputLossBehavior' smart constructor.
data InputLossBehavior = InputLossBehavior'
  { -- | Documentation update needed
    blackFrameMsec :: Core.Maybe Core.Natural,
    -- | When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
    inputLossImageColor :: Core.Maybe Core.Text,
    -- | When input loss image type is "slate" these fields specify the parameters for accessing the slate.
    inputLossImageSlate :: Core.Maybe Types.InputLocation,
    -- | Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
    inputLossImageType :: Core.Maybe Types.InputLossImageType,
    -- | Documentation update needed
    repeatFrameMsec :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputLossBehavior' value with any optional fields omitted.
mkInputLossBehavior ::
  InputLossBehavior
mkInputLossBehavior =
  InputLossBehavior'
    { blackFrameMsec = Core.Nothing,
      inputLossImageColor = Core.Nothing,
      inputLossImageSlate = Core.Nothing,
      inputLossImageType = Core.Nothing,
      repeatFrameMsec = Core.Nothing
    }

-- | Documentation update needed
--
-- /Note:/ Consider using 'blackFrameMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbBlackFrameMsec :: Lens.Lens' InputLossBehavior (Core.Maybe Core.Natural)
ilbBlackFrameMsec = Lens.field @"blackFrameMsec"
{-# DEPRECATED ilbBlackFrameMsec "Use generic-lens or generic-optics with 'blackFrameMsec' instead." #-}

-- | When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
--
-- /Note:/ Consider using 'inputLossImageColor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbInputLossImageColor :: Lens.Lens' InputLossBehavior (Core.Maybe Core.Text)
ilbInputLossImageColor = Lens.field @"inputLossImageColor"
{-# DEPRECATED ilbInputLossImageColor "Use generic-lens or generic-optics with 'inputLossImageColor' instead." #-}

-- | When input loss image type is "slate" these fields specify the parameters for accessing the slate.
--
-- /Note:/ Consider using 'inputLossImageSlate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbInputLossImageSlate :: Lens.Lens' InputLossBehavior (Core.Maybe Types.InputLocation)
ilbInputLossImageSlate = Lens.field @"inputLossImageSlate"
{-# DEPRECATED ilbInputLossImageSlate "Use generic-lens or generic-optics with 'inputLossImageSlate' instead." #-}

-- | Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
--
-- /Note:/ Consider using 'inputLossImageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbInputLossImageType :: Lens.Lens' InputLossBehavior (Core.Maybe Types.InputLossImageType)
ilbInputLossImageType = Lens.field @"inputLossImageType"
{-# DEPRECATED ilbInputLossImageType "Use generic-lens or generic-optics with 'inputLossImageType' instead." #-}

-- | Documentation update needed
--
-- /Note:/ Consider using 'repeatFrameMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilbRepeatFrameMsec :: Lens.Lens' InputLossBehavior (Core.Maybe Core.Natural)
ilbRepeatFrameMsec = Lens.field @"repeatFrameMsec"
{-# DEPRECATED ilbRepeatFrameMsec "Use generic-lens or generic-optics with 'repeatFrameMsec' instead." #-}

instance Core.FromJSON InputLossBehavior where
  toJSON InputLossBehavior {..} =
    Core.object
      ( Core.catMaybes
          [ ("blackFrameMsec" Core..=) Core.<$> blackFrameMsec,
            ("inputLossImageColor" Core..=) Core.<$> inputLossImageColor,
            ("inputLossImageSlate" Core..=) Core.<$> inputLossImageSlate,
            ("inputLossImageType" Core..=) Core.<$> inputLossImageType,
            ("repeatFrameMsec" Core..=) Core.<$> repeatFrameMsec
          ]
      )

instance Core.FromJSON InputLossBehavior where
  parseJSON =
    Core.withObject "InputLossBehavior" Core.$
      \x ->
        InputLossBehavior'
          Core.<$> (x Core..:? "blackFrameMsec")
          Core.<*> (x Core..:? "inputLossImageColor")
          Core.<*> (x Core..:? "inputLossImageSlate")
          Core.<*> (x Core..:? "inputLossImageType")
          Core.<*> (x Core..:? "repeatFrameMsec")
