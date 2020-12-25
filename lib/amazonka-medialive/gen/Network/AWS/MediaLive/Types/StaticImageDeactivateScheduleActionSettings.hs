{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
  ( StaticImageDeactivateScheduleActionSettings (..),

    -- * Smart constructor
    mkStaticImageDeactivateScheduleActionSettings,

    -- * Lenses
    sidsasFadeOut,
    sidsasLayer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the action to deactivate the image in a specific layer.
--
-- /See:/ 'mkStaticImageDeactivateScheduleActionSettings' smart constructor.
data StaticImageDeactivateScheduleActionSettings = StaticImageDeactivateScheduleActionSettings'
  { -- | The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
    fadeOut :: Core.Maybe Core.Natural,
    -- | The image overlay layer to deactivate, 0 to 7. Default is 0.
    layer :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StaticImageDeactivateScheduleActionSettings' value with any optional fields omitted.
mkStaticImageDeactivateScheduleActionSettings ::
  StaticImageDeactivateScheduleActionSettings
mkStaticImageDeactivateScheduleActionSettings =
  StaticImageDeactivateScheduleActionSettings'
    { fadeOut =
        Core.Nothing,
      layer = Core.Nothing
    }

-- | The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
--
-- /Note:/ Consider using 'fadeOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sidsasFadeOut :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Core.Maybe Core.Natural)
sidsasFadeOut = Lens.field @"fadeOut"
{-# DEPRECATED sidsasFadeOut "Use generic-lens or generic-optics with 'fadeOut' instead." #-}

-- | The image overlay layer to deactivate, 0 to 7. Default is 0.
--
-- /Note:/ Consider using 'layer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sidsasLayer :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Core.Maybe Core.Natural)
sidsasLayer = Lens.field @"layer"
{-# DEPRECATED sidsasLayer "Use generic-lens or generic-optics with 'layer' instead." #-}

instance Core.FromJSON StaticImageDeactivateScheduleActionSettings where
  toJSON StaticImageDeactivateScheduleActionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("fadeOut" Core..=) Core.<$> fadeOut,
            ("layer" Core..=) Core.<$> layer
          ]
      )

instance Core.FromJSON StaticImageDeactivateScheduleActionSettings where
  parseJSON =
    Core.withObject "StaticImageDeactivateScheduleActionSettings" Core.$
      \x ->
        StaticImageDeactivateScheduleActionSettings'
          Core.<$> (x Core..:? "fadeOut") Core.<*> (x Core..:? "layer")
