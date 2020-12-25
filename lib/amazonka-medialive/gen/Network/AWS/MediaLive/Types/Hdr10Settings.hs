{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Hdr10Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Hdr10Settings
  ( Hdr10Settings (..),

    -- * Smart constructor
    mkHdr10Settings,

    -- * Lenses
    hsMaxCll,
    hsMaxFall,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Hdr10 Settings
--
-- /See:/ 'mkHdr10Settings' smart constructor.
data Hdr10Settings = Hdr10Settings'
  { -- | Maximum Content Light Level
    --
    -- An integer metadata value defining the maximum light level, in nits,
    -- of any single pixel within an encoded HDR video stream or file.
    maxCll :: Core.Maybe Core.Natural,
    -- | Maximum Frame Average Light Level
    --
    -- An integer metadata value defining the maximum average light level, in nits,
    -- for any single frame within an encoded HDR video stream or file.
    maxFall :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Hdr10Settings' value with any optional fields omitted.
mkHdr10Settings ::
  Hdr10Settings
mkHdr10Settings =
  Hdr10Settings' {maxCll = Core.Nothing, maxFall = Core.Nothing}

-- | Maximum Content Light Level
--
-- An integer metadata value defining the maximum light level, in nits,
-- of any single pixel within an encoded HDR video stream or file.
--
-- /Note:/ Consider using 'maxCll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxCll :: Lens.Lens' Hdr10Settings (Core.Maybe Core.Natural)
hsMaxCll = Lens.field @"maxCll"
{-# DEPRECATED hsMaxCll "Use generic-lens or generic-optics with 'maxCll' instead." #-}

-- | Maximum Frame Average Light Level
--
-- An integer metadata value defining the maximum average light level, in nits,
-- for any single frame within an encoded HDR video stream or file.
--
-- /Note:/ Consider using 'maxFall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxFall :: Lens.Lens' Hdr10Settings (Core.Maybe Core.Natural)
hsMaxFall = Lens.field @"maxFall"
{-# DEPRECATED hsMaxFall "Use generic-lens or generic-optics with 'maxFall' instead." #-}

instance Core.FromJSON Hdr10Settings where
  toJSON Hdr10Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxCll" Core..=) Core.<$> maxCll,
            ("maxFall" Core..=) Core.<$> maxFall
          ]
      )

instance Core.FromJSON Hdr10Settings where
  parseJSON =
    Core.withObject "Hdr10Settings" Core.$
      \x ->
        Hdr10Settings'
          Core.<$> (x Core..:? "maxCll") Core.<*> (x Core..:? "maxFall")
