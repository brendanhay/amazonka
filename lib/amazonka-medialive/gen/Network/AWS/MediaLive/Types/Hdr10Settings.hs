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
    hsMaxFall,
    hsMaxCll,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Hdr10 Settings
--
-- /See:/ 'mkHdr10Settings' smart constructor.
data Hdr10Settings = Hdr10Settings'
  { -- | Maximum Frame Average Light Level
    --
    -- An integer metadata value defining the maximum average light level, in nits,
    -- for any single frame within an encoded HDR video stream or file.
    maxFall :: Lude.Maybe Lude.Natural,
    -- | Maximum Content Light Level
    --
    -- An integer metadata value defining the maximum light level, in nits,
    -- of any single pixel within an encoded HDR video stream or file.
    maxCll :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Hdr10Settings' with the minimum fields required to make a request.
--
-- * 'maxFall' - Maximum Frame Average Light Level
--
-- An integer metadata value defining the maximum average light level, in nits,
-- for any single frame within an encoded HDR video stream or file.
-- * 'maxCll' - Maximum Content Light Level
--
-- An integer metadata value defining the maximum light level, in nits,
-- of any single pixel within an encoded HDR video stream or file.
mkHdr10Settings ::
  Hdr10Settings
mkHdr10Settings =
  Hdr10Settings' {maxFall = Lude.Nothing, maxCll = Lude.Nothing}

-- | Maximum Frame Average Light Level
--
-- An integer metadata value defining the maximum average light level, in nits,
-- for any single frame within an encoded HDR video stream or file.
--
-- /Note:/ Consider using 'maxFall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxFall :: Lens.Lens' Hdr10Settings (Lude.Maybe Lude.Natural)
hsMaxFall = Lens.lens (maxFall :: Hdr10Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxFall = a} :: Hdr10Settings)
{-# DEPRECATED hsMaxFall "Use generic-lens or generic-optics with 'maxFall' instead." #-}

-- | Maximum Content Light Level
--
-- An integer metadata value defining the maximum light level, in nits,
-- of any single pixel within an encoded HDR video stream or file.
--
-- /Note:/ Consider using 'maxCll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsMaxCll :: Lens.Lens' Hdr10Settings (Lude.Maybe Lude.Natural)
hsMaxCll = Lens.lens (maxCll :: Hdr10Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxCll = a} :: Hdr10Settings)
{-# DEPRECATED hsMaxCll "Use generic-lens or generic-optics with 'maxCll' instead." #-}

instance Lude.FromJSON Hdr10Settings where
  parseJSON =
    Lude.withObject
      "Hdr10Settings"
      ( \x ->
          Hdr10Settings'
            Lude.<$> (x Lude..:? "maxFall") Lude.<*> (x Lude..:? "maxCll")
      )

instance Lude.ToJSON Hdr10Settings where
  toJSON Hdr10Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("maxFall" Lude..=) Lude.<$> maxFall,
            ("maxCll" Lude..=) Lude.<$> maxCll
          ]
      )
