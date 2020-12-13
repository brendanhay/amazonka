{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Resolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Resolution
  ( Resolution (..),

    -- * Smart constructor
    mkResolution,

    -- * Lenses
    rHeight,
    rWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the screen resolution of a device in height and width, expressed in pixels.
--
-- /See:/ 'mkResolution' smart constructor.
data Resolution = Resolution'
  { -- | The screen resolution's height, expressed in pixels.
    height :: Lude.Maybe Lude.Int,
    -- | The screen resolution's width, expressed in pixels.
    width :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resolution' with the minimum fields required to make a request.
--
-- * 'height' - The screen resolution's height, expressed in pixels.
-- * 'width' - The screen resolution's width, expressed in pixels.
mkResolution ::
  Resolution
mkResolution =
  Resolution' {height = Lude.Nothing, width = Lude.Nothing}

-- | The screen resolution's height, expressed in pixels.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHeight :: Lens.Lens' Resolution (Lude.Maybe Lude.Int)
rHeight = Lens.lens (height :: Resolution -> Lude.Maybe Lude.Int) (\s a -> s {height = a} :: Resolution)
{-# DEPRECATED rHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | The screen resolution's width, expressed in pixels.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWidth :: Lens.Lens' Resolution (Lude.Maybe Lude.Int)
rWidth = Lens.lens (width :: Resolution -> Lude.Maybe Lude.Int) (\s a -> s {width = a} :: Resolution)
{-# DEPRECATED rWidth "Use generic-lens or generic-optics with 'width' instead." #-}

instance Lude.FromJSON Resolution where
  parseJSON =
    Lude.withObject
      "Resolution"
      ( \x ->
          Resolution'
            Lude.<$> (x Lude..:? "height") Lude.<*> (x Lude..:? "width")
      )
