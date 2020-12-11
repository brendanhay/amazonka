-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurnin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeBurnin
  ( TimecodeBurnin (..),

    -- * Smart constructor
    mkTimecodeBurnin,

    -- * Lenses
    tbPrefix,
    tbFontSize,
    tbPosition,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TimecodeBurninPosition
import qualified Network.AWS.Prelude as Lude

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- /See:/ 'mkTimecodeBurnin' smart constructor.
data TimecodeBurnin = TimecodeBurnin'
  { prefix ::
      Lude.Maybe Lude.Text,
    fontSize :: Lude.Maybe Lude.Natural,
    position :: Lude.Maybe TimecodeBurninPosition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimecodeBurnin' with the minimum fields required to make a request.
--
-- * 'fontSize' - Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
-- * 'position' - Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
-- * 'prefix' - Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
mkTimecodeBurnin ::
  TimecodeBurnin
mkTimecodeBurnin =
  TimecodeBurnin'
    { prefix = Lude.Nothing,
      fontSize = Lude.Nothing,
      position = Lude.Nothing
    }

-- | Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbPrefix :: Lens.Lens' TimecodeBurnin (Lude.Maybe Lude.Text)
tbPrefix = Lens.lens (prefix :: TimecodeBurnin -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: TimecodeBurnin)
{-# DEPRECATED tbPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbFontSize :: Lens.Lens' TimecodeBurnin (Lude.Maybe Lude.Natural)
tbFontSize = Lens.lens (fontSize :: TimecodeBurnin -> Lude.Maybe Lude.Natural) (\s a -> s {fontSize = a} :: TimecodeBurnin)
{-# DEPRECATED tbFontSize "Use generic-lens or generic-optics with 'fontSize' instead." #-}

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbPosition :: Lens.Lens' TimecodeBurnin (Lude.Maybe TimecodeBurninPosition)
tbPosition = Lens.lens (position :: TimecodeBurnin -> Lude.Maybe TimecodeBurninPosition) (\s a -> s {position = a} :: TimecodeBurnin)
{-# DEPRECATED tbPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Lude.FromJSON TimecodeBurnin where
  parseJSON =
    Lude.withObject
      "TimecodeBurnin"
      ( \x ->
          TimecodeBurnin'
            Lude.<$> (x Lude..:? "prefix")
            Lude.<*> (x Lude..:? "fontSize")
            Lude.<*> (x Lude..:? "position")
      )

instance Lude.ToJSON TimecodeBurnin where
  toJSON TimecodeBurnin' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("prefix" Lude..=) Lude.<$> prefix,
            ("fontSize" Lude..=) Lude.<$> fontSize,
            ("position" Lude..=) Lude.<$> position
          ]
      )
