{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
  ( EbuTtDDestinationSettings (..),

    -- * Smart constructor
    mkEbuTtDDestinationSettings,

    -- * Lenses
    etddsFillLineGap,
    etddsFontFamily,
    etddsStyleControl,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.EbuTtDDestinationStyleControl
import Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl
import qualified Network.AWS.Prelude as Lude

-- | Ebu Tt DDestination Settings
--
-- /See:/ 'mkEbuTtDDestinationSettings' smart constructor.
data EbuTtDDestinationSettings = EbuTtDDestinationSettings'
  { -- | Specifies how to handle the gap between the lines (in multi-line captions).
    --
    --
    -- - enabled: Fill with the captions background color (as specified in the input captions).
    -- - disabled: Leave the gap unfilled.
    fillLineGap :: Lude.Maybe EbuTtDFillLineGapControl,
    -- | Specifies the font family to include in the font data attached to the EBU-TT captions. Valid only if styleControl is set to include. If you leave this field empty, the font family is set to "monospaced". (If styleControl is set to exclude, the font family is always set to "monospaced".)
    --
    --
    -- You specify only the font family. All other style information (color, bold, position and so on) is copied from the input captions. The size is always set to 100% to allow the downstream player to choose the size.
    --
    -- - Enter a list of font families, as a comma-separated list of font names, in order of preference. The name can be a font family (such as “Arial”), or a generic font family (such as “serif”), or “default” (to let the downstream player choose the font).
    -- - Leave blank to set the family to “monospace”.
    fontFamily :: Lude.Maybe Lude.Text,
    -- | Specifies the style information (font color, font position, and so on) to include in the font data that is attached to the EBU-TT captions.
    --
    --
    -- - include: Take the style information (font color, font position, and so on) from the source captions and include that information in the font data attached to the EBU-TT captions. This option is valid only if the source captions are Embedded or Teletext.
    -- - exclude: In the font data attached to the EBU-TT captions, set the font family to "monospaced". Do not include any other style information.
    styleControl :: Lude.Maybe EbuTtDDestinationStyleControl
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EbuTtDDestinationSettings' with the minimum fields required to make a request.
--
-- * 'fillLineGap' - Specifies how to handle the gap between the lines (in multi-line captions).
--
--
-- - enabled: Fill with the captions background color (as specified in the input captions).
-- - disabled: Leave the gap unfilled.
-- * 'fontFamily' - Specifies the font family to include in the font data attached to the EBU-TT captions. Valid only if styleControl is set to include. If you leave this field empty, the font family is set to "monospaced". (If styleControl is set to exclude, the font family is always set to "monospaced".)
--
--
-- You specify only the font family. All other style information (color, bold, position and so on) is copied from the input captions. The size is always set to 100% to allow the downstream player to choose the size.
--
-- - Enter a list of font families, as a comma-separated list of font names, in order of preference. The name can be a font family (such as “Arial”), or a generic font family (such as “serif”), or “default” (to let the downstream player choose the font).
-- - Leave blank to set the family to “monospace”.
-- * 'styleControl' - Specifies the style information (font color, font position, and so on) to include in the font data that is attached to the EBU-TT captions.
--
--
-- - include: Take the style information (font color, font position, and so on) from the source captions and include that information in the font data attached to the EBU-TT captions. This option is valid only if the source captions are Embedded or Teletext.
-- - exclude: In the font data attached to the EBU-TT captions, set the font family to "monospaced". Do not include any other style information.
mkEbuTtDDestinationSettings ::
  EbuTtDDestinationSettings
mkEbuTtDDestinationSettings =
  EbuTtDDestinationSettings'
    { fillLineGap = Lude.Nothing,
      fontFamily = Lude.Nothing,
      styleControl = Lude.Nothing
    }

-- | Specifies how to handle the gap between the lines (in multi-line captions).
--
--
-- - enabled: Fill with the captions background color (as specified in the input captions).
-- - disabled: Leave the gap unfilled.
--
-- /Note:/ Consider using 'fillLineGap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etddsFillLineGap :: Lens.Lens' EbuTtDDestinationSettings (Lude.Maybe EbuTtDFillLineGapControl)
etddsFillLineGap = Lens.lens (fillLineGap :: EbuTtDDestinationSettings -> Lude.Maybe EbuTtDFillLineGapControl) (\s a -> s {fillLineGap = a} :: EbuTtDDestinationSettings)
{-# DEPRECATED etddsFillLineGap "Use generic-lens or generic-optics with 'fillLineGap' instead." #-}

-- | Specifies the font family to include in the font data attached to the EBU-TT captions. Valid only if styleControl is set to include. If you leave this field empty, the font family is set to "monospaced". (If styleControl is set to exclude, the font family is always set to "monospaced".)
--
--
-- You specify only the font family. All other style information (color, bold, position and so on) is copied from the input captions. The size is always set to 100% to allow the downstream player to choose the size.
--
-- - Enter a list of font families, as a comma-separated list of font names, in order of preference. The name can be a font family (such as “Arial”), or a generic font family (such as “serif”), or “default” (to let the downstream player choose the font).
-- - Leave blank to set the family to “monospace”.
--
-- /Note:/ Consider using 'fontFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etddsFontFamily :: Lens.Lens' EbuTtDDestinationSettings (Lude.Maybe Lude.Text)
etddsFontFamily = Lens.lens (fontFamily :: EbuTtDDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {fontFamily = a} :: EbuTtDDestinationSettings)
{-# DEPRECATED etddsFontFamily "Use generic-lens or generic-optics with 'fontFamily' instead." #-}

-- | Specifies the style information (font color, font position, and so on) to include in the font data that is attached to the EBU-TT captions.
--
--
-- - include: Take the style information (font color, font position, and so on) from the source captions and include that information in the font data attached to the EBU-TT captions. This option is valid only if the source captions are Embedded or Teletext.
-- - exclude: In the font data attached to the EBU-TT captions, set the font family to "monospaced". Do not include any other style information.
--
-- /Note:/ Consider using 'styleControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etddsStyleControl :: Lens.Lens' EbuTtDDestinationSettings (Lude.Maybe EbuTtDDestinationStyleControl)
etddsStyleControl = Lens.lens (styleControl :: EbuTtDDestinationSettings -> Lude.Maybe EbuTtDDestinationStyleControl) (\s a -> s {styleControl = a} :: EbuTtDDestinationSettings)
{-# DEPRECATED etddsStyleControl "Use generic-lens or generic-optics with 'styleControl' instead." #-}

instance Lude.FromJSON EbuTtDDestinationSettings where
  parseJSON =
    Lude.withObject
      "EbuTtDDestinationSettings"
      ( \x ->
          EbuTtDDestinationSettings'
            Lude.<$> (x Lude..:? "fillLineGap")
            Lude.<*> (x Lude..:? "fontFamily")
            Lude.<*> (x Lude..:? "styleControl")
      )

instance Lude.ToJSON EbuTtDDestinationSettings where
  toJSON EbuTtDDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fillLineGap" Lude..=) Lude.<$> fillLineGap,
            ("fontFamily" Lude..=) Lude.<$> fontFamily,
            ("styleControl" Lude..=) Lude.<$> styleControl
          ]
      )
