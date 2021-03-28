{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
  ( EbuTtDDestinationSettings (..)
  -- * Smart constructor
  , mkEbuTtDDestinationSettings
  -- * Lenses
  , etddsFillLineGap
  , etddsFontFamily
  , etddsStyleControl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.EbuTtDDestinationStyleControl as Types
import qualified Network.AWS.MediaLive.Types.EbuTtDFillLineGapControl as Types
import qualified Network.AWS.Prelude as Core

-- | Ebu Tt DDestination Settings
--
-- /See:/ 'mkEbuTtDDestinationSettings' smart constructor.
data EbuTtDDestinationSettings = EbuTtDDestinationSettings'
  { fillLineGap :: Core.Maybe Types.EbuTtDFillLineGapControl
    -- ^ Specifies how to handle the gap between the lines (in multi-line captions).
--
--
-- - enabled: Fill with the captions background color (as specified in the input captions).
-- - disabled: Leave the gap unfilled.
  , fontFamily :: Core.Maybe Core.Text
    -- ^ Specifies the font family to include in the font data attached to the EBU-TT captions. Valid only if styleControl is set to include. If you leave this field empty, the font family is set to "monospaced". (If styleControl is set to exclude, the font family is always set to "monospaced".)
--
--
-- You specify only the font family. All other style information (color, bold, position and so on) is copied from the input captions. The size is always set to 100% to allow the downstream player to choose the size.
--
-- - Enter a list of font families, as a comma-separated list of font names, in order of preference. The name can be a font family (such as “Arial”), or a generic font family (such as “serif”), or “default” (to let the downstream player choose the font).
-- - Leave blank to set the family to “monospace”.
  , styleControl :: Core.Maybe Types.EbuTtDDestinationStyleControl
    -- ^ Specifies the style information (font color, font position, and so on) to include in the font data that is attached to the EBU-TT captions.
--
--
-- - include: Take the style information (font color, font position, and so on) from the source captions and include that information in the font data attached to the EBU-TT captions. This option is valid only if the source captions are Embedded or Teletext.
-- - exclude: In the font data attached to the EBU-TT captions, set the font family to "monospaced". Do not include any other style information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbuTtDDestinationSettings' value with any optional fields omitted.
mkEbuTtDDestinationSettings
    :: EbuTtDDestinationSettings
mkEbuTtDDestinationSettings
  = EbuTtDDestinationSettings'{fillLineGap = Core.Nothing,
                               fontFamily = Core.Nothing, styleControl = Core.Nothing}

-- | Specifies how to handle the gap between the lines (in multi-line captions).
--
--
-- - enabled: Fill with the captions background color (as specified in the input captions).
-- - disabled: Leave the gap unfilled.
--
-- /Note:/ Consider using 'fillLineGap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etddsFillLineGap :: Lens.Lens' EbuTtDDestinationSettings (Core.Maybe Types.EbuTtDFillLineGapControl)
etddsFillLineGap = Lens.field @"fillLineGap"
{-# INLINEABLE etddsFillLineGap #-}
{-# DEPRECATED fillLineGap "Use generic-lens or generic-optics with 'fillLineGap' instead"  #-}

-- | Specifies the font family to include in the font data attached to the EBU-TT captions. Valid only if styleControl is set to include. If you leave this field empty, the font family is set to "monospaced". (If styleControl is set to exclude, the font family is always set to "monospaced".)
--
--
-- You specify only the font family. All other style information (color, bold, position and so on) is copied from the input captions. The size is always set to 100% to allow the downstream player to choose the size.
--
-- - Enter a list of font families, as a comma-separated list of font names, in order of preference. The name can be a font family (such as “Arial”), or a generic font family (such as “serif”), or “default” (to let the downstream player choose the font).
-- - Leave blank to set the family to “monospace”.
--
-- /Note:/ Consider using 'fontFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etddsFontFamily :: Lens.Lens' EbuTtDDestinationSettings (Core.Maybe Core.Text)
etddsFontFamily = Lens.field @"fontFamily"
{-# INLINEABLE etddsFontFamily #-}
{-# DEPRECATED fontFamily "Use generic-lens or generic-optics with 'fontFamily' instead"  #-}

-- | Specifies the style information (font color, font position, and so on) to include in the font data that is attached to the EBU-TT captions.
--
--
-- - include: Take the style information (font color, font position, and so on) from the source captions and include that information in the font data attached to the EBU-TT captions. This option is valid only if the source captions are Embedded or Teletext.
-- - exclude: In the font data attached to the EBU-TT captions, set the font family to "monospaced". Do not include any other style information.
--
-- /Note:/ Consider using 'styleControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etddsStyleControl :: Lens.Lens' EbuTtDDestinationSettings (Core.Maybe Types.EbuTtDDestinationStyleControl)
etddsStyleControl = Lens.field @"styleControl"
{-# INLINEABLE etddsStyleControl #-}
{-# DEPRECATED styleControl "Use generic-lens or generic-optics with 'styleControl' instead"  #-}

instance Core.FromJSON EbuTtDDestinationSettings where
        toJSON EbuTtDDestinationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("fillLineGap" Core..=) Core.<$> fillLineGap,
                  ("fontFamily" Core..=) Core.<$> fontFamily,
                  ("styleControl" Core..=) Core.<$> styleControl])

instance Core.FromJSON EbuTtDDestinationSettings where
        parseJSON
          = Core.withObject "EbuTtDDestinationSettings" Core.$
              \ x ->
                EbuTtDDestinationSettings' Core.<$>
                  (x Core..:? "fillLineGap") Core.<*> x Core..:? "fontFamily"
                    Core.<*> x Core..:? "styleControl"
