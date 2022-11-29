{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.EbuTtDDestinationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.EbuTtDDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.EbuTtDDestinationStyleControl
import Amazonka.MediaLive.Types.EbuTtDFillLineGapControl
import qualified Amazonka.Prelude as Prelude

-- | Ebu Tt DDestination Settings
--
-- /See:/ 'newEbuTtDDestinationSettings' smart constructor.
data EbuTtDDestinationSettings = EbuTtDDestinationSettings'
  { -- | Specifies how to handle the gap between the lines (in multi-line
    -- captions). - enabled: Fill with the captions background color (as
    -- specified in the input captions). - disabled: Leave the gap unfilled.
    fillLineGap :: Prelude.Maybe EbuTtDFillLineGapControl,
    -- | Specifies the style information (font color, font position, and so on)
    -- to include in the font data that is attached to the EBU-TT captions. -
    -- include: Take the style information (font color, font position, and so
    -- on) from the source captions and include that information in the font
    -- data attached to the EBU-TT captions. This option is valid only if the
    -- source captions are Embedded or Teletext. - exclude: In the font data
    -- attached to the EBU-TT captions, set the font family to \"monospaced\".
    -- Do not include any other style information.
    styleControl :: Prelude.Maybe EbuTtDDestinationStyleControl,
    -- | Specifies the font family to include in the font data attached to the
    -- EBU-TT captions. Valid only if styleControl is set to include. If you
    -- leave this field empty, the font family is set to \"monospaced\". (If
    -- styleControl is set to exclude, the font family is always set to
    -- \"monospaced\".) You specify only the font family. All other style
    -- information (color, bold, position and so on) is copied from the input
    -- captions. The size is always set to 100% to allow the downstream player
    -- to choose the size. - Enter a list of font families, as a
    -- comma-separated list of font names, in order of preference. The name can
    -- be a font family (such as “Arial”), or a generic font family (such as
    -- “serif”), or “default” (to let the downstream player choose the font). -
    -- Leave blank to set the family to “monospace”.
    fontFamily :: Prelude.Maybe Prelude.Text,
    -- | Complete this field if you want to include the name of the copyright
    -- holder in the copyright tag in the captions metadata.
    copyrightHolder :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbuTtDDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fillLineGap', 'ebuTtDDestinationSettings_fillLineGap' - Specifies how to handle the gap between the lines (in multi-line
-- captions). - enabled: Fill with the captions background color (as
-- specified in the input captions). - disabled: Leave the gap unfilled.
--
-- 'styleControl', 'ebuTtDDestinationSettings_styleControl' - Specifies the style information (font color, font position, and so on)
-- to include in the font data that is attached to the EBU-TT captions. -
-- include: Take the style information (font color, font position, and so
-- on) from the source captions and include that information in the font
-- data attached to the EBU-TT captions. This option is valid only if the
-- source captions are Embedded or Teletext. - exclude: In the font data
-- attached to the EBU-TT captions, set the font family to \"monospaced\".
-- Do not include any other style information.
--
-- 'fontFamily', 'ebuTtDDestinationSettings_fontFamily' - Specifies the font family to include in the font data attached to the
-- EBU-TT captions. Valid only if styleControl is set to include. If you
-- leave this field empty, the font family is set to \"monospaced\". (If
-- styleControl is set to exclude, the font family is always set to
-- \"monospaced\".) You specify only the font family. All other style
-- information (color, bold, position and so on) is copied from the input
-- captions. The size is always set to 100% to allow the downstream player
-- to choose the size. - Enter a list of font families, as a
-- comma-separated list of font names, in order of preference. The name can
-- be a font family (such as “Arial”), or a generic font family (such as
-- “serif”), or “default” (to let the downstream player choose the font). -
-- Leave blank to set the family to “monospace”.
--
-- 'copyrightHolder', 'ebuTtDDestinationSettings_copyrightHolder' - Complete this field if you want to include the name of the copyright
-- holder in the copyright tag in the captions metadata.
newEbuTtDDestinationSettings ::
  EbuTtDDestinationSettings
newEbuTtDDestinationSettings =
  EbuTtDDestinationSettings'
    { fillLineGap =
        Prelude.Nothing,
      styleControl = Prelude.Nothing,
      fontFamily = Prelude.Nothing,
      copyrightHolder = Prelude.Nothing
    }

-- | Specifies how to handle the gap between the lines (in multi-line
-- captions). - enabled: Fill with the captions background color (as
-- specified in the input captions). - disabled: Leave the gap unfilled.
ebuTtDDestinationSettings_fillLineGap :: Lens.Lens' EbuTtDDestinationSettings (Prelude.Maybe EbuTtDFillLineGapControl)
ebuTtDDestinationSettings_fillLineGap = Lens.lens (\EbuTtDDestinationSettings' {fillLineGap} -> fillLineGap) (\s@EbuTtDDestinationSettings' {} a -> s {fillLineGap = a} :: EbuTtDDestinationSettings)

-- | Specifies the style information (font color, font position, and so on)
-- to include in the font data that is attached to the EBU-TT captions. -
-- include: Take the style information (font color, font position, and so
-- on) from the source captions and include that information in the font
-- data attached to the EBU-TT captions. This option is valid only if the
-- source captions are Embedded or Teletext. - exclude: In the font data
-- attached to the EBU-TT captions, set the font family to \"monospaced\".
-- Do not include any other style information.
ebuTtDDestinationSettings_styleControl :: Lens.Lens' EbuTtDDestinationSettings (Prelude.Maybe EbuTtDDestinationStyleControl)
ebuTtDDestinationSettings_styleControl = Lens.lens (\EbuTtDDestinationSettings' {styleControl} -> styleControl) (\s@EbuTtDDestinationSettings' {} a -> s {styleControl = a} :: EbuTtDDestinationSettings)

-- | Specifies the font family to include in the font data attached to the
-- EBU-TT captions. Valid only if styleControl is set to include. If you
-- leave this field empty, the font family is set to \"monospaced\". (If
-- styleControl is set to exclude, the font family is always set to
-- \"monospaced\".) You specify only the font family. All other style
-- information (color, bold, position and so on) is copied from the input
-- captions. The size is always set to 100% to allow the downstream player
-- to choose the size. - Enter a list of font families, as a
-- comma-separated list of font names, in order of preference. The name can
-- be a font family (such as “Arial”), or a generic font family (such as
-- “serif”), or “default” (to let the downstream player choose the font). -
-- Leave blank to set the family to “monospace”.
ebuTtDDestinationSettings_fontFamily :: Lens.Lens' EbuTtDDestinationSettings (Prelude.Maybe Prelude.Text)
ebuTtDDestinationSettings_fontFamily = Lens.lens (\EbuTtDDestinationSettings' {fontFamily} -> fontFamily) (\s@EbuTtDDestinationSettings' {} a -> s {fontFamily = a} :: EbuTtDDestinationSettings)

-- | Complete this field if you want to include the name of the copyright
-- holder in the copyright tag in the captions metadata.
ebuTtDDestinationSettings_copyrightHolder :: Lens.Lens' EbuTtDDestinationSettings (Prelude.Maybe Prelude.Text)
ebuTtDDestinationSettings_copyrightHolder = Lens.lens (\EbuTtDDestinationSettings' {copyrightHolder} -> copyrightHolder) (\s@EbuTtDDestinationSettings' {} a -> s {copyrightHolder = a} :: EbuTtDDestinationSettings)

instance Core.FromJSON EbuTtDDestinationSettings where
  parseJSON =
    Core.withObject
      "EbuTtDDestinationSettings"
      ( \x ->
          EbuTtDDestinationSettings'
            Prelude.<$> (x Core..:? "fillLineGap")
            Prelude.<*> (x Core..:? "styleControl")
            Prelude.<*> (x Core..:? "fontFamily")
            Prelude.<*> (x Core..:? "copyrightHolder")
      )

instance Prelude.Hashable EbuTtDDestinationSettings where
  hashWithSalt _salt EbuTtDDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` fillLineGap
      `Prelude.hashWithSalt` styleControl
      `Prelude.hashWithSalt` fontFamily
      `Prelude.hashWithSalt` copyrightHolder

instance Prelude.NFData EbuTtDDestinationSettings where
  rnf EbuTtDDestinationSettings' {..} =
    Prelude.rnf fillLineGap
      `Prelude.seq` Prelude.rnf styleControl
      `Prelude.seq` Prelude.rnf fontFamily
      `Prelude.seq` Prelude.rnf copyrightHolder

instance Core.ToJSON EbuTtDDestinationSettings where
  toJSON EbuTtDDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fillLineGap" Core..=) Prelude.<$> fillLineGap,
            ("styleControl" Core..=) Prelude.<$> styleControl,
            ("fontFamily" Core..=) Prelude.<$> fontFamily,
            ("copyrightHolder" Core..=)
              Prelude.<$> copyrightHolder
          ]
      )
