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
-- Module      : Amazonka.QuickSight.Types.FontConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FontConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FontDecoration
import Amazonka.QuickSight.Types.FontSize
import Amazonka.QuickSight.Types.FontStyle
import Amazonka.QuickSight.Types.FontWeight

-- | Configures the display properties of the given text.
--
-- /See:/ 'newFontConfiguration' smart constructor.
data FontConfiguration = FontConfiguration'
  { -- | Determines the color of the text.
    fontColor :: Prelude.Maybe Prelude.Text,
    -- | Determines the appearance of decorative lines on the text.
    fontDecoration :: Prelude.Maybe FontDecoration,
    -- | The option that determines the text display size.
    fontSize :: Prelude.Maybe FontSize,
    -- | Determines the text display face that is inherited by the given font
    -- family.
    fontStyle :: Prelude.Maybe FontStyle,
    -- | The option that determines the text display weight, or boldness.
    fontWeight :: Prelude.Maybe FontWeight
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FontConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fontColor', 'fontConfiguration_fontColor' - Determines the color of the text.
--
-- 'fontDecoration', 'fontConfiguration_fontDecoration' - Determines the appearance of decorative lines on the text.
--
-- 'fontSize', 'fontConfiguration_fontSize' - The option that determines the text display size.
--
-- 'fontStyle', 'fontConfiguration_fontStyle' - Determines the text display face that is inherited by the given font
-- family.
--
-- 'fontWeight', 'fontConfiguration_fontWeight' - The option that determines the text display weight, or boldness.
newFontConfiguration ::
  FontConfiguration
newFontConfiguration =
  FontConfiguration'
    { fontColor = Prelude.Nothing,
      fontDecoration = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      fontStyle = Prelude.Nothing,
      fontWeight = Prelude.Nothing
    }

-- | Determines the color of the text.
fontConfiguration_fontColor :: Lens.Lens' FontConfiguration (Prelude.Maybe Prelude.Text)
fontConfiguration_fontColor = Lens.lens (\FontConfiguration' {fontColor} -> fontColor) (\s@FontConfiguration' {} a -> s {fontColor = a} :: FontConfiguration)

-- | Determines the appearance of decorative lines on the text.
fontConfiguration_fontDecoration :: Lens.Lens' FontConfiguration (Prelude.Maybe FontDecoration)
fontConfiguration_fontDecoration = Lens.lens (\FontConfiguration' {fontDecoration} -> fontDecoration) (\s@FontConfiguration' {} a -> s {fontDecoration = a} :: FontConfiguration)

-- | The option that determines the text display size.
fontConfiguration_fontSize :: Lens.Lens' FontConfiguration (Prelude.Maybe FontSize)
fontConfiguration_fontSize = Lens.lens (\FontConfiguration' {fontSize} -> fontSize) (\s@FontConfiguration' {} a -> s {fontSize = a} :: FontConfiguration)

-- | Determines the text display face that is inherited by the given font
-- family.
fontConfiguration_fontStyle :: Lens.Lens' FontConfiguration (Prelude.Maybe FontStyle)
fontConfiguration_fontStyle = Lens.lens (\FontConfiguration' {fontStyle} -> fontStyle) (\s@FontConfiguration' {} a -> s {fontStyle = a} :: FontConfiguration)

-- | The option that determines the text display weight, or boldness.
fontConfiguration_fontWeight :: Lens.Lens' FontConfiguration (Prelude.Maybe FontWeight)
fontConfiguration_fontWeight = Lens.lens (\FontConfiguration' {fontWeight} -> fontWeight) (\s@FontConfiguration' {} a -> s {fontWeight = a} :: FontConfiguration)

instance Data.FromJSON FontConfiguration where
  parseJSON =
    Data.withObject
      "FontConfiguration"
      ( \x ->
          FontConfiguration'
            Prelude.<$> (x Data..:? "FontColor")
            Prelude.<*> (x Data..:? "FontDecoration")
            Prelude.<*> (x Data..:? "FontSize")
            Prelude.<*> (x Data..:? "FontStyle")
            Prelude.<*> (x Data..:? "FontWeight")
      )

instance Prelude.Hashable FontConfiguration where
  hashWithSalt _salt FontConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fontColor
      `Prelude.hashWithSalt` fontDecoration
      `Prelude.hashWithSalt` fontSize
      `Prelude.hashWithSalt` fontStyle
      `Prelude.hashWithSalt` fontWeight

instance Prelude.NFData FontConfiguration where
  rnf FontConfiguration' {..} =
    Prelude.rnf fontColor
      `Prelude.seq` Prelude.rnf fontDecoration
      `Prelude.seq` Prelude.rnf fontSize
      `Prelude.seq` Prelude.rnf fontStyle
      `Prelude.seq` Prelude.rnf fontWeight

instance Data.ToJSON FontConfiguration where
  toJSON FontConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FontColor" Data..=) Prelude.<$> fontColor,
            ("FontDecoration" Data..=)
              Prelude.<$> fontDecoration,
            ("FontSize" Data..=) Prelude.<$> fontSize,
            ("FontStyle" Data..=) Prelude.<$> fontStyle,
            ("FontWeight" Data..=) Prelude.<$> fontWeight
          ]
      )
