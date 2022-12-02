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
-- Module      : Amazonka.Rekognition.Types.DominantColor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DominantColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A description of the dominant colors in an image.
--
-- /See:/ 'newDominantColor' smart constructor.
data DominantColor = DominantColor'
  { -- | One of 12 simplified color names applied to a dominant color.
    simplifiedColor :: Prelude.Maybe Prelude.Text,
    -- | The Hex code equivalent of the RGB values for a dominant color.
    hexCode :: Prelude.Maybe Prelude.Text,
    -- | The percentage of image pixels that have a given dominant color.
    pixelPercent :: Prelude.Maybe Prelude.Double,
    -- | The Green RGB value for a dominant color.
    green :: Prelude.Maybe Prelude.Natural,
    -- | The CSS color name of a dominant color.
    cSSColor :: Prelude.Maybe Prelude.Text,
    -- | The Blue RGB value for a dominant color.
    blue :: Prelude.Maybe Prelude.Natural,
    -- | The Red RGB value for a dominant color.
    red :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DominantColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simplifiedColor', 'dominantColor_simplifiedColor' - One of 12 simplified color names applied to a dominant color.
--
-- 'hexCode', 'dominantColor_hexCode' - The Hex code equivalent of the RGB values for a dominant color.
--
-- 'pixelPercent', 'dominantColor_pixelPercent' - The percentage of image pixels that have a given dominant color.
--
-- 'green', 'dominantColor_green' - The Green RGB value for a dominant color.
--
-- 'cSSColor', 'dominantColor_cSSColor' - The CSS color name of a dominant color.
--
-- 'blue', 'dominantColor_blue' - The Blue RGB value for a dominant color.
--
-- 'red', 'dominantColor_red' - The Red RGB value for a dominant color.
newDominantColor ::
  DominantColor
newDominantColor =
  DominantColor'
    { simplifiedColor = Prelude.Nothing,
      hexCode = Prelude.Nothing,
      pixelPercent = Prelude.Nothing,
      green = Prelude.Nothing,
      cSSColor = Prelude.Nothing,
      blue = Prelude.Nothing,
      red = Prelude.Nothing
    }

-- | One of 12 simplified color names applied to a dominant color.
dominantColor_simplifiedColor :: Lens.Lens' DominantColor (Prelude.Maybe Prelude.Text)
dominantColor_simplifiedColor = Lens.lens (\DominantColor' {simplifiedColor} -> simplifiedColor) (\s@DominantColor' {} a -> s {simplifiedColor = a} :: DominantColor)

-- | The Hex code equivalent of the RGB values for a dominant color.
dominantColor_hexCode :: Lens.Lens' DominantColor (Prelude.Maybe Prelude.Text)
dominantColor_hexCode = Lens.lens (\DominantColor' {hexCode} -> hexCode) (\s@DominantColor' {} a -> s {hexCode = a} :: DominantColor)

-- | The percentage of image pixels that have a given dominant color.
dominantColor_pixelPercent :: Lens.Lens' DominantColor (Prelude.Maybe Prelude.Double)
dominantColor_pixelPercent = Lens.lens (\DominantColor' {pixelPercent} -> pixelPercent) (\s@DominantColor' {} a -> s {pixelPercent = a} :: DominantColor)

-- | The Green RGB value for a dominant color.
dominantColor_green :: Lens.Lens' DominantColor (Prelude.Maybe Prelude.Natural)
dominantColor_green = Lens.lens (\DominantColor' {green} -> green) (\s@DominantColor' {} a -> s {green = a} :: DominantColor)

-- | The CSS color name of a dominant color.
dominantColor_cSSColor :: Lens.Lens' DominantColor (Prelude.Maybe Prelude.Text)
dominantColor_cSSColor = Lens.lens (\DominantColor' {cSSColor} -> cSSColor) (\s@DominantColor' {} a -> s {cSSColor = a} :: DominantColor)

-- | The Blue RGB value for a dominant color.
dominantColor_blue :: Lens.Lens' DominantColor (Prelude.Maybe Prelude.Natural)
dominantColor_blue = Lens.lens (\DominantColor' {blue} -> blue) (\s@DominantColor' {} a -> s {blue = a} :: DominantColor)

-- | The Red RGB value for a dominant color.
dominantColor_red :: Lens.Lens' DominantColor (Prelude.Maybe Prelude.Natural)
dominantColor_red = Lens.lens (\DominantColor' {red} -> red) (\s@DominantColor' {} a -> s {red = a} :: DominantColor)

instance Data.FromJSON DominantColor where
  parseJSON =
    Data.withObject
      "DominantColor"
      ( \x ->
          DominantColor'
            Prelude.<$> (x Data..:? "SimplifiedColor")
            Prelude.<*> (x Data..:? "HexCode")
            Prelude.<*> (x Data..:? "PixelPercent")
            Prelude.<*> (x Data..:? "Green")
            Prelude.<*> (x Data..:? "CSSColor")
            Prelude.<*> (x Data..:? "Blue")
            Prelude.<*> (x Data..:? "Red")
      )

instance Prelude.Hashable DominantColor where
  hashWithSalt _salt DominantColor' {..} =
    _salt `Prelude.hashWithSalt` simplifiedColor
      `Prelude.hashWithSalt` hexCode
      `Prelude.hashWithSalt` pixelPercent
      `Prelude.hashWithSalt` green
      `Prelude.hashWithSalt` cSSColor
      `Prelude.hashWithSalt` blue
      `Prelude.hashWithSalt` red

instance Prelude.NFData DominantColor where
  rnf DominantColor' {..} =
    Prelude.rnf simplifiedColor
      `Prelude.seq` Prelude.rnf hexCode
      `Prelude.seq` Prelude.rnf pixelPercent
      `Prelude.seq` Prelude.rnf green
      `Prelude.seq` Prelude.rnf cSSColor
      `Prelude.seq` Prelude.rnf blue
      `Prelude.seq` Prelude.rnf red
