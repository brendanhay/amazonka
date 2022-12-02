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
-- Module      : Amazonka.Rekognition.Types.BoundingBox
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.BoundingBox where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies the bounding box around the label, face, text, object of
-- interest, or personal protective equipment. The @left@ (x-coordinate)
-- and @top@ (y-coordinate) are coordinates representing the top and left
-- sides of the bounding box. Note that the upper-left corner of the image
-- is the origin (0,0).
--
-- The @top@ and @left@ values returned are ratios of the overall image
-- size. For example, if the input image is 700x200 pixels, and the
-- top-left coordinate of the bounding box is 350x50 pixels, the API
-- returns a @left@ value of 0.5 (350\/700) and a @top@ value of 0.25
-- (50\/200).
--
-- The @width@ and @height@ values represent the dimensions of the bounding
-- box as a ratio of the overall image dimension. For example, if the input
-- image is 700x200 pixels, and the bounding box width is 70 pixels, the
-- width returned is 0.1.
--
-- The bounding box coordinates can have negative values. For example, if
-- Amazon Rekognition is able to detect a face that is at the image edge
-- and is only partially visible, the service can return coordinates that
-- are outside the image bounds and, depending on the image edge, you might
-- get negative values or values greater than 1 for the @left@ or @top@
-- values.
--
-- /See:/ 'newBoundingBox' smart constructor.
data BoundingBox = BoundingBox'
  { -- | Width of the bounding box as a ratio of the overall image width.
    width :: Prelude.Maybe Prelude.Double,
    -- | Top coordinate of the bounding box as a ratio of overall image height.
    top :: Prelude.Maybe Prelude.Double,
    -- | Left coordinate of the bounding box as a ratio of overall image width.
    left :: Prelude.Maybe Prelude.Double,
    -- | Height of the bounding box as a ratio of the overall image height.
    height :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoundingBox' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'width', 'boundingBox_width' - Width of the bounding box as a ratio of the overall image width.
--
-- 'top', 'boundingBox_top' - Top coordinate of the bounding box as a ratio of overall image height.
--
-- 'left', 'boundingBox_left' - Left coordinate of the bounding box as a ratio of overall image width.
--
-- 'height', 'boundingBox_height' - Height of the bounding box as a ratio of the overall image height.
newBoundingBox ::
  BoundingBox
newBoundingBox =
  BoundingBox'
    { width = Prelude.Nothing,
      top = Prelude.Nothing,
      left = Prelude.Nothing,
      height = Prelude.Nothing
    }

-- | Width of the bounding box as a ratio of the overall image width.
boundingBox_width :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_width = Lens.lens (\BoundingBox' {width} -> width) (\s@BoundingBox' {} a -> s {width = a} :: BoundingBox)

-- | Top coordinate of the bounding box as a ratio of overall image height.
boundingBox_top :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_top = Lens.lens (\BoundingBox' {top} -> top) (\s@BoundingBox' {} a -> s {top = a} :: BoundingBox)

-- | Left coordinate of the bounding box as a ratio of overall image width.
boundingBox_left :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_left = Lens.lens (\BoundingBox' {left} -> left) (\s@BoundingBox' {} a -> s {left = a} :: BoundingBox)

-- | Height of the bounding box as a ratio of the overall image height.
boundingBox_height :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_height = Lens.lens (\BoundingBox' {height} -> height) (\s@BoundingBox' {} a -> s {height = a} :: BoundingBox)

instance Data.FromJSON BoundingBox where
  parseJSON =
    Data.withObject
      "BoundingBox"
      ( \x ->
          BoundingBox'
            Prelude.<$> (x Data..:? "Width")
            Prelude.<*> (x Data..:? "Top")
            Prelude.<*> (x Data..:? "Left")
            Prelude.<*> (x Data..:? "Height")
      )

instance Prelude.Hashable BoundingBox where
  hashWithSalt _salt BoundingBox' {..} =
    _salt `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` top
      `Prelude.hashWithSalt` left
      `Prelude.hashWithSalt` height

instance Prelude.NFData BoundingBox where
  rnf BoundingBox' {..} =
    Prelude.rnf width
      `Prelude.seq` Prelude.rnf top
      `Prelude.seq` Prelude.rnf left
      `Prelude.seq` Prelude.rnf height

instance Data.ToJSON BoundingBox where
  toJSON BoundingBox' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Width" Data..=) Prelude.<$> width,
            ("Top" Data..=) Prelude.<$> top,
            ("Left" Data..=) Prelude.<$> left,
            ("Height" Data..=) Prelude.<$> height
          ]
      )
