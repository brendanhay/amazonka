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
-- Module      : Amazonka.Comprehend.Types.BoundingBox
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BoundingBox where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The bounding box around the detected page or around an element on a
-- document page. The left (x-coordinate) and top (y-coordinate) are
-- coordinates that represent the top and left sides of the bounding box.
-- Note that the upper-left corner of the image is the origin (0,0).
--
-- For additional information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/API_BoundingBox.html BoundingBox>
-- in the Amazon Textract API reference.
--
-- /See:/ 'newBoundingBox' smart constructor.
data BoundingBox = BoundingBox'
  { -- | The height of the bounding box as a ratio of the overall document page
    -- height.
    height :: Prelude.Maybe Prelude.Double,
    -- | The left coordinate of the bounding box as a ratio of overall document
    -- page width.
    left :: Prelude.Maybe Prelude.Double,
    -- | The top coordinate of the bounding box as a ratio of overall document
    -- page height.
    top :: Prelude.Maybe Prelude.Double,
    -- | The width of the bounding box as a ratio of the overall document page
    -- width.
    width :: Prelude.Maybe Prelude.Double
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
-- 'height', 'boundingBox_height' - The height of the bounding box as a ratio of the overall document page
-- height.
--
-- 'left', 'boundingBox_left' - The left coordinate of the bounding box as a ratio of overall document
-- page width.
--
-- 'top', 'boundingBox_top' - The top coordinate of the bounding box as a ratio of overall document
-- page height.
--
-- 'width', 'boundingBox_width' - The width of the bounding box as a ratio of the overall document page
-- width.
newBoundingBox ::
  BoundingBox
newBoundingBox =
  BoundingBox'
    { height = Prelude.Nothing,
      left = Prelude.Nothing,
      top = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | The height of the bounding box as a ratio of the overall document page
-- height.
boundingBox_height :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_height = Lens.lens (\BoundingBox' {height} -> height) (\s@BoundingBox' {} a -> s {height = a} :: BoundingBox)

-- | The left coordinate of the bounding box as a ratio of overall document
-- page width.
boundingBox_left :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_left = Lens.lens (\BoundingBox' {left} -> left) (\s@BoundingBox' {} a -> s {left = a} :: BoundingBox)

-- | The top coordinate of the bounding box as a ratio of overall document
-- page height.
boundingBox_top :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_top = Lens.lens (\BoundingBox' {top} -> top) (\s@BoundingBox' {} a -> s {top = a} :: BoundingBox)

-- | The width of the bounding box as a ratio of the overall document page
-- width.
boundingBox_width :: Lens.Lens' BoundingBox (Prelude.Maybe Prelude.Double)
boundingBox_width = Lens.lens (\BoundingBox' {width} -> width) (\s@BoundingBox' {} a -> s {width = a} :: BoundingBox)

instance Data.FromJSON BoundingBox where
  parseJSON =
    Data.withObject
      "BoundingBox"
      ( \x ->
          BoundingBox'
            Prelude.<$> (x Data..:? "Height")
            Prelude.<*> (x Data..:? "Left")
            Prelude.<*> (x Data..:? "Top")
            Prelude.<*> (x Data..:? "Width")
      )

instance Prelude.Hashable BoundingBox where
  hashWithSalt _salt BoundingBox' {..} =
    _salt
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` left
      `Prelude.hashWithSalt` top
      `Prelude.hashWithSalt` width

instance Prelude.NFData BoundingBox where
  rnf BoundingBox' {..} =
    Prelude.rnf height
      `Prelude.seq` Prelude.rnf left
      `Prelude.seq` Prelude.rnf top
      `Prelude.seq` Prelude.rnf width
