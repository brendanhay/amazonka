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
-- Module      : Amazonka.Textract.Types.Point
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Point where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The X and Y coordinates of a point on a document page. The X and Y
-- values that are returned are ratios of the overall document page size.
-- For example, if the input document is 700 x 200 and the operation
-- returns X=0.5 and Y=0.25, then the point is at the (350,50) pixel
-- coordinate on the document page.
--
-- An array of @Point@ objects, @Polygon@, is returned by
-- DetectDocumentText. @Polygon@ represents a fine-grained polygon around
-- detected text. For more information, see Geometry in the Amazon Textract
-- Developer Guide.
--
-- /See:/ 'newPoint' smart constructor.
data Point = Point'
  { -- | The value of the X coordinate for a point on a @Polygon@.
    x :: Prelude.Maybe Prelude.Double,
    -- | The value of the Y coordinate for a point on a @Polygon@.
    y :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Point' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'x', 'point_x' - The value of the X coordinate for a point on a @Polygon@.
--
-- 'y', 'point_y' - The value of the Y coordinate for a point on a @Polygon@.
newPoint ::
  Point
newPoint =
  Point' {x = Prelude.Nothing, y = Prelude.Nothing}

-- | The value of the X coordinate for a point on a @Polygon@.
point_x :: Lens.Lens' Point (Prelude.Maybe Prelude.Double)
point_x = Lens.lens (\Point' {x} -> x) (\s@Point' {} a -> s {x = a} :: Point)

-- | The value of the Y coordinate for a point on a @Polygon@.
point_y :: Lens.Lens' Point (Prelude.Maybe Prelude.Double)
point_y = Lens.lens (\Point' {y} -> y) (\s@Point' {} a -> s {y = a} :: Point)

instance Data.FromJSON Point where
  parseJSON =
    Data.withObject
      "Point"
      ( \x ->
          Point'
            Prelude.<$> (x Data..:? "X") Prelude.<*> (x Data..:? "Y")
      )

instance Prelude.Hashable Point where
  hashWithSalt _salt Point' {..} =
    _salt `Prelude.hashWithSalt` x
      `Prelude.hashWithSalt` y

instance Prelude.NFData Point where
  rnf Point' {..} =
    Prelude.rnf x `Prelude.seq` Prelude.rnf y
