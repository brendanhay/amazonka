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
-- Module      : Amazonka.Comprehend.Types.Point
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.Point where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The X and Y coordinates of a point on a document page.
--
-- For additional information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/API_Point.html Point> in
-- the Amazon Textract API reference.
--
-- /See:/ 'newPoint' smart constructor.
data Point = Point'
  { -- | The value of the X coordinate for a point on a polygon
    x :: Prelude.Maybe Prelude.Double,
    -- | The value of the Y coordinate for a point on a polygon
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
-- 'x', 'point_x' - The value of the X coordinate for a point on a polygon
--
-- 'y', 'point_y' - The value of the Y coordinate for a point on a polygon
newPoint ::
  Point
newPoint =
  Point' {x = Prelude.Nothing, y = Prelude.Nothing}

-- | The value of the X coordinate for a point on a polygon
point_x :: Lens.Lens' Point (Prelude.Maybe Prelude.Double)
point_x = Lens.lens (\Point' {x} -> x) (\s@Point' {} a -> s {x = a} :: Point)

-- | The value of the Y coordinate for a point on a polygon
point_y :: Lens.Lens' Point (Prelude.Maybe Prelude.Double)
point_y = Lens.lens (\Point' {y} -> y) (\s@Point' {} a -> s {y = a} :: Point)

instance Data.FromJSON Point where
  parseJSON =
    Data.withObject
      "Point"
      ( \x ->
          Point'
            Prelude.<$> (x Data..:? "X")
            Prelude.<*> (x Data..:? "Y")
      )

instance Prelude.Hashable Point where
  hashWithSalt _salt Point' {..} =
    _salt
      `Prelude.hashWithSalt` x
      `Prelude.hashWithSalt` y

instance Prelude.NFData Point where
  rnf Point' {..} =
    Prelude.rnf x `Prelude.seq` Prelude.rnf y
