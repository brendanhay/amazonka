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
-- Module      : Amazonka.Rekognition.Types.Landmark
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Landmark where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.LandmarkType

-- | Indicates the location of the landmark on the face.
--
-- /See:/ 'newLandmark' smart constructor.
data Landmark = Landmark'
  { -- | Type of landmark.
    type' :: Prelude.Maybe LandmarkType,
    -- | The x-coordinate of the landmark expressed as a ratio of the width of
    -- the image. The x-coordinate is measured from the left-side of the image.
    -- For example, if the image is 700 pixels wide and the x-coordinate of the
    -- landmark is at 350 pixels, this value is 0.5.
    x :: Prelude.Maybe Prelude.Double,
    -- | The y-coordinate of the landmark expressed as a ratio of the height of
    -- the image. The y-coordinate is measured from the top of the image. For
    -- example, if the image height is 200 pixels and the y-coordinate of the
    -- landmark is at 50 pixels, this value is 0.25.
    y :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Landmark' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'landmark_type' - Type of landmark.
--
-- 'x', 'landmark_x' - The x-coordinate of the landmark expressed as a ratio of the width of
-- the image. The x-coordinate is measured from the left-side of the image.
-- For example, if the image is 700 pixels wide and the x-coordinate of the
-- landmark is at 350 pixels, this value is 0.5.
--
-- 'y', 'landmark_y' - The y-coordinate of the landmark expressed as a ratio of the height of
-- the image. The y-coordinate is measured from the top of the image. For
-- example, if the image height is 200 pixels and the y-coordinate of the
-- landmark is at 50 pixels, this value is 0.25.
newLandmark ::
  Landmark
newLandmark =
  Landmark'
    { type' = Prelude.Nothing,
      x = Prelude.Nothing,
      y = Prelude.Nothing
    }

-- | Type of landmark.
landmark_type :: Lens.Lens' Landmark (Prelude.Maybe LandmarkType)
landmark_type = Lens.lens (\Landmark' {type'} -> type') (\s@Landmark' {} a -> s {type' = a} :: Landmark)

-- | The x-coordinate of the landmark expressed as a ratio of the width of
-- the image. The x-coordinate is measured from the left-side of the image.
-- For example, if the image is 700 pixels wide and the x-coordinate of the
-- landmark is at 350 pixels, this value is 0.5.
landmark_x :: Lens.Lens' Landmark (Prelude.Maybe Prelude.Double)
landmark_x = Lens.lens (\Landmark' {x} -> x) (\s@Landmark' {} a -> s {x = a} :: Landmark)

-- | The y-coordinate of the landmark expressed as a ratio of the height of
-- the image. The y-coordinate is measured from the top of the image. For
-- example, if the image height is 200 pixels and the y-coordinate of the
-- landmark is at 50 pixels, this value is 0.25.
landmark_y :: Lens.Lens' Landmark (Prelude.Maybe Prelude.Double)
landmark_y = Lens.lens (\Landmark' {y} -> y) (\s@Landmark' {} a -> s {y = a} :: Landmark)

instance Data.FromJSON Landmark where
  parseJSON =
    Data.withObject
      "Landmark"
      ( \x ->
          Landmark'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "X")
            Prelude.<*> (x Data..:? "Y")
      )

instance Prelude.Hashable Landmark where
  hashWithSalt _salt Landmark' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` x
      `Prelude.hashWithSalt` y

instance Prelude.NFData Landmark where
  rnf Landmark' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf x
      `Prelude.seq` Prelude.rnf y
