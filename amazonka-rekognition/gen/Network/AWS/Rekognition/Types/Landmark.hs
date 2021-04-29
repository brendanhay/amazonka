{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.Types.Landmark
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Landmark where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.LandmarkType

-- | Indicates the location of the landmark on the face.
--
-- /See:/ 'newLandmark' smart constructor.
data Landmark = Landmark'
  { -- | The y-coordinate of the landmark expressed as a ratio of the height of
    -- the image. The y-coordinate is measured from the top of the image. For
    -- example, if the image height is 200 pixels and the y-coordinate of the
    -- landmark is at 50 pixels, this value is 0.25.
    y :: Prelude.Maybe Prelude.Double,
    -- | The x-coordinate of the landmark expressed as a ratio of the width of
    -- the image. The x-coordinate is measured from the left-side of the image.
    -- For example, if the image is 700 pixels wide and the x-coordinate of the
    -- landmark is at 350 pixels, this value is 0.5.
    x :: Prelude.Maybe Prelude.Double,
    -- | Type of landmark.
    type' :: Prelude.Maybe LandmarkType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Landmark' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'y', 'landmark_y' - The y-coordinate of the landmark expressed as a ratio of the height of
-- the image. The y-coordinate is measured from the top of the image. For
-- example, if the image height is 200 pixels and the y-coordinate of the
-- landmark is at 50 pixels, this value is 0.25.
--
-- 'x', 'landmark_x' - The x-coordinate of the landmark expressed as a ratio of the width of
-- the image. The x-coordinate is measured from the left-side of the image.
-- For example, if the image is 700 pixels wide and the x-coordinate of the
-- landmark is at 350 pixels, this value is 0.5.
--
-- 'type'', 'landmark_type' - Type of landmark.
newLandmark ::
  Landmark
newLandmark =
  Landmark'
    { y = Prelude.Nothing,
      x = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The y-coordinate of the landmark expressed as a ratio of the height of
-- the image. The y-coordinate is measured from the top of the image. For
-- example, if the image height is 200 pixels and the y-coordinate of the
-- landmark is at 50 pixels, this value is 0.25.
landmark_y :: Lens.Lens' Landmark (Prelude.Maybe Prelude.Double)
landmark_y = Lens.lens (\Landmark' {y} -> y) (\s@Landmark' {} a -> s {y = a} :: Landmark)

-- | The x-coordinate of the landmark expressed as a ratio of the width of
-- the image. The x-coordinate is measured from the left-side of the image.
-- For example, if the image is 700 pixels wide and the x-coordinate of the
-- landmark is at 350 pixels, this value is 0.5.
landmark_x :: Lens.Lens' Landmark (Prelude.Maybe Prelude.Double)
landmark_x = Lens.lens (\Landmark' {x} -> x) (\s@Landmark' {} a -> s {x = a} :: Landmark)

-- | Type of landmark.
landmark_type :: Lens.Lens' Landmark (Prelude.Maybe LandmarkType)
landmark_type = Lens.lens (\Landmark' {type'} -> type') (\s@Landmark' {} a -> s {type' = a} :: Landmark)

instance Prelude.FromJSON Landmark where
  parseJSON =
    Prelude.withObject
      "Landmark"
      ( \x ->
          Landmark'
            Prelude.<$> (x Prelude..:? "Y")
            Prelude.<*> (x Prelude..:? "X")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Landmark

instance Prelude.NFData Landmark
