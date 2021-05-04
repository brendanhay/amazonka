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
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionOffset where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specify the offset between the upper-left corner of the video frame and
-- the top left corner of the overlay.
--
-- /See:/ 'newMotionImageInsertionOffset' smart constructor.
data MotionImageInsertionOffset = MotionImageInsertionOffset'
  { -- | Set the distance, in pixels, between the overlay and the left edge of
    -- the video frame.
    imageX :: Prelude.Maybe Prelude.Natural,
    -- | Set the distance, in pixels, between the overlay and the top edge of the
    -- video frame.
    imageY :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MotionImageInsertionOffset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageX', 'motionImageInsertionOffset_imageX' - Set the distance, in pixels, between the overlay and the left edge of
-- the video frame.
--
-- 'imageY', 'motionImageInsertionOffset_imageY' - Set the distance, in pixels, between the overlay and the top edge of the
-- video frame.
newMotionImageInsertionOffset ::
  MotionImageInsertionOffset
newMotionImageInsertionOffset =
  MotionImageInsertionOffset'
    { imageX =
        Prelude.Nothing,
      imageY = Prelude.Nothing
    }

-- | Set the distance, in pixels, between the overlay and the left edge of
-- the video frame.
motionImageInsertionOffset_imageX :: Lens.Lens' MotionImageInsertionOffset (Prelude.Maybe Prelude.Natural)
motionImageInsertionOffset_imageX = Lens.lens (\MotionImageInsertionOffset' {imageX} -> imageX) (\s@MotionImageInsertionOffset' {} a -> s {imageX = a} :: MotionImageInsertionOffset)

-- | Set the distance, in pixels, between the overlay and the top edge of the
-- video frame.
motionImageInsertionOffset_imageY :: Lens.Lens' MotionImageInsertionOffset (Prelude.Maybe Prelude.Natural)
motionImageInsertionOffset_imageY = Lens.lens (\MotionImageInsertionOffset' {imageY} -> imageY) (\s@MotionImageInsertionOffset' {} a -> s {imageY = a} :: MotionImageInsertionOffset)

instance Prelude.FromJSON MotionImageInsertionOffset where
  parseJSON =
    Prelude.withObject
      "MotionImageInsertionOffset"
      ( \x ->
          MotionImageInsertionOffset'
            Prelude.<$> (x Prelude..:? "imageX")
            Prelude.<*> (x Prelude..:? "imageY")
      )

instance Prelude.Hashable MotionImageInsertionOffset

instance Prelude.NFData MotionImageInsertionOffset

instance Prelude.ToJSON MotionImageInsertionOffset where
  toJSON MotionImageInsertionOffset' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("imageX" Prelude..=) Prelude.<$> imageX,
            ("imageY" Prelude..=) Prelude.<$> imageY
          ]
      )
