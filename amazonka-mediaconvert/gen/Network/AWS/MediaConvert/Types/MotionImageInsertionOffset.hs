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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specify the offset between the upper-left corner of the video frame and
-- the top left corner of the overlay.
--
-- /See:/ 'newMotionImageInsertionOffset' smart constructor.
data MotionImageInsertionOffset = MotionImageInsertionOffset'
  { -- | Set the distance, in pixels, between the overlay and the left edge of
    -- the video frame.
    imageX :: Core.Maybe Core.Natural,
    -- | Set the distance, in pixels, between the overlay and the top edge of the
    -- video frame.
    imageY :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { imageX = Core.Nothing,
      imageY = Core.Nothing
    }

-- | Set the distance, in pixels, between the overlay and the left edge of
-- the video frame.
motionImageInsertionOffset_imageX :: Lens.Lens' MotionImageInsertionOffset (Core.Maybe Core.Natural)
motionImageInsertionOffset_imageX = Lens.lens (\MotionImageInsertionOffset' {imageX} -> imageX) (\s@MotionImageInsertionOffset' {} a -> s {imageX = a} :: MotionImageInsertionOffset)

-- | Set the distance, in pixels, between the overlay and the top edge of the
-- video frame.
motionImageInsertionOffset_imageY :: Lens.Lens' MotionImageInsertionOffset (Core.Maybe Core.Natural)
motionImageInsertionOffset_imageY = Lens.lens (\MotionImageInsertionOffset' {imageY} -> imageY) (\s@MotionImageInsertionOffset' {} a -> s {imageY = a} :: MotionImageInsertionOffset)

instance Core.FromJSON MotionImageInsertionOffset where
  parseJSON =
    Core.withObject
      "MotionImageInsertionOffset"
      ( \x ->
          MotionImageInsertionOffset'
            Core.<$> (x Core..:? "imageX") Core.<*> (x Core..:? "imageY")
      )

instance Core.Hashable MotionImageInsertionOffset

instance Core.NFData MotionImageInsertionOffset

instance Core.ToJSON MotionImageInsertionOffset where
  toJSON MotionImageInsertionOffset' {..} =
    Core.object
      ( Core.catMaybes
          [ ("imageX" Core..=) Core.<$> imageX,
            ("imageY" Core..=) Core.<$> imageY
          ]
      )
