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
-- Module      : Amazonka.MediaConvert.Types.MotionImageInsertionOffset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MotionImageInsertionOffset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON MotionImageInsertionOffset where
  parseJSON =
    Data.withObject
      "MotionImageInsertionOffset"
      ( \x ->
          MotionImageInsertionOffset'
            Prelude.<$> (x Data..:? "imageX")
            Prelude.<*> (x Data..:? "imageY")
      )

instance Prelude.Hashable MotionImageInsertionOffset where
  hashWithSalt _salt MotionImageInsertionOffset' {..} =
    _salt
      `Prelude.hashWithSalt` imageX
      `Prelude.hashWithSalt` imageY

instance Prelude.NFData MotionImageInsertionOffset where
  rnf MotionImageInsertionOffset' {..} =
    Prelude.rnf imageX `Prelude.seq` Prelude.rnf imageY

instance Data.ToJSON MotionImageInsertionOffset where
  toJSON MotionImageInsertionOffset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("imageX" Data..=) Prelude.<$> imageX,
            ("imageY" Data..=) Prelude.<$> imageY
          ]
      )
