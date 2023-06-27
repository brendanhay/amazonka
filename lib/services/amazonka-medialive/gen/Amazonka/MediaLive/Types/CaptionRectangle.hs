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
-- Module      : Amazonka.MediaLive.Types.CaptionRectangle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CaptionRectangle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Caption Rectangle
--
-- /See:/ 'newCaptionRectangle' smart constructor.
data CaptionRectangle = CaptionRectangle'
  { -- | See the description in leftOffset. For topOffset, specify the position
    -- of the top edge of the rectangle, as a percentage of the underlying
    -- frame height, and relative to the top edge of the frame. For example,
    -- \\\"10\\\" means the measurement is 10% of the underlying frame height.
    -- The rectangle top edge starts at that position from the top edge of the
    -- frame. This field corresponds to tts:origin - Y in the TTML standard.
    topOffset :: Prelude.Double,
    -- | See the description in leftOffset. For height, specify the entire height
    -- of the rectangle as a percentage of the underlying frame height. For
    -- example, \\\"80\\\" means the rectangle height is 80% of the underlying
    -- frame height. The topOffset and rectangleHeight must add up to 100% or
    -- less. This field corresponds to tts:extent - Y in the TTML standard.
    height :: Prelude.Double,
    -- | See the description in leftOffset. For width, specify the entire width
    -- of the rectangle as a percentage of the underlying frame width. For
    -- example, \\\"80\\\" means the rectangle width is 80% of the underlying
    -- frame width. The leftOffset and rectangleWidth must add up to 100% or
    -- less. This field corresponds to tts:extent - X in the TTML standard.
    width :: Prelude.Double,
    -- | Applies only if you plan to convert these source captions to EBU-TT-D or
    -- TTML in an output. (Make sure to leave the default if you don\'t have
    -- either of these formats in the output.) You can define a display
    -- rectangle for the captions that is smaller than the underlying video
    -- frame. You define the rectangle by specifying the position of the left
    -- edge, top edge, bottom edge, and right edge of the rectangle, all within
    -- the underlying video frame. The units for the measurements are
    -- percentages. If you specify a value for one of these fields, you must
    -- specify a value for all of them. For leftOffset, specify the position of
    -- the left edge of the rectangle, as a percentage of the underlying frame
    -- width, and relative to the left edge of the frame. For example,
    -- \\\"10\\\" means the measurement is 10% of the underlying frame width.
    -- The rectangle left edge starts at that position from the left edge of
    -- the frame. This field corresponds to tts:origin - X in the TTML
    -- standard.
    leftOffset :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionRectangle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topOffset', 'captionRectangle_topOffset' - See the description in leftOffset. For topOffset, specify the position
-- of the top edge of the rectangle, as a percentage of the underlying
-- frame height, and relative to the top edge of the frame. For example,
-- \\\"10\\\" means the measurement is 10% of the underlying frame height.
-- The rectangle top edge starts at that position from the top edge of the
-- frame. This field corresponds to tts:origin - Y in the TTML standard.
--
-- 'height', 'captionRectangle_height' - See the description in leftOffset. For height, specify the entire height
-- of the rectangle as a percentage of the underlying frame height. For
-- example, \\\"80\\\" means the rectangle height is 80% of the underlying
-- frame height. The topOffset and rectangleHeight must add up to 100% or
-- less. This field corresponds to tts:extent - Y in the TTML standard.
--
-- 'width', 'captionRectangle_width' - See the description in leftOffset. For width, specify the entire width
-- of the rectangle as a percentage of the underlying frame width. For
-- example, \\\"80\\\" means the rectangle width is 80% of the underlying
-- frame width. The leftOffset and rectangleWidth must add up to 100% or
-- less. This field corresponds to tts:extent - X in the TTML standard.
--
-- 'leftOffset', 'captionRectangle_leftOffset' - Applies only if you plan to convert these source captions to EBU-TT-D or
-- TTML in an output. (Make sure to leave the default if you don\'t have
-- either of these formats in the output.) You can define a display
-- rectangle for the captions that is smaller than the underlying video
-- frame. You define the rectangle by specifying the position of the left
-- edge, top edge, bottom edge, and right edge of the rectangle, all within
-- the underlying video frame. The units for the measurements are
-- percentages. If you specify a value for one of these fields, you must
-- specify a value for all of them. For leftOffset, specify the position of
-- the left edge of the rectangle, as a percentage of the underlying frame
-- width, and relative to the left edge of the frame. For example,
-- \\\"10\\\" means the measurement is 10% of the underlying frame width.
-- The rectangle left edge starts at that position from the left edge of
-- the frame. This field corresponds to tts:origin - X in the TTML
-- standard.
newCaptionRectangle ::
  -- | 'topOffset'
  Prelude.Double ->
  -- | 'height'
  Prelude.Double ->
  -- | 'width'
  Prelude.Double ->
  -- | 'leftOffset'
  Prelude.Double ->
  CaptionRectangle
newCaptionRectangle
  pTopOffset_
  pHeight_
  pWidth_
  pLeftOffset_ =
    CaptionRectangle'
      { topOffset = pTopOffset_,
        height = pHeight_,
        width = pWidth_,
        leftOffset = pLeftOffset_
      }

-- | See the description in leftOffset. For topOffset, specify the position
-- of the top edge of the rectangle, as a percentage of the underlying
-- frame height, and relative to the top edge of the frame. For example,
-- \\\"10\\\" means the measurement is 10% of the underlying frame height.
-- The rectangle top edge starts at that position from the top edge of the
-- frame. This field corresponds to tts:origin - Y in the TTML standard.
captionRectangle_topOffset :: Lens.Lens' CaptionRectangle Prelude.Double
captionRectangle_topOffset = Lens.lens (\CaptionRectangle' {topOffset} -> topOffset) (\s@CaptionRectangle' {} a -> s {topOffset = a} :: CaptionRectangle)

-- | See the description in leftOffset. For height, specify the entire height
-- of the rectangle as a percentage of the underlying frame height. For
-- example, \\\"80\\\" means the rectangle height is 80% of the underlying
-- frame height. The topOffset and rectangleHeight must add up to 100% or
-- less. This field corresponds to tts:extent - Y in the TTML standard.
captionRectangle_height :: Lens.Lens' CaptionRectangle Prelude.Double
captionRectangle_height = Lens.lens (\CaptionRectangle' {height} -> height) (\s@CaptionRectangle' {} a -> s {height = a} :: CaptionRectangle)

-- | See the description in leftOffset. For width, specify the entire width
-- of the rectangle as a percentage of the underlying frame width. For
-- example, \\\"80\\\" means the rectangle width is 80% of the underlying
-- frame width. The leftOffset and rectangleWidth must add up to 100% or
-- less. This field corresponds to tts:extent - X in the TTML standard.
captionRectangle_width :: Lens.Lens' CaptionRectangle Prelude.Double
captionRectangle_width = Lens.lens (\CaptionRectangle' {width} -> width) (\s@CaptionRectangle' {} a -> s {width = a} :: CaptionRectangle)

-- | Applies only if you plan to convert these source captions to EBU-TT-D or
-- TTML in an output. (Make sure to leave the default if you don\'t have
-- either of these formats in the output.) You can define a display
-- rectangle for the captions that is smaller than the underlying video
-- frame. You define the rectangle by specifying the position of the left
-- edge, top edge, bottom edge, and right edge of the rectangle, all within
-- the underlying video frame. The units for the measurements are
-- percentages. If you specify a value for one of these fields, you must
-- specify a value for all of them. For leftOffset, specify the position of
-- the left edge of the rectangle, as a percentage of the underlying frame
-- width, and relative to the left edge of the frame. For example,
-- \\\"10\\\" means the measurement is 10% of the underlying frame width.
-- The rectangle left edge starts at that position from the left edge of
-- the frame. This field corresponds to tts:origin - X in the TTML
-- standard.
captionRectangle_leftOffset :: Lens.Lens' CaptionRectangle Prelude.Double
captionRectangle_leftOffset = Lens.lens (\CaptionRectangle' {leftOffset} -> leftOffset) (\s@CaptionRectangle' {} a -> s {leftOffset = a} :: CaptionRectangle)

instance Data.FromJSON CaptionRectangle where
  parseJSON =
    Data.withObject
      "CaptionRectangle"
      ( \x ->
          CaptionRectangle'
            Prelude.<$> (x Data..: "topOffset")
            Prelude.<*> (x Data..: "height")
            Prelude.<*> (x Data..: "width")
            Prelude.<*> (x Data..: "leftOffset")
      )

instance Prelude.Hashable CaptionRectangle where
  hashWithSalt _salt CaptionRectangle' {..} =
    _salt
      `Prelude.hashWithSalt` topOffset
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` leftOffset

instance Prelude.NFData CaptionRectangle where
  rnf CaptionRectangle' {..} =
    Prelude.rnf topOffset
      `Prelude.seq` Prelude.rnf height
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf leftOffset

instance Data.ToJSON CaptionRectangle where
  toJSON CaptionRectangle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("topOffset" Data..= topOffset),
            Prelude.Just ("height" Data..= height),
            Prelude.Just ("width" Data..= width),
            Prelude.Just ("leftOffset" Data..= leftOffset)
          ]
      )
