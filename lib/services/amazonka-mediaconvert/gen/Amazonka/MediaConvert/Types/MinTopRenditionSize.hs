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
-- Module      : Amazonka.MediaConvert.Types.MinTopRenditionSize
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MinTopRenditionSize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use Min top rendition size to specify a minimum size for the highest
-- resolution in your ABR stack. * The highest resolution in your ABR stack
-- will be equal to or greater than the value that you enter. For example:
-- If you specify 1280x720 the highest resolution in your ABR stack will be
-- equal to or greater than 1280x720. * If you specify a value for Max
-- resolution, the value that you specify for Min top rendition size must
-- be less than, or equal to, Max resolution.
--
-- /See:/ 'newMinTopRenditionSize' smart constructor.
data MinTopRenditionSize = MinTopRenditionSize'
  { -- | Use Width to define the video resolution width, in pixels, for this
    -- rule.
    width :: Prelude.Maybe Prelude.Natural,
    -- | Use Height to define the video resolution height, in pixels, for this
    -- rule.
    height :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MinTopRenditionSize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'width', 'minTopRenditionSize_width' - Use Width to define the video resolution width, in pixels, for this
-- rule.
--
-- 'height', 'minTopRenditionSize_height' - Use Height to define the video resolution height, in pixels, for this
-- rule.
newMinTopRenditionSize ::
  MinTopRenditionSize
newMinTopRenditionSize =
  MinTopRenditionSize'
    { width = Prelude.Nothing,
      height = Prelude.Nothing
    }

-- | Use Width to define the video resolution width, in pixels, for this
-- rule.
minTopRenditionSize_width :: Lens.Lens' MinTopRenditionSize (Prelude.Maybe Prelude.Natural)
minTopRenditionSize_width = Lens.lens (\MinTopRenditionSize' {width} -> width) (\s@MinTopRenditionSize' {} a -> s {width = a} :: MinTopRenditionSize)

-- | Use Height to define the video resolution height, in pixels, for this
-- rule.
minTopRenditionSize_height :: Lens.Lens' MinTopRenditionSize (Prelude.Maybe Prelude.Natural)
minTopRenditionSize_height = Lens.lens (\MinTopRenditionSize' {height} -> height) (\s@MinTopRenditionSize' {} a -> s {height = a} :: MinTopRenditionSize)

instance Core.FromJSON MinTopRenditionSize where
  parseJSON =
    Core.withObject
      "MinTopRenditionSize"
      ( \x ->
          MinTopRenditionSize'
            Prelude.<$> (x Core..:? "width")
            Prelude.<*> (x Core..:? "height")
      )

instance Prelude.Hashable MinTopRenditionSize where
  hashWithSalt _salt MinTopRenditionSize' {..} =
    _salt `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` height

instance Prelude.NFData MinTopRenditionSize where
  rnf MinTopRenditionSize' {..} =
    Prelude.rnf width `Prelude.seq` Prelude.rnf height

instance Core.ToJSON MinTopRenditionSize where
  toJSON MinTopRenditionSize' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("width" Core..=) Prelude.<$> width,
            ("height" Core..=) Prelude.<$> height
          ]
      )
