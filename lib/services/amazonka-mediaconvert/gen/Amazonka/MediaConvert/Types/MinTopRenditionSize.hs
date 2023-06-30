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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MinTopRenditionSize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Use Height to define the video resolution height, in pixels, for this
    -- rule.
    height :: Prelude.Maybe Prelude.Natural,
    -- | Use Width to define the video resolution width, in pixels, for this
    -- rule.
    width :: Prelude.Maybe Prelude.Natural
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
-- 'height', 'minTopRenditionSize_height' - Use Height to define the video resolution height, in pixels, for this
-- rule.
--
-- 'width', 'minTopRenditionSize_width' - Use Width to define the video resolution width, in pixels, for this
-- rule.
newMinTopRenditionSize ::
  MinTopRenditionSize
newMinTopRenditionSize =
  MinTopRenditionSize'
    { height = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | Use Height to define the video resolution height, in pixels, for this
-- rule.
minTopRenditionSize_height :: Lens.Lens' MinTopRenditionSize (Prelude.Maybe Prelude.Natural)
minTopRenditionSize_height = Lens.lens (\MinTopRenditionSize' {height} -> height) (\s@MinTopRenditionSize' {} a -> s {height = a} :: MinTopRenditionSize)

-- | Use Width to define the video resolution width, in pixels, for this
-- rule.
minTopRenditionSize_width :: Lens.Lens' MinTopRenditionSize (Prelude.Maybe Prelude.Natural)
minTopRenditionSize_width = Lens.lens (\MinTopRenditionSize' {width} -> width) (\s@MinTopRenditionSize' {} a -> s {width = a} :: MinTopRenditionSize)

instance Data.FromJSON MinTopRenditionSize where
  parseJSON =
    Data.withObject
      "MinTopRenditionSize"
      ( \x ->
          MinTopRenditionSize'
            Prelude.<$> (x Data..:? "height")
            Prelude.<*> (x Data..:? "width")
      )

instance Prelude.Hashable MinTopRenditionSize where
  hashWithSalt _salt MinTopRenditionSize' {..} =
    _salt
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` width

instance Prelude.NFData MinTopRenditionSize where
  rnf MinTopRenditionSize' {..} =
    Prelude.rnf height `Prelude.seq` Prelude.rnf width

instance Data.ToJSON MinTopRenditionSize where
  toJSON MinTopRenditionSize' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("height" Data..=) Prelude.<$> height,
            ("width" Data..=) Prelude.<$> width
          ]
      )
