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
-- Module      : Amazonka.DeviceFarm.Types.Resolution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Resolution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the screen resolution of a device in height and width,
-- expressed in pixels.
--
-- /See:/ 'newResolution' smart constructor.
data Resolution = Resolution'
  { -- | The screen resolution\'s width, expressed in pixels.
    width :: Prelude.Maybe Prelude.Int,
    -- | The screen resolution\'s height, expressed in pixels.
    height :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'width', 'resolution_width' - The screen resolution\'s width, expressed in pixels.
--
-- 'height', 'resolution_height' - The screen resolution\'s height, expressed in pixels.
newResolution ::
  Resolution
newResolution =
  Resolution'
    { width = Prelude.Nothing,
      height = Prelude.Nothing
    }

-- | The screen resolution\'s width, expressed in pixels.
resolution_width :: Lens.Lens' Resolution (Prelude.Maybe Prelude.Int)
resolution_width = Lens.lens (\Resolution' {width} -> width) (\s@Resolution' {} a -> s {width = a} :: Resolution)

-- | The screen resolution\'s height, expressed in pixels.
resolution_height :: Lens.Lens' Resolution (Prelude.Maybe Prelude.Int)
resolution_height = Lens.lens (\Resolution' {height} -> height) (\s@Resolution' {} a -> s {height = a} :: Resolution)

instance Data.FromJSON Resolution where
  parseJSON =
    Data.withObject
      "Resolution"
      ( \x ->
          Resolution'
            Prelude.<$> (x Data..:? "width")
            Prelude.<*> (x Data..:? "height")
      )

instance Prelude.Hashable Resolution where
  hashWithSalt _salt Resolution' {..} =
    _salt `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` height

instance Prelude.NFData Resolution where
  rnf Resolution' {..} =
    Prelude.rnf width `Prelude.seq` Prelude.rnf height
