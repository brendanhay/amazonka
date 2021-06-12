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
-- Module      : Network.AWS.DeviceFarm.Types.Resolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Resolution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the screen resolution of a device in height and width,
-- expressed in pixels.
--
-- /See:/ 'newResolution' smart constructor.
data Resolution = Resolution'
  { -- | The screen resolution\'s height, expressed in pixels.
    height :: Core.Maybe Core.Int,
    -- | The screen resolution\'s width, expressed in pixels.
    width :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Resolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'resolution_height' - The screen resolution\'s height, expressed in pixels.
--
-- 'width', 'resolution_width' - The screen resolution\'s width, expressed in pixels.
newResolution ::
  Resolution
newResolution =
  Resolution'
    { height = Core.Nothing,
      width = Core.Nothing
    }

-- | The screen resolution\'s height, expressed in pixels.
resolution_height :: Lens.Lens' Resolution (Core.Maybe Core.Int)
resolution_height = Lens.lens (\Resolution' {height} -> height) (\s@Resolution' {} a -> s {height = a} :: Resolution)

-- | The screen resolution\'s width, expressed in pixels.
resolution_width :: Lens.Lens' Resolution (Core.Maybe Core.Int)
resolution_width = Lens.lens (\Resolution' {width} -> width) (\s@Resolution' {} a -> s {width = a} :: Resolution)

instance Core.FromJSON Resolution where
  parseJSON =
    Core.withObject
      "Resolution"
      ( \x ->
          Resolution'
            Core.<$> (x Core..:? "height") Core.<*> (x Core..:? "width")
      )

instance Core.Hashable Resolution

instance Core.NFData Resolution
