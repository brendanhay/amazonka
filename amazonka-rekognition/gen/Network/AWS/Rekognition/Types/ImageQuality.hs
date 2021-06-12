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
-- Module      : Network.AWS.Rekognition.Types.ImageQuality
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ImageQuality where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies face image brightness and sharpness.
--
-- /See:/ 'newImageQuality' smart constructor.
data ImageQuality = ImageQuality'
  { -- | Value representing brightness of the face. The service returns a value
    -- between 0 and 100 (inclusive). A higher value indicates a brighter face
    -- image.
    brightness :: Core.Maybe Core.Double,
    -- | Value representing sharpness of the face. The service returns a value
    -- between 0 and 100 (inclusive). A higher value indicates a sharper face
    -- image.
    sharpness :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageQuality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brightness', 'imageQuality_brightness' - Value representing brightness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a brighter face
-- image.
--
-- 'sharpness', 'imageQuality_sharpness' - Value representing sharpness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a sharper face
-- image.
newImageQuality ::
  ImageQuality
newImageQuality =
  ImageQuality'
    { brightness = Core.Nothing,
      sharpness = Core.Nothing
    }

-- | Value representing brightness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a brighter face
-- image.
imageQuality_brightness :: Lens.Lens' ImageQuality (Core.Maybe Core.Double)
imageQuality_brightness = Lens.lens (\ImageQuality' {brightness} -> brightness) (\s@ImageQuality' {} a -> s {brightness = a} :: ImageQuality)

-- | Value representing sharpness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a sharper face
-- image.
imageQuality_sharpness :: Lens.Lens' ImageQuality (Core.Maybe Core.Double)
imageQuality_sharpness = Lens.lens (\ImageQuality' {sharpness} -> sharpness) (\s@ImageQuality' {} a -> s {sharpness = a} :: ImageQuality)

instance Core.FromJSON ImageQuality where
  parseJSON =
    Core.withObject
      "ImageQuality"
      ( \x ->
          ImageQuality'
            Core.<$> (x Core..:? "Brightness")
            Core.<*> (x Core..:? "Sharpness")
      )

instance Core.Hashable ImageQuality

instance Core.NFData ImageQuality
