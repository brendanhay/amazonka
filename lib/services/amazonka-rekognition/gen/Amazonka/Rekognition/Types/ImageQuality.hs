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
-- Module      : Amazonka.Rekognition.Types.ImageQuality
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ImageQuality where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies face image brightness and sharpness.
--
-- /See:/ 'newImageQuality' smart constructor.
data ImageQuality = ImageQuality'
  { -- | Value representing sharpness of the face. The service returns a value
    -- between 0 and 100 (inclusive). A higher value indicates a sharper face
    -- image.
    sharpness :: Prelude.Maybe Prelude.Double,
    -- | Value representing brightness of the face. The service returns a value
    -- between 0 and 100 (inclusive). A higher value indicates a brighter face
    -- image.
    brightness :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageQuality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharpness', 'imageQuality_sharpness' - Value representing sharpness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a sharper face
-- image.
--
-- 'brightness', 'imageQuality_brightness' - Value representing brightness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a brighter face
-- image.
newImageQuality ::
  ImageQuality
newImageQuality =
  ImageQuality'
    { sharpness = Prelude.Nothing,
      brightness = Prelude.Nothing
    }

-- | Value representing sharpness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a sharper face
-- image.
imageQuality_sharpness :: Lens.Lens' ImageQuality (Prelude.Maybe Prelude.Double)
imageQuality_sharpness = Lens.lens (\ImageQuality' {sharpness} -> sharpness) (\s@ImageQuality' {} a -> s {sharpness = a} :: ImageQuality)

-- | Value representing brightness of the face. The service returns a value
-- between 0 and 100 (inclusive). A higher value indicates a brighter face
-- image.
imageQuality_brightness :: Lens.Lens' ImageQuality (Prelude.Maybe Prelude.Double)
imageQuality_brightness = Lens.lens (\ImageQuality' {brightness} -> brightness) (\s@ImageQuality' {} a -> s {brightness = a} :: ImageQuality)

instance Data.FromJSON ImageQuality where
  parseJSON =
    Data.withObject
      "ImageQuality"
      ( \x ->
          ImageQuality'
            Prelude.<$> (x Data..:? "Sharpness")
            Prelude.<*> (x Data..:? "Brightness")
      )

instance Prelude.Hashable ImageQuality where
  hashWithSalt _salt ImageQuality' {..} =
    _salt `Prelude.hashWithSalt` sharpness
      `Prelude.hashWithSalt` brightness

instance Prelude.NFData ImageQuality where
  rnf ImageQuality' {..} =
    Prelude.rnf sharpness
      `Prelude.seq` Prelude.rnf brightness
