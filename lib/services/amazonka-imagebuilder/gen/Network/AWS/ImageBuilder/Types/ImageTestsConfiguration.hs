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
-- Module      : Amazonka.ImageBuilder.Types.ImageTestsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageTestsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Image tests configuration.
--
-- /See:/ 'newImageTestsConfiguration' smart constructor.
data ImageTestsConfiguration = ImageTestsConfiguration'
  { -- | The maximum time in minutes that tests are permitted to run.
    timeoutMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Defines if tests should be executed when building this image.
    imageTestsEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageTestsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutMinutes', 'imageTestsConfiguration_timeoutMinutes' - The maximum time in minutes that tests are permitted to run.
--
-- 'imageTestsEnabled', 'imageTestsConfiguration_imageTestsEnabled' - Defines if tests should be executed when building this image.
newImageTestsConfiguration ::
  ImageTestsConfiguration
newImageTestsConfiguration =
  ImageTestsConfiguration'
    { timeoutMinutes =
        Prelude.Nothing,
      imageTestsEnabled = Prelude.Nothing
    }

-- | The maximum time in minutes that tests are permitted to run.
imageTestsConfiguration_timeoutMinutes :: Lens.Lens' ImageTestsConfiguration (Prelude.Maybe Prelude.Natural)
imageTestsConfiguration_timeoutMinutes = Lens.lens (\ImageTestsConfiguration' {timeoutMinutes} -> timeoutMinutes) (\s@ImageTestsConfiguration' {} a -> s {timeoutMinutes = a} :: ImageTestsConfiguration)

-- | Defines if tests should be executed when building this image.
imageTestsConfiguration_imageTestsEnabled :: Lens.Lens' ImageTestsConfiguration (Prelude.Maybe Prelude.Bool)
imageTestsConfiguration_imageTestsEnabled = Lens.lens (\ImageTestsConfiguration' {imageTestsEnabled} -> imageTestsEnabled) (\s@ImageTestsConfiguration' {} a -> s {imageTestsEnabled = a} :: ImageTestsConfiguration)

instance Core.FromJSON ImageTestsConfiguration where
  parseJSON =
    Core.withObject
      "ImageTestsConfiguration"
      ( \x ->
          ImageTestsConfiguration'
            Prelude.<$> (x Core..:? "timeoutMinutes")
            Prelude.<*> (x Core..:? "imageTestsEnabled")
      )

instance Prelude.Hashable ImageTestsConfiguration

instance Prelude.NFData ImageTestsConfiguration

instance Core.ToJSON ImageTestsConfiguration where
  toJSON ImageTestsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timeoutMinutes" Core..=)
              Prelude.<$> timeoutMinutes,
            ("imageTestsEnabled" Core..=)
              Prelude.<$> imageTestsEnabled
          ]
      )
