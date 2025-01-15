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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageTestsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configure image tests for your pipeline build. Tests run after building
-- the image, to verify that the AMI or container image is valid before
-- distributing it.
--
-- /See:/ 'newImageTestsConfiguration' smart constructor.
data ImageTestsConfiguration = ImageTestsConfiguration'
  { -- | Determines if tests should run after building the image. Image Builder
    -- defaults to enable tests to run following the image build, before image
    -- distribution.
    imageTestsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The maximum time in minutes that tests are permitted to run.
    timeoutMinutes :: Prelude.Maybe Prelude.Natural
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
-- 'imageTestsEnabled', 'imageTestsConfiguration_imageTestsEnabled' - Determines if tests should run after building the image. Image Builder
-- defaults to enable tests to run following the image build, before image
-- distribution.
--
-- 'timeoutMinutes', 'imageTestsConfiguration_timeoutMinutes' - The maximum time in minutes that tests are permitted to run.
newImageTestsConfiguration ::
  ImageTestsConfiguration
newImageTestsConfiguration =
  ImageTestsConfiguration'
    { imageTestsEnabled =
        Prelude.Nothing,
      timeoutMinutes = Prelude.Nothing
    }

-- | Determines if tests should run after building the image. Image Builder
-- defaults to enable tests to run following the image build, before image
-- distribution.
imageTestsConfiguration_imageTestsEnabled :: Lens.Lens' ImageTestsConfiguration (Prelude.Maybe Prelude.Bool)
imageTestsConfiguration_imageTestsEnabled = Lens.lens (\ImageTestsConfiguration' {imageTestsEnabled} -> imageTestsEnabled) (\s@ImageTestsConfiguration' {} a -> s {imageTestsEnabled = a} :: ImageTestsConfiguration)

-- | The maximum time in minutes that tests are permitted to run.
imageTestsConfiguration_timeoutMinutes :: Lens.Lens' ImageTestsConfiguration (Prelude.Maybe Prelude.Natural)
imageTestsConfiguration_timeoutMinutes = Lens.lens (\ImageTestsConfiguration' {timeoutMinutes} -> timeoutMinutes) (\s@ImageTestsConfiguration' {} a -> s {timeoutMinutes = a} :: ImageTestsConfiguration)

instance Data.FromJSON ImageTestsConfiguration where
  parseJSON =
    Data.withObject
      "ImageTestsConfiguration"
      ( \x ->
          ImageTestsConfiguration'
            Prelude.<$> (x Data..:? "imageTestsEnabled")
            Prelude.<*> (x Data..:? "timeoutMinutes")
      )

instance Prelude.Hashable ImageTestsConfiguration where
  hashWithSalt _salt ImageTestsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` imageTestsEnabled
      `Prelude.hashWithSalt` timeoutMinutes

instance Prelude.NFData ImageTestsConfiguration where
  rnf ImageTestsConfiguration' {..} =
    Prelude.rnf imageTestsEnabled `Prelude.seq`
      Prelude.rnf timeoutMinutes

instance Data.ToJSON ImageTestsConfiguration where
  toJSON ImageTestsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("imageTestsEnabled" Data..=)
              Prelude.<$> imageTestsEnabled,
            ("timeoutMinutes" Data..=)
              Prelude.<$> timeoutMinutes
          ]
      )
