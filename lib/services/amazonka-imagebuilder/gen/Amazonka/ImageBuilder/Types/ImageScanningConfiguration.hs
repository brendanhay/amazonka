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
-- Module      : Amazonka.ImageBuilder.Types.ImageScanningConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageScanningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.EcrConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Contains settings for Image Builder image resource and container image
-- scans.
--
-- /See:/ 'newImageScanningConfiguration' smart constructor.
data ImageScanningConfiguration = ImageScanningConfiguration'
  { -- | Contains Amazon ECR settings for vulnerability scans.
    ecrConfiguration :: Prelude.Maybe EcrConfiguration,
    -- | A setting that indicates whether Image Builder keeps a snapshot of the
    -- vulnerability scans that Amazon Inspector runs against the build
    -- instance when you create a new image.
    imageScanningEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecrConfiguration', 'imageScanningConfiguration_ecrConfiguration' - Contains Amazon ECR settings for vulnerability scans.
--
-- 'imageScanningEnabled', 'imageScanningConfiguration_imageScanningEnabled' - A setting that indicates whether Image Builder keeps a snapshot of the
-- vulnerability scans that Amazon Inspector runs against the build
-- instance when you create a new image.
newImageScanningConfiguration ::
  ImageScanningConfiguration
newImageScanningConfiguration =
  ImageScanningConfiguration'
    { ecrConfiguration =
        Prelude.Nothing,
      imageScanningEnabled = Prelude.Nothing
    }

-- | Contains Amazon ECR settings for vulnerability scans.
imageScanningConfiguration_ecrConfiguration :: Lens.Lens' ImageScanningConfiguration (Prelude.Maybe EcrConfiguration)
imageScanningConfiguration_ecrConfiguration = Lens.lens (\ImageScanningConfiguration' {ecrConfiguration} -> ecrConfiguration) (\s@ImageScanningConfiguration' {} a -> s {ecrConfiguration = a} :: ImageScanningConfiguration)

-- | A setting that indicates whether Image Builder keeps a snapshot of the
-- vulnerability scans that Amazon Inspector runs against the build
-- instance when you create a new image.
imageScanningConfiguration_imageScanningEnabled :: Lens.Lens' ImageScanningConfiguration (Prelude.Maybe Prelude.Bool)
imageScanningConfiguration_imageScanningEnabled = Lens.lens (\ImageScanningConfiguration' {imageScanningEnabled} -> imageScanningEnabled) (\s@ImageScanningConfiguration' {} a -> s {imageScanningEnabled = a} :: ImageScanningConfiguration)

instance Data.FromJSON ImageScanningConfiguration where
  parseJSON =
    Data.withObject
      "ImageScanningConfiguration"
      ( \x ->
          ImageScanningConfiguration'
            Prelude.<$> (x Data..:? "ecrConfiguration")
            Prelude.<*> (x Data..:? "imageScanningEnabled")
      )

instance Prelude.Hashable ImageScanningConfiguration where
  hashWithSalt _salt ImageScanningConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` ecrConfiguration
      `Prelude.hashWithSalt` imageScanningEnabled

instance Prelude.NFData ImageScanningConfiguration where
  rnf ImageScanningConfiguration' {..} =
    Prelude.rnf ecrConfiguration
      `Prelude.seq` Prelude.rnf imageScanningEnabled

instance Data.ToJSON ImageScanningConfiguration where
  toJSON ImageScanningConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ecrConfiguration" Data..=)
              Prelude.<$> ecrConfiguration,
            ("imageScanningEnabled" Data..=)
              Prelude.<$> imageScanningEnabled
          ]
      )
