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
-- Module      : Network.AWS.ECR.Types.ImageScanningConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanningConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The image scanning configuration for a repository.
--
-- /See:/ 'newImageScanningConfiguration' smart constructor.
data ImageScanningConfiguration = ImageScanningConfiguration'
  { -- | The setting that determines whether images are scanned after being
    -- pushed to a repository. If set to @true@, images will be scanned after
    -- being pushed. If this parameter is not specified, it will default to
    -- @false@ and images will not be scanned unless a scan is manually started
    -- with the StartImageScan API.
    scanOnPush :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanOnPush', 'imageScanningConfiguration_scanOnPush' - The setting that determines whether images are scanned after being
-- pushed to a repository. If set to @true@, images will be scanned after
-- being pushed. If this parameter is not specified, it will default to
-- @false@ and images will not be scanned unless a scan is manually started
-- with the StartImageScan API.
newImageScanningConfiguration ::
  ImageScanningConfiguration
newImageScanningConfiguration =
  ImageScanningConfiguration'
    { scanOnPush =
        Core.Nothing
    }

-- | The setting that determines whether images are scanned after being
-- pushed to a repository. If set to @true@, images will be scanned after
-- being pushed. If this parameter is not specified, it will default to
-- @false@ and images will not be scanned unless a scan is manually started
-- with the StartImageScan API.
imageScanningConfiguration_scanOnPush :: Lens.Lens' ImageScanningConfiguration (Core.Maybe Core.Bool)
imageScanningConfiguration_scanOnPush = Lens.lens (\ImageScanningConfiguration' {scanOnPush} -> scanOnPush) (\s@ImageScanningConfiguration' {} a -> s {scanOnPush = a} :: ImageScanningConfiguration)

instance Core.FromJSON ImageScanningConfiguration where
  parseJSON =
    Core.withObject
      "ImageScanningConfiguration"
      ( \x ->
          ImageScanningConfiguration'
            Core.<$> (x Core..:? "scanOnPush")
      )

instance Core.Hashable ImageScanningConfiguration

instance Core.NFData ImageScanningConfiguration

instance Core.ToJSON ImageScanningConfiguration where
  toJSON ImageScanningConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [("scanOnPush" Core..=) Core.<$> scanOnPush]
      )
