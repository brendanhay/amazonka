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
-- Module      : Amazonka.ECR.Types.ImageScanningConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageScanningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The image scanning configuration for a repository.
--
-- /See:/ 'newImageScanningConfiguration' smart constructor.
data ImageScanningConfiguration = ImageScanningConfiguration'
  { -- | The setting that determines whether images are scanned after being
    -- pushed to a repository. If set to @true@, images will be scanned after
    -- being pushed. If this parameter is not specified, it will default to
    -- @false@ and images will not be scanned unless a scan is manually started
    -- with the
    -- <https://docs.aws.amazon.com/AmazonECR/latest/APIReference/API_StartImageScan.html API_StartImageScan>
    -- API.
    scanOnPush :: Prelude.Maybe Prelude.Bool
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
-- 'scanOnPush', 'imageScanningConfiguration_scanOnPush' - The setting that determines whether images are scanned after being
-- pushed to a repository. If set to @true@, images will be scanned after
-- being pushed. If this parameter is not specified, it will default to
-- @false@ and images will not be scanned unless a scan is manually started
-- with the
-- <https://docs.aws.amazon.com/AmazonECR/latest/APIReference/API_StartImageScan.html API_StartImageScan>
-- API.
newImageScanningConfiguration ::
  ImageScanningConfiguration
newImageScanningConfiguration =
  ImageScanningConfiguration'
    { scanOnPush =
        Prelude.Nothing
    }

-- | The setting that determines whether images are scanned after being
-- pushed to a repository. If set to @true@, images will be scanned after
-- being pushed. If this parameter is not specified, it will default to
-- @false@ and images will not be scanned unless a scan is manually started
-- with the
-- <https://docs.aws.amazon.com/AmazonECR/latest/APIReference/API_StartImageScan.html API_StartImageScan>
-- API.
imageScanningConfiguration_scanOnPush :: Lens.Lens' ImageScanningConfiguration (Prelude.Maybe Prelude.Bool)
imageScanningConfiguration_scanOnPush = Lens.lens (\ImageScanningConfiguration' {scanOnPush} -> scanOnPush) (\s@ImageScanningConfiguration' {} a -> s {scanOnPush = a} :: ImageScanningConfiguration)

instance Data.FromJSON ImageScanningConfiguration where
  parseJSON =
    Data.withObject
      "ImageScanningConfiguration"
      ( \x ->
          ImageScanningConfiguration'
            Prelude.<$> (x Data..:? "scanOnPush")
      )

instance Prelude.Hashable ImageScanningConfiguration where
  hashWithSalt _salt ImageScanningConfiguration' {..} =
    _salt `Prelude.hashWithSalt` scanOnPush

instance Prelude.NFData ImageScanningConfiguration where
  rnf ImageScanningConfiguration' {..} =
    Prelude.rnf scanOnPush

instance Data.ToJSON ImageScanningConfiguration where
  toJSON ImageScanningConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("scanOnPush" Data..=) Prelude.<$> scanOnPush]
      )
