{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECR.Types.ImageScanStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanStatus where

import Network.AWS.ECR.Types.ScanStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The current status of an image scan.
--
-- /See:/ 'newImageScanStatus' smart constructor.
data ImageScanStatus = ImageScanStatus'
  { -- | The current state of an image scan.
    status :: Prelude.Maybe ScanStatus,
    -- | The description of the image scan status.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImageScanStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'imageScanStatus_status' - The current state of an image scan.
--
-- 'description', 'imageScanStatus_description' - The description of the image scan status.
newImageScanStatus ::
  ImageScanStatus
newImageScanStatus =
  ImageScanStatus'
    { status = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The current state of an image scan.
imageScanStatus_status :: Lens.Lens' ImageScanStatus (Prelude.Maybe ScanStatus)
imageScanStatus_status = Lens.lens (\ImageScanStatus' {status} -> status) (\s@ImageScanStatus' {} a -> s {status = a} :: ImageScanStatus)

-- | The description of the image scan status.
imageScanStatus_description :: Lens.Lens' ImageScanStatus (Prelude.Maybe Prelude.Text)
imageScanStatus_description = Lens.lens (\ImageScanStatus' {description} -> description) (\s@ImageScanStatus' {} a -> s {description = a} :: ImageScanStatus)

instance Prelude.FromJSON ImageScanStatus where
  parseJSON =
    Prelude.withObject
      "ImageScanStatus"
      ( \x ->
          ImageScanStatus'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "description")
      )

instance Prelude.Hashable ImageScanStatus

instance Prelude.NFData ImageScanStatus
