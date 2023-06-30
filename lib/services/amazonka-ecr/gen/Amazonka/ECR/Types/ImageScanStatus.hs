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
-- Module      : Amazonka.ECR.Types.ImageScanStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageScanStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ScanStatus
import qualified Amazonka.Prelude as Prelude

-- | The current status of an image scan.
--
-- /See:/ 'newImageScanStatus' smart constructor.
data ImageScanStatus = ImageScanStatus'
  { -- | The description of the image scan status.
    description :: Prelude.Maybe Prelude.Text,
    -- | The current state of an image scan.
    status :: Prelude.Maybe ScanStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'imageScanStatus_description' - The description of the image scan status.
--
-- 'status', 'imageScanStatus_status' - The current state of an image scan.
newImageScanStatus ::
  ImageScanStatus
newImageScanStatus =
  ImageScanStatus'
    { description = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The description of the image scan status.
imageScanStatus_description :: Lens.Lens' ImageScanStatus (Prelude.Maybe Prelude.Text)
imageScanStatus_description = Lens.lens (\ImageScanStatus' {description} -> description) (\s@ImageScanStatus' {} a -> s {description = a} :: ImageScanStatus)

-- | The current state of an image scan.
imageScanStatus_status :: Lens.Lens' ImageScanStatus (Prelude.Maybe ScanStatus)
imageScanStatus_status = Lens.lens (\ImageScanStatus' {status} -> status) (\s@ImageScanStatus' {} a -> s {status = a} :: ImageScanStatus)

instance Data.FromJSON ImageScanStatus where
  parseJSON =
    Data.withObject
      "ImageScanStatus"
      ( \x ->
          ImageScanStatus'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ImageScanStatus where
  hashWithSalt _salt ImageScanStatus' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImageScanStatus where
  rnf ImageScanStatus' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf status
