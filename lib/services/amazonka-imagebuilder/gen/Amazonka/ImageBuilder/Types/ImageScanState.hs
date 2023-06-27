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
-- Module      : Amazonka.ImageBuilder.Types.ImageScanState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ImageScanState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.ImageScanStatus
import qualified Amazonka.Prelude as Prelude

-- | Shows the vulnerability scan status for a specific image, and the reason
-- for that status.
--
-- /See:/ 'newImageScanState' smart constructor.
data ImageScanState = ImageScanState'
  { -- | The reason for the scan status for the image.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The current state of vulnerability scans for the image.
    status :: Prelude.Maybe ImageScanStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'imageScanState_reason' - The reason for the scan status for the image.
--
-- 'status', 'imageScanState_status' - The current state of vulnerability scans for the image.
newImageScanState ::
  ImageScanState
newImageScanState =
  ImageScanState'
    { reason = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The reason for the scan status for the image.
imageScanState_reason :: Lens.Lens' ImageScanState (Prelude.Maybe Prelude.Text)
imageScanState_reason = Lens.lens (\ImageScanState' {reason} -> reason) (\s@ImageScanState' {} a -> s {reason = a} :: ImageScanState)

-- | The current state of vulnerability scans for the image.
imageScanState_status :: Lens.Lens' ImageScanState (Prelude.Maybe ImageScanStatus)
imageScanState_status = Lens.lens (\ImageScanState' {status} -> status) (\s@ImageScanState' {} a -> s {status = a} :: ImageScanState)

instance Data.FromJSON ImageScanState where
  parseJSON =
    Data.withObject
      "ImageScanState"
      ( \x ->
          ImageScanState'
            Prelude.<$> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ImageScanState where
  hashWithSalt _salt ImageScanState' {..} =
    _salt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImageScanState where
  rnf ImageScanState' {..} =
    Prelude.rnf reason `Prelude.seq` Prelude.rnf status
