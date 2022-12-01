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
-- Module      : Amazonka.GuardDuty.Types.EbsVolumeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.EbsVolumeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.VolumeDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains list of scanned and skipped EBS volumes with details.
--
-- /See:/ 'newEbsVolumeDetails' smart constructor.
data EbsVolumeDetails = EbsVolumeDetails'
  { -- | List of EBS volumes that were skipped from the malware scan.
    skippedVolumeDetails :: Prelude.Maybe [VolumeDetail],
    -- | List of EBS volumes that were scanned.
    scannedVolumeDetails :: Prelude.Maybe [VolumeDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsVolumeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skippedVolumeDetails', 'ebsVolumeDetails_skippedVolumeDetails' - List of EBS volumes that were skipped from the malware scan.
--
-- 'scannedVolumeDetails', 'ebsVolumeDetails_scannedVolumeDetails' - List of EBS volumes that were scanned.
newEbsVolumeDetails ::
  EbsVolumeDetails
newEbsVolumeDetails =
  EbsVolumeDetails'
    { skippedVolumeDetails =
        Prelude.Nothing,
      scannedVolumeDetails = Prelude.Nothing
    }

-- | List of EBS volumes that were skipped from the malware scan.
ebsVolumeDetails_skippedVolumeDetails :: Lens.Lens' EbsVolumeDetails (Prelude.Maybe [VolumeDetail])
ebsVolumeDetails_skippedVolumeDetails = Lens.lens (\EbsVolumeDetails' {skippedVolumeDetails} -> skippedVolumeDetails) (\s@EbsVolumeDetails' {} a -> s {skippedVolumeDetails = a} :: EbsVolumeDetails) Prelude.. Lens.mapping Lens.coerced

-- | List of EBS volumes that were scanned.
ebsVolumeDetails_scannedVolumeDetails :: Lens.Lens' EbsVolumeDetails (Prelude.Maybe [VolumeDetail])
ebsVolumeDetails_scannedVolumeDetails = Lens.lens (\EbsVolumeDetails' {scannedVolumeDetails} -> scannedVolumeDetails) (\s@EbsVolumeDetails' {} a -> s {scannedVolumeDetails = a} :: EbsVolumeDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON EbsVolumeDetails where
  parseJSON =
    Core.withObject
      "EbsVolumeDetails"
      ( \x ->
          EbsVolumeDetails'
            Prelude.<$> ( x Core..:? "skippedVolumeDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "scannedVolumeDetails"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EbsVolumeDetails where
  hashWithSalt _salt EbsVolumeDetails' {..} =
    _salt `Prelude.hashWithSalt` skippedVolumeDetails
      `Prelude.hashWithSalt` scannedVolumeDetails

instance Prelude.NFData EbsVolumeDetails where
  rnf EbsVolumeDetails' {..} =
    Prelude.rnf skippedVolumeDetails
      `Prelude.seq` Prelude.rnf scannedVolumeDetails
