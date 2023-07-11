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
-- Module      : Amazonka.GuardDuty.Types.ScannedItemCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScannedItemCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Total number of scanned files.
--
-- /See:/ 'newScannedItemCount' smart constructor.
data ScannedItemCount = ScannedItemCount'
  { -- | Number of files scanned.
    files :: Prelude.Maybe Prelude.Int,
    -- | Total GB of files scanned for malware.
    totalGb :: Prelude.Maybe Prelude.Int,
    -- | Total number of scanned volumes.
    volumes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScannedItemCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'files', 'scannedItemCount_files' - Number of files scanned.
--
-- 'totalGb', 'scannedItemCount_totalGb' - Total GB of files scanned for malware.
--
-- 'volumes', 'scannedItemCount_volumes' - Total number of scanned volumes.
newScannedItemCount ::
  ScannedItemCount
newScannedItemCount =
  ScannedItemCount'
    { files = Prelude.Nothing,
      totalGb = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | Number of files scanned.
scannedItemCount_files :: Lens.Lens' ScannedItemCount (Prelude.Maybe Prelude.Int)
scannedItemCount_files = Lens.lens (\ScannedItemCount' {files} -> files) (\s@ScannedItemCount' {} a -> s {files = a} :: ScannedItemCount)

-- | Total GB of files scanned for malware.
scannedItemCount_totalGb :: Lens.Lens' ScannedItemCount (Prelude.Maybe Prelude.Int)
scannedItemCount_totalGb = Lens.lens (\ScannedItemCount' {totalGb} -> totalGb) (\s@ScannedItemCount' {} a -> s {totalGb = a} :: ScannedItemCount)

-- | Total number of scanned volumes.
scannedItemCount_volumes :: Lens.Lens' ScannedItemCount (Prelude.Maybe Prelude.Int)
scannedItemCount_volumes = Lens.lens (\ScannedItemCount' {volumes} -> volumes) (\s@ScannedItemCount' {} a -> s {volumes = a} :: ScannedItemCount)

instance Data.FromJSON ScannedItemCount where
  parseJSON =
    Data.withObject
      "ScannedItemCount"
      ( \x ->
          ScannedItemCount'
            Prelude.<$> (x Data..:? "files")
            Prelude.<*> (x Data..:? "totalGb")
            Prelude.<*> (x Data..:? "volumes")
      )

instance Prelude.Hashable ScannedItemCount where
  hashWithSalt _salt ScannedItemCount' {..} =
    _salt
      `Prelude.hashWithSalt` files
      `Prelude.hashWithSalt` totalGb
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData ScannedItemCount where
  rnf ScannedItemCount' {..} =
    Prelude.rnf files
      `Prelude.seq` Prelude.rnf totalGb
      `Prelude.seq` Prelude.rnf volumes
