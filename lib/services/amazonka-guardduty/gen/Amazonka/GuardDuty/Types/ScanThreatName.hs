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
-- Module      : Amazonka.GuardDuty.Types.ScanThreatName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanThreatName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.ScanFilePath
import qualified Amazonka.Prelude as Prelude

-- | Contains files infected with the given threat providing details of
-- malware name and severity.
--
-- /See:/ 'newScanThreatName' smart constructor.
data ScanThreatName = ScanThreatName'
  { -- | Severity of threat identified as part of the malware scan.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The name of the identified threat.
    name :: Prelude.Maybe Prelude.Text,
    -- | Total number of files infected with given threat.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | List of infected files in EBS volume with details.
    filePaths :: Prelude.Maybe [ScanFilePath]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanThreatName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severity', 'scanThreatName_severity' - Severity of threat identified as part of the malware scan.
--
-- 'name', 'scanThreatName_name' - The name of the identified threat.
--
-- 'itemCount', 'scanThreatName_itemCount' - Total number of files infected with given threat.
--
-- 'filePaths', 'scanThreatName_filePaths' - List of infected files in EBS volume with details.
newScanThreatName ::
  ScanThreatName
newScanThreatName =
  ScanThreatName'
    { severity = Prelude.Nothing,
      name = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      filePaths = Prelude.Nothing
    }

-- | Severity of threat identified as part of the malware scan.
scanThreatName_severity :: Lens.Lens' ScanThreatName (Prelude.Maybe Prelude.Text)
scanThreatName_severity = Lens.lens (\ScanThreatName' {severity} -> severity) (\s@ScanThreatName' {} a -> s {severity = a} :: ScanThreatName)

-- | The name of the identified threat.
scanThreatName_name :: Lens.Lens' ScanThreatName (Prelude.Maybe Prelude.Text)
scanThreatName_name = Lens.lens (\ScanThreatName' {name} -> name) (\s@ScanThreatName' {} a -> s {name = a} :: ScanThreatName)

-- | Total number of files infected with given threat.
scanThreatName_itemCount :: Lens.Lens' ScanThreatName (Prelude.Maybe Prelude.Int)
scanThreatName_itemCount = Lens.lens (\ScanThreatName' {itemCount} -> itemCount) (\s@ScanThreatName' {} a -> s {itemCount = a} :: ScanThreatName)

-- | List of infected files in EBS volume with details.
scanThreatName_filePaths :: Lens.Lens' ScanThreatName (Prelude.Maybe [ScanFilePath])
scanThreatName_filePaths = Lens.lens (\ScanThreatName' {filePaths} -> filePaths) (\s@ScanThreatName' {} a -> s {filePaths = a} :: ScanThreatName) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ScanThreatName where
  parseJSON =
    Core.withObject
      "ScanThreatName"
      ( \x ->
          ScanThreatName'
            Prelude.<$> (x Core..:? "severity")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "itemCount")
            Prelude.<*> (x Core..:? "filePaths" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ScanThreatName where
  hashWithSalt _salt ScanThreatName' {..} =
    _salt `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` filePaths

instance Prelude.NFData ScanThreatName where
  rnf ScanThreatName' {..} =
    Prelude.rnf severity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf filePaths
