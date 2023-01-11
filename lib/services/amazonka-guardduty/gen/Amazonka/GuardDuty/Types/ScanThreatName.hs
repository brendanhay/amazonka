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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanThreatName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ScanFilePath
import qualified Amazonka.Prelude as Prelude

-- | Contains files infected with the given threat providing details of
-- malware name and severity.
--
-- /See:/ 'newScanThreatName' smart constructor.
data ScanThreatName = ScanThreatName'
  { -- | List of infected files in EBS volume with details.
    filePaths :: Prelude.Maybe [ScanFilePath],
    -- | Total number of files infected with given threat.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the identified threat.
    name :: Prelude.Maybe Prelude.Text,
    -- | Severity of threat identified as part of the malware scan.
    severity :: Prelude.Maybe Prelude.Text
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
-- 'filePaths', 'scanThreatName_filePaths' - List of infected files in EBS volume with details.
--
-- 'itemCount', 'scanThreatName_itemCount' - Total number of files infected with given threat.
--
-- 'name', 'scanThreatName_name' - The name of the identified threat.
--
-- 'severity', 'scanThreatName_severity' - Severity of threat identified as part of the malware scan.
newScanThreatName ::
  ScanThreatName
newScanThreatName =
  ScanThreatName'
    { filePaths = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      name = Prelude.Nothing,
      severity = Prelude.Nothing
    }

-- | List of infected files in EBS volume with details.
scanThreatName_filePaths :: Lens.Lens' ScanThreatName (Prelude.Maybe [ScanFilePath])
scanThreatName_filePaths = Lens.lens (\ScanThreatName' {filePaths} -> filePaths) (\s@ScanThreatName' {} a -> s {filePaths = a} :: ScanThreatName) Prelude.. Lens.mapping Lens.coerced

-- | Total number of files infected with given threat.
scanThreatName_itemCount :: Lens.Lens' ScanThreatName (Prelude.Maybe Prelude.Int)
scanThreatName_itemCount = Lens.lens (\ScanThreatName' {itemCount} -> itemCount) (\s@ScanThreatName' {} a -> s {itemCount = a} :: ScanThreatName)

-- | The name of the identified threat.
scanThreatName_name :: Lens.Lens' ScanThreatName (Prelude.Maybe Prelude.Text)
scanThreatName_name = Lens.lens (\ScanThreatName' {name} -> name) (\s@ScanThreatName' {} a -> s {name = a} :: ScanThreatName)

-- | Severity of threat identified as part of the malware scan.
scanThreatName_severity :: Lens.Lens' ScanThreatName (Prelude.Maybe Prelude.Text)
scanThreatName_severity = Lens.lens (\ScanThreatName' {severity} -> severity) (\s@ScanThreatName' {} a -> s {severity = a} :: ScanThreatName)

instance Data.FromJSON ScanThreatName where
  parseJSON =
    Data.withObject
      "ScanThreatName"
      ( \x ->
          ScanThreatName'
            Prelude.<$> (x Data..:? "filePaths" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "itemCount")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "severity")
      )

instance Prelude.Hashable ScanThreatName where
  hashWithSalt _salt ScanThreatName' {..} =
    _salt `Prelude.hashWithSalt` filePaths
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` severity

instance Prelude.NFData ScanThreatName where
  rnf ScanThreatName' {..} =
    Prelude.rnf filePaths
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf severity
