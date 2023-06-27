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
-- Module      : Amazonka.MGN.Types.ImportErrorData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportErrorData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Import error data.
--
-- /See:/ 'newImportErrorData' smart constructor.
data ImportErrorData = ImportErrorData'
  { -- | Import error data application ID.
    applicationID :: Prelude.Maybe Prelude.Text,
    -- | Import error data ec2 LaunchTemplate ID.
    ec2LaunchTemplateID :: Prelude.Maybe Prelude.Text,
    -- | Import error data raw error.
    rawError :: Prelude.Maybe Prelude.Text,
    -- | Import error data row number.
    rowNumber :: Prelude.Maybe Prelude.Natural,
    -- | Import error data source server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Import error data wave id.
    waveID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportErrorData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationID', 'importErrorData_applicationID' - Import error data application ID.
--
-- 'ec2LaunchTemplateID', 'importErrorData_ec2LaunchTemplateID' - Import error data ec2 LaunchTemplate ID.
--
-- 'rawError', 'importErrorData_rawError' - Import error data raw error.
--
-- 'rowNumber', 'importErrorData_rowNumber' - Import error data row number.
--
-- 'sourceServerID', 'importErrorData_sourceServerID' - Import error data source server ID.
--
-- 'waveID', 'importErrorData_waveID' - Import error data wave id.
newImportErrorData ::
  ImportErrorData
newImportErrorData =
  ImportErrorData'
    { applicationID = Prelude.Nothing,
      ec2LaunchTemplateID = Prelude.Nothing,
      rawError = Prelude.Nothing,
      rowNumber = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      waveID = Prelude.Nothing
    }

-- | Import error data application ID.
importErrorData_applicationID :: Lens.Lens' ImportErrorData (Prelude.Maybe Prelude.Text)
importErrorData_applicationID = Lens.lens (\ImportErrorData' {applicationID} -> applicationID) (\s@ImportErrorData' {} a -> s {applicationID = a} :: ImportErrorData)

-- | Import error data ec2 LaunchTemplate ID.
importErrorData_ec2LaunchTemplateID :: Lens.Lens' ImportErrorData (Prelude.Maybe Prelude.Text)
importErrorData_ec2LaunchTemplateID = Lens.lens (\ImportErrorData' {ec2LaunchTemplateID} -> ec2LaunchTemplateID) (\s@ImportErrorData' {} a -> s {ec2LaunchTemplateID = a} :: ImportErrorData)

-- | Import error data raw error.
importErrorData_rawError :: Lens.Lens' ImportErrorData (Prelude.Maybe Prelude.Text)
importErrorData_rawError = Lens.lens (\ImportErrorData' {rawError} -> rawError) (\s@ImportErrorData' {} a -> s {rawError = a} :: ImportErrorData)

-- | Import error data row number.
importErrorData_rowNumber :: Lens.Lens' ImportErrorData (Prelude.Maybe Prelude.Natural)
importErrorData_rowNumber = Lens.lens (\ImportErrorData' {rowNumber} -> rowNumber) (\s@ImportErrorData' {} a -> s {rowNumber = a} :: ImportErrorData)

-- | Import error data source server ID.
importErrorData_sourceServerID :: Lens.Lens' ImportErrorData (Prelude.Maybe Prelude.Text)
importErrorData_sourceServerID = Lens.lens (\ImportErrorData' {sourceServerID} -> sourceServerID) (\s@ImportErrorData' {} a -> s {sourceServerID = a} :: ImportErrorData)

-- | Import error data wave id.
importErrorData_waveID :: Lens.Lens' ImportErrorData (Prelude.Maybe Prelude.Text)
importErrorData_waveID = Lens.lens (\ImportErrorData' {waveID} -> waveID) (\s@ImportErrorData' {} a -> s {waveID = a} :: ImportErrorData)

instance Data.FromJSON ImportErrorData where
  parseJSON =
    Data.withObject
      "ImportErrorData"
      ( \x ->
          ImportErrorData'
            Prelude.<$> (x Data..:? "applicationID")
            Prelude.<*> (x Data..:? "ec2LaunchTemplateID")
            Prelude.<*> (x Data..:? "rawError")
            Prelude.<*> (x Data..:? "rowNumber")
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "waveID")
      )

instance Prelude.Hashable ImportErrorData where
  hashWithSalt _salt ImportErrorData' {..} =
    _salt
      `Prelude.hashWithSalt` applicationID
      `Prelude.hashWithSalt` ec2LaunchTemplateID
      `Prelude.hashWithSalt` rawError
      `Prelude.hashWithSalt` rowNumber
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` waveID

instance Prelude.NFData ImportErrorData where
  rnf ImportErrorData' {..} =
    Prelude.rnf applicationID
      `Prelude.seq` Prelude.rnf ec2LaunchTemplateID
      `Prelude.seq` Prelude.rnf rawError
      `Prelude.seq` Prelude.rnf rowNumber
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf waveID
