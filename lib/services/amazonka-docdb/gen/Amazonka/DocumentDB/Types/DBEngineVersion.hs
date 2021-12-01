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
-- Module      : Amazonka.DocumentDB.Types.DBEngineVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.DBEngineVersion where

import qualified Amazonka.Core as Core
import Amazonka.DocumentDB.Types.UpgradeTarget
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about an engine version.
--
-- /See:/ 'newDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { -- | The version number of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine version.
    dbEngineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter group family for the database engine.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine.
    dbEngineDescription :: Prelude.Maybe Prelude.Text,
    -- | A list of engine versions that this database engine version can be
    -- upgraded to.
    validUpgradeTarget :: Prelude.Maybe [UpgradeTarget],
    -- | A value that indicates whether the engine version supports exporting the
    -- log types specified by @ExportableLogTypes@ to CloudWatch Logs.
    supportsLogExportsToCloudwatchLogs :: Prelude.Maybe Prelude.Bool,
    -- | The types of logs that the database engine has available for export to
    -- Amazon CloudWatch Logs.
    exportableLogTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBEngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'dbEngineVersion_engineVersion' - The version number of the database engine.
--
-- 'dbEngineVersionDescription', 'dbEngineVersion_dbEngineVersionDescription' - The description of the database engine version.
--
-- 'engine', 'dbEngineVersion_engine' - The name of the database engine.
--
-- 'dbParameterGroupFamily', 'dbEngineVersion_dbParameterGroupFamily' - The name of the parameter group family for the database engine.
--
-- 'dbEngineDescription', 'dbEngineVersion_dbEngineDescription' - The description of the database engine.
--
-- 'validUpgradeTarget', 'dbEngineVersion_validUpgradeTarget' - A list of engine versions that this database engine version can be
-- upgraded to.
--
-- 'supportsLogExportsToCloudwatchLogs', 'dbEngineVersion_supportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the
-- log types specified by @ExportableLogTypes@ to CloudWatch Logs.
--
-- 'exportableLogTypes', 'dbEngineVersion_exportableLogTypes' - The types of logs that the database engine has available for export to
-- Amazon CloudWatch Logs.
newDBEngineVersion ::
  DBEngineVersion
newDBEngineVersion =
  DBEngineVersion'
    { engineVersion = Prelude.Nothing,
      dbEngineVersionDescription = Prelude.Nothing,
      engine = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      dbEngineDescription = Prelude.Nothing,
      validUpgradeTarget = Prelude.Nothing,
      supportsLogExportsToCloudwatchLogs = Prelude.Nothing,
      exportableLogTypes = Prelude.Nothing
    }

-- | The version number of the database engine.
dbEngineVersion_engineVersion :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engineVersion = Lens.lens (\DBEngineVersion' {engineVersion} -> engineVersion) (\s@DBEngineVersion' {} a -> s {engineVersion = a} :: DBEngineVersion)

-- | The description of the database engine version.
dbEngineVersion_dbEngineVersionDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineVersionDescription = Lens.lens (\DBEngineVersion' {dbEngineVersionDescription} -> dbEngineVersionDescription) (\s@DBEngineVersion' {} a -> s {dbEngineVersionDescription = a} :: DBEngineVersion)

-- | The name of the database engine.
dbEngineVersion_engine :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engine = Lens.lens (\DBEngineVersion' {engine} -> engine) (\s@DBEngineVersion' {} a -> s {engine = a} :: DBEngineVersion)

-- | The name of the parameter group family for the database engine.
dbEngineVersion_dbParameterGroupFamily :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbParameterGroupFamily = Lens.lens (\DBEngineVersion' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBEngineVersion' {} a -> s {dbParameterGroupFamily = a} :: DBEngineVersion)

-- | The description of the database engine.
dbEngineVersion_dbEngineDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineDescription = Lens.lens (\DBEngineVersion' {dbEngineDescription} -> dbEngineDescription) (\s@DBEngineVersion' {} a -> s {dbEngineDescription = a} :: DBEngineVersion)

-- | A list of engine versions that this database engine version can be
-- upgraded to.
dbEngineVersion_validUpgradeTarget :: Lens.Lens' DBEngineVersion (Prelude.Maybe [UpgradeTarget])
dbEngineVersion_validUpgradeTarget = Lens.lens (\DBEngineVersion' {validUpgradeTarget} -> validUpgradeTarget) (\s@DBEngineVersion' {} a -> s {validUpgradeTarget = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the engine version supports exporting the
-- log types specified by @ExportableLogTypes@ to CloudWatch Logs.
dbEngineVersion_supportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsLogExportsToCloudwatchLogs = Lens.lens (\DBEngineVersion' {supportsLogExportsToCloudwatchLogs} -> supportsLogExportsToCloudwatchLogs) (\s@DBEngineVersion' {} a -> s {supportsLogExportsToCloudwatchLogs = a} :: DBEngineVersion)

-- | The types of logs that the database engine has available for export to
-- Amazon CloudWatch Logs.
dbEngineVersion_exportableLogTypes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_exportableLogTypes = Lens.lens (\DBEngineVersion' {exportableLogTypes} -> exportableLogTypes) (\s@DBEngineVersion' {} a -> s {exportableLogTypes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "DBEngineVersionDescription")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "DBParameterGroupFamily")
      Prelude.<*> (x Core..@? "DBEngineDescription")
      Prelude.<*> ( x Core..@? "ValidUpgradeTarget"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "UpgradeTarget")
                  )
      Prelude.<*> (x Core..@? "SupportsLogExportsToCloudwatchLogs")
      Prelude.<*> ( x Core..@? "ExportableLogTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable DBEngineVersion where
  hashWithSalt salt' DBEngineVersion' {..} =
    salt' `Prelude.hashWithSalt` exportableLogTypes
      `Prelude.hashWithSalt` supportsLogExportsToCloudwatchLogs
      `Prelude.hashWithSalt` validUpgradeTarget
      `Prelude.hashWithSalt` dbEngineDescription
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` dbEngineVersionDescription
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData DBEngineVersion where
  rnf DBEngineVersion' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf exportableLogTypes
      `Prelude.seq` Prelude.rnf supportsLogExportsToCloudwatchLogs
      `Prelude.seq` Prelude.rnf validUpgradeTarget
      `Prelude.seq` Prelude.rnf dbEngineDescription
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf dbEngineVersionDescription
