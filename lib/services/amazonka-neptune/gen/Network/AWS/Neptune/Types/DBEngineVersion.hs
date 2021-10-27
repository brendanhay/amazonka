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
-- Module      : Network.AWS.Neptune.Types.DBEngineVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Neptune.Types.DBEngineVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Neptune.Types.CharacterSet
import Network.AWS.Neptune.Types.Timezone
import Network.AWS.Neptune.Types.UpgradeTarget
import qualified Network.AWS.Prelude as Prelude

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
--
-- /See:/ 'newDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { -- | The version number of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine version.
    dbEngineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    defaultCharacterSet :: Prelude.Maybe CharacterSet,
    -- | The name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group family for the database engine.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    supportedCharacterSets :: Prelude.Maybe [CharacterSet],
    -- | The description of the database engine.
    dbEngineDescription :: Prelude.Maybe Prelude.Text,
    -- | A list of engine versions that this database engine version can be
    -- upgraded to.
    validUpgradeTarget :: Prelude.Maybe [UpgradeTarget],
    -- | A value that indicates whether the engine version supports exporting the
    -- log types specified by ExportableLogTypes to CloudWatch Logs.
    supportsLogExportsToCloudwatchLogs :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the database engine version supports read replicas.
    supportsReadReplica :: Prelude.Maybe Prelude.Bool,
    -- | A list of the time zones supported by this engine for the @Timezone@
    -- parameter of the @CreateDBInstance@ action.
    supportedTimezones :: Prelude.Maybe [Timezone],
    -- | The types of logs that the database engine has available for export to
    -- CloudWatch Logs.
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
-- 'defaultCharacterSet', 'dbEngineVersion_defaultCharacterSet' - /(Not supported by Neptune)/
--
-- 'engine', 'dbEngineVersion_engine' - The name of the database engine.
--
-- 'dbParameterGroupFamily', 'dbEngineVersion_dbParameterGroupFamily' - The name of the DB parameter group family for the database engine.
--
-- 'supportedCharacterSets', 'dbEngineVersion_supportedCharacterSets' - /(Not supported by Neptune)/
--
-- 'dbEngineDescription', 'dbEngineVersion_dbEngineDescription' - The description of the database engine.
--
-- 'validUpgradeTarget', 'dbEngineVersion_validUpgradeTarget' - A list of engine versions that this database engine version can be
-- upgraded to.
--
-- 'supportsLogExportsToCloudwatchLogs', 'dbEngineVersion_supportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- 'supportsReadReplica', 'dbEngineVersion_supportsReadReplica' - Indicates whether the database engine version supports read replicas.
--
-- 'supportedTimezones', 'dbEngineVersion_supportedTimezones' - A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
--
-- 'exportableLogTypes', 'dbEngineVersion_exportableLogTypes' - The types of logs that the database engine has available for export to
-- CloudWatch Logs.
newDBEngineVersion ::
  DBEngineVersion
newDBEngineVersion =
  DBEngineVersion'
    { engineVersion = Prelude.Nothing,
      dbEngineVersionDescription = Prelude.Nothing,
      defaultCharacterSet = Prelude.Nothing,
      engine = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      supportedCharacterSets = Prelude.Nothing,
      dbEngineDescription = Prelude.Nothing,
      validUpgradeTarget = Prelude.Nothing,
      supportsLogExportsToCloudwatchLogs = Prelude.Nothing,
      supportsReadReplica = Prelude.Nothing,
      supportedTimezones = Prelude.Nothing,
      exportableLogTypes = Prelude.Nothing
    }

-- | The version number of the database engine.
dbEngineVersion_engineVersion :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engineVersion = Lens.lens (\DBEngineVersion' {engineVersion} -> engineVersion) (\s@DBEngineVersion' {} a -> s {engineVersion = a} :: DBEngineVersion)

-- | The description of the database engine version.
dbEngineVersion_dbEngineVersionDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineVersionDescription = Lens.lens (\DBEngineVersion' {dbEngineVersionDescription} -> dbEngineVersionDescription) (\s@DBEngineVersion' {} a -> s {dbEngineVersionDescription = a} :: DBEngineVersion)

-- | /(Not supported by Neptune)/
dbEngineVersion_defaultCharacterSet :: Lens.Lens' DBEngineVersion (Prelude.Maybe CharacterSet)
dbEngineVersion_defaultCharacterSet = Lens.lens (\DBEngineVersion' {defaultCharacterSet} -> defaultCharacterSet) (\s@DBEngineVersion' {} a -> s {defaultCharacterSet = a} :: DBEngineVersion)

-- | The name of the database engine.
dbEngineVersion_engine :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engine = Lens.lens (\DBEngineVersion' {engine} -> engine) (\s@DBEngineVersion' {} a -> s {engine = a} :: DBEngineVersion)

-- | The name of the DB parameter group family for the database engine.
dbEngineVersion_dbParameterGroupFamily :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbParameterGroupFamily = Lens.lens (\DBEngineVersion' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBEngineVersion' {} a -> s {dbParameterGroupFamily = a} :: DBEngineVersion)

-- | /(Not supported by Neptune)/
dbEngineVersion_supportedCharacterSets :: Lens.Lens' DBEngineVersion (Prelude.Maybe [CharacterSet])
dbEngineVersion_supportedCharacterSets = Lens.lens (\DBEngineVersion' {supportedCharacterSets} -> supportedCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedCharacterSets = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | The description of the database engine.
dbEngineVersion_dbEngineDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineDescription = Lens.lens (\DBEngineVersion' {dbEngineDescription} -> dbEngineDescription) (\s@DBEngineVersion' {} a -> s {dbEngineDescription = a} :: DBEngineVersion)

-- | A list of engine versions that this database engine version can be
-- upgraded to.
dbEngineVersion_validUpgradeTarget :: Lens.Lens' DBEngineVersion (Prelude.Maybe [UpgradeTarget])
dbEngineVersion_validUpgradeTarget = Lens.lens (\DBEngineVersion' {validUpgradeTarget} -> validUpgradeTarget) (\s@DBEngineVersion' {} a -> s {validUpgradeTarget = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
dbEngineVersion_supportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsLogExportsToCloudwatchLogs = Lens.lens (\DBEngineVersion' {supportsLogExportsToCloudwatchLogs} -> supportsLogExportsToCloudwatchLogs) (\s@DBEngineVersion' {} a -> s {supportsLogExportsToCloudwatchLogs = a} :: DBEngineVersion)

-- | Indicates whether the database engine version supports read replicas.
dbEngineVersion_supportsReadReplica :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsReadReplica = Lens.lens (\DBEngineVersion' {supportsReadReplica} -> supportsReadReplica) (\s@DBEngineVersion' {} a -> s {supportsReadReplica = a} :: DBEngineVersion)

-- | A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
dbEngineVersion_supportedTimezones :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Timezone])
dbEngineVersion_supportedTimezones = Lens.lens (\DBEngineVersion' {supportedTimezones} -> supportedTimezones) (\s@DBEngineVersion' {} a -> s {supportedTimezones = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | The types of logs that the database engine has available for export to
-- CloudWatch Logs.
dbEngineVersion_exportableLogTypes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_exportableLogTypes = Lens.lens (\DBEngineVersion' {exportableLogTypes} -> exportableLogTypes) (\s@DBEngineVersion' {} a -> s {exportableLogTypes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "DBEngineVersionDescription")
      Prelude.<*> (x Core..@? "DefaultCharacterSet")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "DBParameterGroupFamily")
      Prelude.<*> ( x Core..@? "SupportedCharacterSets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "CharacterSet")
                  )
      Prelude.<*> (x Core..@? "DBEngineDescription")
      Prelude.<*> ( x Core..@? "ValidUpgradeTarget"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "UpgradeTarget")
                  )
      Prelude.<*> (x Core..@? "SupportsLogExportsToCloudwatchLogs")
      Prelude.<*> (x Core..@? "SupportsReadReplica")
      Prelude.<*> ( x Core..@? "SupportedTimezones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Timezone")
                  )
      Prelude.<*> ( x Core..@? "ExportableLogTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable DBEngineVersion

instance Prelude.NFData DBEngineVersion
