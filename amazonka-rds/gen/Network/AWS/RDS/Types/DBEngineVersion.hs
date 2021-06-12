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
-- Module      : Network.AWS.RDS.Types.DBEngineVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBEngineVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.CharacterSet
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget

-- | This data type is used as a response element in the action
-- @DescribeDBEngineVersions@.
--
-- /See:/ 'newDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { -- | The status of the DB engine version, either @available@ or @deprecated@.
    status :: Core.Maybe Core.Text,
    -- | The description of the database engine version.
    dbEngineVersionDescription :: Core.Maybe Core.Text,
    -- | The description of the database engine.
    dbEngineDescription :: Core.Maybe Core.Text,
    -- | A list of the time zones supported by this engine for the @Timezone@
    -- parameter of the @CreateDBInstance@ action.
    supportedTimezones :: Core.Maybe [Timezone],
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Core.Maybe [Core.Text],
    -- | A list of the character sets supported by the Oracle DB engine for the
    -- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedNcharCharacterSets :: Core.Maybe [CharacterSet],
    -- | A list of engine versions that this database engine version can be
    -- upgraded to.
    validUpgradeTarget :: Core.Maybe [UpgradeTarget],
    -- | The version number of the database engine.
    engineVersion :: Core.Maybe Core.Text,
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific DB engine version.
    supportsGlobalDatabases :: Core.Maybe Core.Bool,
    -- | The types of logs that the database engine has available for export to
    -- CloudWatch Logs.
    exportableLogTypes :: Core.Maybe [Core.Text],
    -- | A list of the character sets supported by this engine for the
    -- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedCharacterSets :: Core.Maybe [CharacterSet],
    -- | Indicates whether the database engine version supports read replicas.
    supportsReadReplica :: Core.Maybe Core.Bool,
    -- | The name of the DB parameter group family for the database engine.
    dbParameterGroupFamily :: Core.Maybe Core.Text,
    -- | The name of the database engine.
    engine :: Core.Maybe Core.Text,
    -- | A list of features supported by the DB engine. Supported feature names
    -- include the following.
    --
    -- -   s3Import
    supportedFeatureNames :: Core.Maybe [Core.Text],
    -- | A value that indicates whether the engine version supports exporting the
    -- log types specified by ExportableLogTypes to CloudWatch Logs.
    supportsLogExportsToCloudwatchLogs :: Core.Maybe Core.Bool,
    -- | The default character set for new instances of this engine version, if
    -- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
    -- specified.
    defaultCharacterSet :: Core.Maybe CharacterSet,
    -- | A value that indicates whether you can use Aurora parallel query with a
    -- specific DB engine version.
    supportsParallelQuery :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBEngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbEngineVersion_status' - The status of the DB engine version, either @available@ or @deprecated@.
--
-- 'dbEngineVersionDescription', 'dbEngineVersion_dbEngineVersionDescription' - The description of the database engine version.
--
-- 'dbEngineDescription', 'dbEngineVersion_dbEngineDescription' - The description of the database engine.
--
-- 'supportedTimezones', 'dbEngineVersion_supportedTimezones' - A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
--
-- 'supportedEngineModes', 'dbEngineVersion_supportedEngineModes' - A list of the supported DB engine modes.
--
-- 'supportedNcharCharacterSets', 'dbEngineVersion_supportedNcharCharacterSets' - A list of the character sets supported by the Oracle DB engine for the
-- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- 'validUpgradeTarget', 'dbEngineVersion_validUpgradeTarget' - A list of engine versions that this database engine version can be
-- upgraded to.
--
-- 'engineVersion', 'dbEngineVersion_engineVersion' - The version number of the database engine.
--
-- 'supportsGlobalDatabases', 'dbEngineVersion_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- a specific DB engine version.
--
-- 'exportableLogTypes', 'dbEngineVersion_exportableLogTypes' - The types of logs that the database engine has available for export to
-- CloudWatch Logs.
--
-- 'supportedCharacterSets', 'dbEngineVersion_supportedCharacterSets' - A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- 'supportsReadReplica', 'dbEngineVersion_supportsReadReplica' - Indicates whether the database engine version supports read replicas.
--
-- 'dbParameterGroupFamily', 'dbEngineVersion_dbParameterGroupFamily' - The name of the DB parameter group family for the database engine.
--
-- 'engine', 'dbEngineVersion_engine' - The name of the database engine.
--
-- 'supportedFeatureNames', 'dbEngineVersion_supportedFeatureNames' - A list of features supported by the DB engine. Supported feature names
-- include the following.
--
-- -   s3Import
--
-- 'supportsLogExportsToCloudwatchLogs', 'dbEngineVersion_supportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- 'defaultCharacterSet', 'dbEngineVersion_defaultCharacterSet' - The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
-- specified.
--
-- 'supportsParallelQuery', 'dbEngineVersion_supportsParallelQuery' - A value that indicates whether you can use Aurora parallel query with a
-- specific DB engine version.
newDBEngineVersion ::
  DBEngineVersion
newDBEngineVersion =
  DBEngineVersion'
    { status = Core.Nothing,
      dbEngineVersionDescription = Core.Nothing,
      dbEngineDescription = Core.Nothing,
      supportedTimezones = Core.Nothing,
      supportedEngineModes = Core.Nothing,
      supportedNcharCharacterSets = Core.Nothing,
      validUpgradeTarget = Core.Nothing,
      engineVersion = Core.Nothing,
      supportsGlobalDatabases = Core.Nothing,
      exportableLogTypes = Core.Nothing,
      supportedCharacterSets = Core.Nothing,
      supportsReadReplica = Core.Nothing,
      dbParameterGroupFamily = Core.Nothing,
      engine = Core.Nothing,
      supportedFeatureNames = Core.Nothing,
      supportsLogExportsToCloudwatchLogs = Core.Nothing,
      defaultCharacterSet = Core.Nothing,
      supportsParallelQuery = Core.Nothing
    }

-- | The status of the DB engine version, either @available@ or @deprecated@.
dbEngineVersion_status :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Text)
dbEngineVersion_status = Lens.lens (\DBEngineVersion' {status} -> status) (\s@DBEngineVersion' {} a -> s {status = a} :: DBEngineVersion)

-- | The description of the database engine version.
dbEngineVersion_dbEngineVersionDescription :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Text)
dbEngineVersion_dbEngineVersionDescription = Lens.lens (\DBEngineVersion' {dbEngineVersionDescription} -> dbEngineVersionDescription) (\s@DBEngineVersion' {} a -> s {dbEngineVersionDescription = a} :: DBEngineVersion)

-- | The description of the database engine.
dbEngineVersion_dbEngineDescription :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Text)
dbEngineVersion_dbEngineDescription = Lens.lens (\DBEngineVersion' {dbEngineDescription} -> dbEngineDescription) (\s@DBEngineVersion' {} a -> s {dbEngineDescription = a} :: DBEngineVersion)

-- | A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
dbEngineVersion_supportedTimezones :: Lens.Lens' DBEngineVersion (Core.Maybe [Timezone])
dbEngineVersion_supportedTimezones = Lens.lens (\DBEngineVersion' {supportedTimezones} -> supportedTimezones) (\s@DBEngineVersion' {} a -> s {supportedTimezones = a} :: DBEngineVersion) Core.. Lens.mapping Lens._Coerce

-- | A list of the supported DB engine modes.
dbEngineVersion_supportedEngineModes :: Lens.Lens' DBEngineVersion (Core.Maybe [Core.Text])
dbEngineVersion_supportedEngineModes = Lens.lens (\DBEngineVersion' {supportedEngineModes} -> supportedEngineModes) (\s@DBEngineVersion' {} a -> s {supportedEngineModes = a} :: DBEngineVersion) Core.. Lens.mapping Lens._Coerce

-- | A list of the character sets supported by the Oracle DB engine for the
-- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedNcharCharacterSets :: Lens.Lens' DBEngineVersion (Core.Maybe [CharacterSet])
dbEngineVersion_supportedNcharCharacterSets = Lens.lens (\DBEngineVersion' {supportedNcharCharacterSets} -> supportedNcharCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedNcharCharacterSets = a} :: DBEngineVersion) Core.. Lens.mapping Lens._Coerce

-- | A list of engine versions that this database engine version can be
-- upgraded to.
dbEngineVersion_validUpgradeTarget :: Lens.Lens' DBEngineVersion (Core.Maybe [UpgradeTarget])
dbEngineVersion_validUpgradeTarget = Lens.lens (\DBEngineVersion' {validUpgradeTarget} -> validUpgradeTarget) (\s@DBEngineVersion' {} a -> s {validUpgradeTarget = a} :: DBEngineVersion) Core.. Lens.mapping Lens._Coerce

-- | The version number of the database engine.
dbEngineVersion_engineVersion :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Text)
dbEngineVersion_engineVersion = Lens.lens (\DBEngineVersion' {engineVersion} -> engineVersion) (\s@DBEngineVersion' {} a -> s {engineVersion = a} :: DBEngineVersion)

-- | A value that indicates whether you can use Aurora global databases with
-- a specific DB engine version.
dbEngineVersion_supportsGlobalDatabases :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbEngineVersion_supportsGlobalDatabases = Lens.lens (\DBEngineVersion' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@DBEngineVersion' {} a -> s {supportsGlobalDatabases = a} :: DBEngineVersion)

-- | The types of logs that the database engine has available for export to
-- CloudWatch Logs.
dbEngineVersion_exportableLogTypes :: Lens.Lens' DBEngineVersion (Core.Maybe [Core.Text])
dbEngineVersion_exportableLogTypes = Lens.lens (\DBEngineVersion' {exportableLogTypes} -> exportableLogTypes) (\s@DBEngineVersion' {} a -> s {exportableLogTypes = a} :: DBEngineVersion) Core.. Lens.mapping Lens._Coerce

-- | A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedCharacterSets :: Lens.Lens' DBEngineVersion (Core.Maybe [CharacterSet])
dbEngineVersion_supportedCharacterSets = Lens.lens (\DBEngineVersion' {supportedCharacterSets} -> supportedCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedCharacterSets = a} :: DBEngineVersion) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the database engine version supports read replicas.
dbEngineVersion_supportsReadReplica :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbEngineVersion_supportsReadReplica = Lens.lens (\DBEngineVersion' {supportsReadReplica} -> supportsReadReplica) (\s@DBEngineVersion' {} a -> s {supportsReadReplica = a} :: DBEngineVersion)

-- | The name of the DB parameter group family for the database engine.
dbEngineVersion_dbParameterGroupFamily :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Text)
dbEngineVersion_dbParameterGroupFamily = Lens.lens (\DBEngineVersion' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBEngineVersion' {} a -> s {dbParameterGroupFamily = a} :: DBEngineVersion)

-- | The name of the database engine.
dbEngineVersion_engine :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Text)
dbEngineVersion_engine = Lens.lens (\DBEngineVersion' {engine} -> engine) (\s@DBEngineVersion' {} a -> s {engine = a} :: DBEngineVersion)

-- | A list of features supported by the DB engine. Supported feature names
-- include the following.
--
-- -   s3Import
dbEngineVersion_supportedFeatureNames :: Lens.Lens' DBEngineVersion (Core.Maybe [Core.Text])
dbEngineVersion_supportedFeatureNames = Lens.lens (\DBEngineVersion' {supportedFeatureNames} -> supportedFeatureNames) (\s@DBEngineVersion' {} a -> s {supportedFeatureNames = a} :: DBEngineVersion) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
dbEngineVersion_supportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbEngineVersion_supportsLogExportsToCloudwatchLogs = Lens.lens (\DBEngineVersion' {supportsLogExportsToCloudwatchLogs} -> supportsLogExportsToCloudwatchLogs) (\s@DBEngineVersion' {} a -> s {supportsLogExportsToCloudwatchLogs = a} :: DBEngineVersion)

-- | The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
-- specified.
dbEngineVersion_defaultCharacterSet :: Lens.Lens' DBEngineVersion (Core.Maybe CharacterSet)
dbEngineVersion_defaultCharacterSet = Lens.lens (\DBEngineVersion' {defaultCharacterSet} -> defaultCharacterSet) (\s@DBEngineVersion' {} a -> s {defaultCharacterSet = a} :: DBEngineVersion)

-- | A value that indicates whether you can use Aurora parallel query with a
-- specific DB engine version.
dbEngineVersion_supportsParallelQuery :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbEngineVersion_supportsParallelQuery = Lens.lens (\DBEngineVersion' {supportsParallelQuery} -> supportsParallelQuery) (\s@DBEngineVersion' {} a -> s {supportsParallelQuery = a} :: DBEngineVersion)

instance Core.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "DBEngineVersionDescription")
      Core.<*> (x Core..@? "DBEngineDescription")
      Core.<*> ( x Core..@? "SupportedTimezones" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Timezone")
               )
      Core.<*> ( x Core..@? "SupportedEngineModes"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "SupportedNcharCharacterSets"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CharacterSet")
               )
      Core.<*> ( x Core..@? "ValidUpgradeTarget" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "UpgradeTarget")
               )
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "SupportsGlobalDatabases")
      Core.<*> ( x Core..@? "ExportableLogTypes" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "SupportedCharacterSets"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CharacterSet")
               )
      Core.<*> (x Core..@? "SupportsReadReplica")
      Core.<*> (x Core..@? "DBParameterGroupFamily")
      Core.<*> (x Core..@? "Engine")
      Core.<*> ( x Core..@? "SupportedFeatureNames"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "SupportsLogExportsToCloudwatchLogs")
      Core.<*> (x Core..@? "DefaultCharacterSet")
      Core.<*> (x Core..@? "SupportsParallelQuery")

instance Core.Hashable DBEngineVersion

instance Core.NFData DBEngineVersion
