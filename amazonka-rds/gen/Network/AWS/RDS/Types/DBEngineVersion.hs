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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.CharacterSet
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget

-- | This data type is used as a response element in the action
-- @DescribeDBEngineVersions@.
--
-- /See:/ 'newDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { -- | The status of the DB engine version, either @available@ or @deprecated@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine version.
    dbEngineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine.
    dbEngineDescription :: Prelude.Maybe Prelude.Text,
    -- | A list of the time zones supported by this engine for the @Timezone@
    -- parameter of the @CreateDBInstance@ action.
    supportedTimezones :: Prelude.Maybe [Timezone],
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
    -- | A list of the character sets supported by the Oracle DB engine for the
    -- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedNcharCharacterSets :: Prelude.Maybe [CharacterSet],
    -- | A list of engine versions that this database engine version can be
    -- upgraded to.
    validUpgradeTarget :: Prelude.Maybe [UpgradeTarget],
    -- | The version number of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific DB engine version.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | The types of logs that the database engine has available for export to
    -- CloudWatch Logs.
    exportableLogTypes :: Prelude.Maybe [Prelude.Text],
    -- | A list of the character sets supported by this engine for the
    -- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedCharacterSets :: Prelude.Maybe [CharacterSet],
    -- | Indicates whether the database engine version supports read replicas.
    supportsReadReplica :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DB parameter group family for the database engine.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | A list of features supported by the DB engine. Supported feature names
    -- include the following.
    --
    -- -   s3Import
    supportedFeatureNames :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether the engine version supports exporting the
    -- log types specified by ExportableLogTypes to CloudWatch Logs.
    supportsLogExportsToCloudwatchLogs :: Prelude.Maybe Prelude.Bool,
    -- | The default character set for new instances of this engine version, if
    -- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
    -- specified.
    defaultCharacterSet :: Prelude.Maybe CharacterSet,
    -- | A value that indicates whether you can use Aurora parallel query with a
    -- specific DB engine version.
    supportsParallelQuery :: Prelude.Maybe Prelude.Bool
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
    { status = Prelude.Nothing,
      dbEngineVersionDescription = Prelude.Nothing,
      dbEngineDescription = Prelude.Nothing,
      supportedTimezones = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      supportedNcharCharacterSets = Prelude.Nothing,
      validUpgradeTarget = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      exportableLogTypes = Prelude.Nothing,
      supportedCharacterSets = Prelude.Nothing,
      supportsReadReplica = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      engine = Prelude.Nothing,
      supportedFeatureNames = Prelude.Nothing,
      supportsLogExportsToCloudwatchLogs = Prelude.Nothing,
      defaultCharacterSet = Prelude.Nothing,
      supportsParallelQuery = Prelude.Nothing
    }

-- | The status of the DB engine version, either @available@ or @deprecated@.
dbEngineVersion_status :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_status = Lens.lens (\DBEngineVersion' {status} -> status) (\s@DBEngineVersion' {} a -> s {status = a} :: DBEngineVersion)

-- | The description of the database engine version.
dbEngineVersion_dbEngineVersionDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineVersionDescription = Lens.lens (\DBEngineVersion' {dbEngineVersionDescription} -> dbEngineVersionDescription) (\s@DBEngineVersion' {} a -> s {dbEngineVersionDescription = a} :: DBEngineVersion)

-- | The description of the database engine.
dbEngineVersion_dbEngineDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineDescription = Lens.lens (\DBEngineVersion' {dbEngineDescription} -> dbEngineDescription) (\s@DBEngineVersion' {} a -> s {dbEngineDescription = a} :: DBEngineVersion)

-- | A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
dbEngineVersion_supportedTimezones :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Timezone])
dbEngineVersion_supportedTimezones = Lens.lens (\DBEngineVersion' {supportedTimezones} -> supportedTimezones) (\s@DBEngineVersion' {} a -> s {supportedTimezones = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the supported DB engine modes.
dbEngineVersion_supportedEngineModes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_supportedEngineModes = Lens.lens (\DBEngineVersion' {supportedEngineModes} -> supportedEngineModes) (\s@DBEngineVersion' {} a -> s {supportedEngineModes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the character sets supported by the Oracle DB engine for the
-- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedNcharCharacterSets :: Lens.Lens' DBEngineVersion (Prelude.Maybe [CharacterSet])
dbEngineVersion_supportedNcharCharacterSets = Lens.lens (\DBEngineVersion' {supportedNcharCharacterSets} -> supportedNcharCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedNcharCharacterSets = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens._Coerce

-- | A list of engine versions that this database engine version can be
-- upgraded to.
dbEngineVersion_validUpgradeTarget :: Lens.Lens' DBEngineVersion (Prelude.Maybe [UpgradeTarget])
dbEngineVersion_validUpgradeTarget = Lens.lens (\DBEngineVersion' {validUpgradeTarget} -> validUpgradeTarget) (\s@DBEngineVersion' {} a -> s {validUpgradeTarget = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens._Coerce

-- | The version number of the database engine.
dbEngineVersion_engineVersion :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engineVersion = Lens.lens (\DBEngineVersion' {engineVersion} -> engineVersion) (\s@DBEngineVersion' {} a -> s {engineVersion = a} :: DBEngineVersion)

-- | A value that indicates whether you can use Aurora global databases with
-- a specific DB engine version.
dbEngineVersion_supportsGlobalDatabases :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsGlobalDatabases = Lens.lens (\DBEngineVersion' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@DBEngineVersion' {} a -> s {supportsGlobalDatabases = a} :: DBEngineVersion)

-- | The types of logs that the database engine has available for export to
-- CloudWatch Logs.
dbEngineVersion_exportableLogTypes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_exportableLogTypes = Lens.lens (\DBEngineVersion' {exportableLogTypes} -> exportableLogTypes) (\s@DBEngineVersion' {} a -> s {exportableLogTypes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedCharacterSets :: Lens.Lens' DBEngineVersion (Prelude.Maybe [CharacterSet])
dbEngineVersion_supportedCharacterSets = Lens.lens (\DBEngineVersion' {supportedCharacterSets} -> supportedCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedCharacterSets = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates whether the database engine version supports read replicas.
dbEngineVersion_supportsReadReplica :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsReadReplica = Lens.lens (\DBEngineVersion' {supportsReadReplica} -> supportsReadReplica) (\s@DBEngineVersion' {} a -> s {supportsReadReplica = a} :: DBEngineVersion)

-- | The name of the DB parameter group family for the database engine.
dbEngineVersion_dbParameterGroupFamily :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbParameterGroupFamily = Lens.lens (\DBEngineVersion' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBEngineVersion' {} a -> s {dbParameterGroupFamily = a} :: DBEngineVersion)

-- | The name of the database engine.
dbEngineVersion_engine :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engine = Lens.lens (\DBEngineVersion' {engine} -> engine) (\s@DBEngineVersion' {} a -> s {engine = a} :: DBEngineVersion)

-- | A list of features supported by the DB engine. Supported feature names
-- include the following.
--
-- -   s3Import
dbEngineVersion_supportedFeatureNames :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_supportedFeatureNames = Lens.lens (\DBEngineVersion' {supportedFeatureNames} -> supportedFeatureNames) (\s@DBEngineVersion' {} a -> s {supportedFeatureNames = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
dbEngineVersion_supportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsLogExportsToCloudwatchLogs = Lens.lens (\DBEngineVersion' {supportsLogExportsToCloudwatchLogs} -> supportsLogExportsToCloudwatchLogs) (\s@DBEngineVersion' {} a -> s {supportsLogExportsToCloudwatchLogs = a} :: DBEngineVersion)

-- | The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
-- specified.
dbEngineVersion_defaultCharacterSet :: Lens.Lens' DBEngineVersion (Prelude.Maybe CharacterSet)
dbEngineVersion_defaultCharacterSet = Lens.lens (\DBEngineVersion' {defaultCharacterSet} -> defaultCharacterSet) (\s@DBEngineVersion' {} a -> s {defaultCharacterSet = a} :: DBEngineVersion)

-- | A value that indicates whether you can use Aurora parallel query with a
-- specific DB engine version.
dbEngineVersion_supportsParallelQuery :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsParallelQuery = Lens.lens (\DBEngineVersion' {supportsParallelQuery} -> supportsParallelQuery) (\s@DBEngineVersion' {} a -> s {supportsParallelQuery = a} :: DBEngineVersion)

instance Core.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "DBEngineVersionDescription")
      Prelude.<*> (x Core..@? "DBEngineDescription")
      Prelude.<*> ( x Core..@? "SupportedTimezones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Timezone")
                  )
      Prelude.<*> ( x Core..@? "SupportedEngineModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "SupportedNcharCharacterSets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "CharacterSet")
                  )
      Prelude.<*> ( x Core..@? "ValidUpgradeTarget"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "UpgradeTarget")
                  )
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "SupportsGlobalDatabases")
      Prelude.<*> ( x Core..@? "ExportableLogTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "SupportedCharacterSets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "CharacterSet")
                  )
      Prelude.<*> (x Core..@? "SupportsReadReplica")
      Prelude.<*> (x Core..@? "DBParameterGroupFamily")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> ( x Core..@? "SupportedFeatureNames"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "SupportsLogExportsToCloudwatchLogs")
      Prelude.<*> (x Core..@? "DefaultCharacterSet")
      Prelude.<*> (x Core..@? "SupportsParallelQuery")

instance Prelude.Hashable DBEngineVersion

instance Prelude.NFData DBEngineVersion
