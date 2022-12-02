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
-- Module      : Amazonka.RDS.Types.DBEngineVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBEngineVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.CharacterSet
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.Timezone
import Amazonka.RDS.Types.UpgradeTarget

-- | This data type is used as a response element in the action
-- @DescribeDBEngineVersions@.
--
-- /See:/ 'newDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { -- | A list of engine versions that this database engine version can be
    -- upgraded to.
    validUpgradeTarget :: Prelude.Maybe [UpgradeTarget],
    -- | The types of logs that the database engine has available for export to
    -- CloudWatch Logs.
    exportableLogTypes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the database engine version supports read replicas.
    supportsReadReplica :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the engine version supports Babelfish for
    -- Aurora PostgreSQL.
    supportsBabelfish :: Prelude.Maybe Prelude.Bool,
    -- | A list of the character sets supported by this engine for the
    -- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedCharacterSets :: Prelude.Maybe [CharacterSet],
    tagList :: Prelude.Maybe [Tag],
    -- | A list of features supported by the DB engine.
    --
    -- The supported features vary by DB engine and DB engine version.
    --
    -- To determine the supported features for a specific DB engine and DB
    -- engine version using the CLI, use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine \<engine_name> --engine-version \<engine_version>@
    --
    -- For example, to determine the supported features for RDS for PostgreSQL
    -- version 13.3 using the CLI, use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine postgres --engine-version 13.3@
    --
    -- The supported features are listed under @SupportedFeatureNames@ in the
    -- output.
    supportedFeatureNames :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the custom engine version.
    dbEngineVersionArn :: Prelude.Maybe Prelude.Text,
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
    -- | The default character set for new instances of this engine version, if
    -- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
    -- specified.
    defaultCharacterSet :: Prelude.Maybe CharacterSet,
    -- | The status of the DB engine version, either @available@ or @deprecated@.
    status :: Prelude.Maybe Prelude.Text,
    -- | JSON string that lists the installation files and parameters that RDS
    -- Custom uses to create a custom engine version (CEV). RDS Custom applies
    -- the patches in the order in which they\'re listed in the manifest. You
    -- can set the Oracle home, Oracle base, and UNIX\/Linux user and group
    -- using the installation parameters. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.preparing.html#custom-cev.preparing.manifest.fields JSON fields in the CEV manifest>
    -- in the /Amazon RDS User Guide/.
    customDBEngineVersionManifest :: Prelude.Maybe Prelude.Text,
    -- | The major engine version of the CEV.
    majorEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket that contains your database
    -- installation files.
    databaseInstallationFilesS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine version.
    dbEngineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether you can use Aurora parallel query with a
    -- specific DB engine version.
    supportsParallelQuery :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the engine version supports exporting the
    -- log types specified by ExportableLogTypes to CloudWatch Logs.
    supportsLogExportsToCloudwatchLogs :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key identifier for an encrypted CEV. This
    -- parameter is required for RDS Custom, but optional for Amazon RDS.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group family for the database engine.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 directory that contains the database installation files.
    -- If not specified, then no prefix is assumed.
    databaseInstallationFilesS3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the DB engine version.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | A list of the time zones supported by this engine for the @Timezone@
    -- parameter of the @CreateDBInstance@ action.
    supportedTimezones :: Prelude.Maybe [Timezone],
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific DB engine version.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | A list of the character sets supported by the Oracle DB engine for the
    -- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedNcharCharacterSets :: Prelude.Maybe [CharacterSet],
    -- | The version number of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine.
    dbEngineDescription :: Prelude.Maybe Prelude.Text
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
-- 'validUpgradeTarget', 'dbEngineVersion_validUpgradeTarget' - A list of engine versions that this database engine version can be
-- upgraded to.
--
-- 'exportableLogTypes', 'dbEngineVersion_exportableLogTypes' - The types of logs that the database engine has available for export to
-- CloudWatch Logs.
--
-- 'supportsReadReplica', 'dbEngineVersion_supportsReadReplica' - Indicates whether the database engine version supports read replicas.
--
-- 'supportsBabelfish', 'dbEngineVersion_supportsBabelfish' - A value that indicates whether the engine version supports Babelfish for
-- Aurora PostgreSQL.
--
-- 'supportedCharacterSets', 'dbEngineVersion_supportedCharacterSets' - A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- 'tagList', 'dbEngineVersion_tagList' - Undocumented member.
--
-- 'supportedFeatureNames', 'dbEngineVersion_supportedFeatureNames' - A list of features supported by the DB engine.
--
-- The supported features vary by DB engine and DB engine version.
--
-- To determine the supported features for a specific DB engine and DB
-- engine version using the CLI, use the following command:
--
-- @aws rds describe-db-engine-versions --engine \<engine_name> --engine-version \<engine_version>@
--
-- For example, to determine the supported features for RDS for PostgreSQL
-- version 13.3 using the CLI, use the following command:
--
-- @aws rds describe-db-engine-versions --engine postgres --engine-version 13.3@
--
-- The supported features are listed under @SupportedFeatureNames@ in the
-- output.
--
-- 'dbEngineVersionArn', 'dbEngineVersion_dbEngineVersionArn' - The ARN of the custom engine version.
--
-- 'supportedEngineModes', 'dbEngineVersion_supportedEngineModes' - A list of the supported DB engine modes.
--
-- 'defaultCharacterSet', 'dbEngineVersion_defaultCharacterSet' - The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
-- specified.
--
-- 'status', 'dbEngineVersion_status' - The status of the DB engine version, either @available@ or @deprecated@.
--
-- 'customDBEngineVersionManifest', 'dbEngineVersion_customDBEngineVersionManifest' - JSON string that lists the installation files and parameters that RDS
-- Custom uses to create a custom engine version (CEV). RDS Custom applies
-- the patches in the order in which they\'re listed in the manifest. You
-- can set the Oracle home, Oracle base, and UNIX\/Linux user and group
-- using the installation parameters. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.preparing.html#custom-cev.preparing.manifest.fields JSON fields in the CEV manifest>
-- in the /Amazon RDS User Guide/.
--
-- 'majorEngineVersion', 'dbEngineVersion_majorEngineVersion' - The major engine version of the CEV.
--
-- 'databaseInstallationFilesS3BucketName', 'dbEngineVersion_databaseInstallationFilesS3BucketName' - The name of the Amazon S3 bucket that contains your database
-- installation files.
--
-- 'dbEngineVersionDescription', 'dbEngineVersion_dbEngineVersionDescription' - The description of the database engine version.
--
-- 'supportsParallelQuery', 'dbEngineVersion_supportsParallelQuery' - A value that indicates whether you can use Aurora parallel query with a
-- specific DB engine version.
--
-- 'supportsLogExportsToCloudwatchLogs', 'dbEngineVersion_supportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- 'kmsKeyId', 'dbEngineVersion_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted CEV. This
-- parameter is required for RDS Custom, but optional for Amazon RDS.
--
-- 'engine', 'dbEngineVersion_engine' - The name of the database engine.
--
-- 'dbParameterGroupFamily', 'dbEngineVersion_dbParameterGroupFamily' - The name of the DB parameter group family for the database engine.
--
-- 'databaseInstallationFilesS3Prefix', 'dbEngineVersion_databaseInstallationFilesS3Prefix' - The Amazon S3 directory that contains the database installation files.
-- If not specified, then no prefix is assumed.
--
-- 'createTime', 'dbEngineVersion_createTime' - The creation time of the DB engine version.
--
-- 'supportedTimezones', 'dbEngineVersion_supportedTimezones' - A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
--
-- 'supportsGlobalDatabases', 'dbEngineVersion_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- a specific DB engine version.
--
-- 'supportedNcharCharacterSets', 'dbEngineVersion_supportedNcharCharacterSets' - A list of the character sets supported by the Oracle DB engine for the
-- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- 'engineVersion', 'dbEngineVersion_engineVersion' - The version number of the database engine.
--
-- 'dbEngineDescription', 'dbEngineVersion_dbEngineDescription' - The description of the database engine.
newDBEngineVersion ::
  DBEngineVersion
newDBEngineVersion =
  DBEngineVersion'
    { validUpgradeTarget =
        Prelude.Nothing,
      exportableLogTypes = Prelude.Nothing,
      supportsReadReplica = Prelude.Nothing,
      supportsBabelfish = Prelude.Nothing,
      supportedCharacterSets = Prelude.Nothing,
      tagList = Prelude.Nothing,
      supportedFeatureNames = Prelude.Nothing,
      dbEngineVersionArn = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      defaultCharacterSet = Prelude.Nothing,
      status = Prelude.Nothing,
      customDBEngineVersionManifest = Prelude.Nothing,
      majorEngineVersion = Prelude.Nothing,
      databaseInstallationFilesS3BucketName =
        Prelude.Nothing,
      dbEngineVersionDescription = Prelude.Nothing,
      supportsParallelQuery = Prelude.Nothing,
      supportsLogExportsToCloudwatchLogs = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engine = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      databaseInstallationFilesS3Prefix = Prelude.Nothing,
      createTime = Prelude.Nothing,
      supportedTimezones = Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      supportedNcharCharacterSets = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      dbEngineDescription = Prelude.Nothing
    }

-- | A list of engine versions that this database engine version can be
-- upgraded to.
dbEngineVersion_validUpgradeTarget :: Lens.Lens' DBEngineVersion (Prelude.Maybe [UpgradeTarget])
dbEngineVersion_validUpgradeTarget = Lens.lens (\DBEngineVersion' {validUpgradeTarget} -> validUpgradeTarget) (\s@DBEngineVersion' {} a -> s {validUpgradeTarget = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | The types of logs that the database engine has available for export to
-- CloudWatch Logs.
dbEngineVersion_exportableLogTypes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_exportableLogTypes = Lens.lens (\DBEngineVersion' {exportableLogTypes} -> exportableLogTypes) (\s@DBEngineVersion' {} a -> s {exportableLogTypes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the database engine version supports read replicas.
dbEngineVersion_supportsReadReplica :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsReadReplica = Lens.lens (\DBEngineVersion' {supportsReadReplica} -> supportsReadReplica) (\s@DBEngineVersion' {} a -> s {supportsReadReplica = a} :: DBEngineVersion)

-- | A value that indicates whether the engine version supports Babelfish for
-- Aurora PostgreSQL.
dbEngineVersion_supportsBabelfish :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsBabelfish = Lens.lens (\DBEngineVersion' {supportsBabelfish} -> supportsBabelfish) (\s@DBEngineVersion' {} a -> s {supportsBabelfish = a} :: DBEngineVersion)

-- | A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedCharacterSets :: Lens.Lens' DBEngineVersion (Prelude.Maybe [CharacterSet])
dbEngineVersion_supportedCharacterSets = Lens.lens (\DBEngineVersion' {supportedCharacterSets} -> supportedCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedCharacterSets = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
dbEngineVersion_tagList :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Tag])
dbEngineVersion_tagList = Lens.lens (\DBEngineVersion' {tagList} -> tagList) (\s@DBEngineVersion' {} a -> s {tagList = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of features supported by the DB engine.
--
-- The supported features vary by DB engine and DB engine version.
--
-- To determine the supported features for a specific DB engine and DB
-- engine version using the CLI, use the following command:
--
-- @aws rds describe-db-engine-versions --engine \<engine_name> --engine-version \<engine_version>@
--
-- For example, to determine the supported features for RDS for PostgreSQL
-- version 13.3 using the CLI, use the following command:
--
-- @aws rds describe-db-engine-versions --engine postgres --engine-version 13.3@
--
-- The supported features are listed under @SupportedFeatureNames@ in the
-- output.
dbEngineVersion_supportedFeatureNames :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_supportedFeatureNames = Lens.lens (\DBEngineVersion' {supportedFeatureNames} -> supportedFeatureNames) (\s@DBEngineVersion' {} a -> s {supportedFeatureNames = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the custom engine version.
dbEngineVersion_dbEngineVersionArn :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineVersionArn = Lens.lens (\DBEngineVersion' {dbEngineVersionArn} -> dbEngineVersionArn) (\s@DBEngineVersion' {} a -> s {dbEngineVersionArn = a} :: DBEngineVersion)

-- | A list of the supported DB engine modes.
dbEngineVersion_supportedEngineModes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_supportedEngineModes = Lens.lens (\DBEngineVersion' {supportedEngineModes} -> supportedEngineModes) (\s@DBEngineVersion' {} a -> s {supportedEngineModes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
-- specified.
dbEngineVersion_defaultCharacterSet :: Lens.Lens' DBEngineVersion (Prelude.Maybe CharacterSet)
dbEngineVersion_defaultCharacterSet = Lens.lens (\DBEngineVersion' {defaultCharacterSet} -> defaultCharacterSet) (\s@DBEngineVersion' {} a -> s {defaultCharacterSet = a} :: DBEngineVersion)

-- | The status of the DB engine version, either @available@ or @deprecated@.
dbEngineVersion_status :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_status = Lens.lens (\DBEngineVersion' {status} -> status) (\s@DBEngineVersion' {} a -> s {status = a} :: DBEngineVersion)

-- | JSON string that lists the installation files and parameters that RDS
-- Custom uses to create a custom engine version (CEV). RDS Custom applies
-- the patches in the order in which they\'re listed in the manifest. You
-- can set the Oracle home, Oracle base, and UNIX\/Linux user and group
-- using the installation parameters. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.preparing.html#custom-cev.preparing.manifest.fields JSON fields in the CEV manifest>
-- in the /Amazon RDS User Guide/.
dbEngineVersion_customDBEngineVersionManifest :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_customDBEngineVersionManifest = Lens.lens (\DBEngineVersion' {customDBEngineVersionManifest} -> customDBEngineVersionManifest) (\s@DBEngineVersion' {} a -> s {customDBEngineVersionManifest = a} :: DBEngineVersion)

-- | The major engine version of the CEV.
dbEngineVersion_majorEngineVersion :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_majorEngineVersion = Lens.lens (\DBEngineVersion' {majorEngineVersion} -> majorEngineVersion) (\s@DBEngineVersion' {} a -> s {majorEngineVersion = a} :: DBEngineVersion)

-- | The name of the Amazon S3 bucket that contains your database
-- installation files.
dbEngineVersion_databaseInstallationFilesS3BucketName :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_databaseInstallationFilesS3BucketName = Lens.lens (\DBEngineVersion' {databaseInstallationFilesS3BucketName} -> databaseInstallationFilesS3BucketName) (\s@DBEngineVersion' {} a -> s {databaseInstallationFilesS3BucketName = a} :: DBEngineVersion)

-- | The description of the database engine version.
dbEngineVersion_dbEngineVersionDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineVersionDescription = Lens.lens (\DBEngineVersion' {dbEngineVersionDescription} -> dbEngineVersionDescription) (\s@DBEngineVersion' {} a -> s {dbEngineVersionDescription = a} :: DBEngineVersion)

-- | A value that indicates whether you can use Aurora parallel query with a
-- specific DB engine version.
dbEngineVersion_supportsParallelQuery :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsParallelQuery = Lens.lens (\DBEngineVersion' {supportsParallelQuery} -> supportsParallelQuery) (\s@DBEngineVersion' {} a -> s {supportsParallelQuery = a} :: DBEngineVersion)

-- | A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
dbEngineVersion_supportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsLogExportsToCloudwatchLogs = Lens.lens (\DBEngineVersion' {supportsLogExportsToCloudwatchLogs} -> supportsLogExportsToCloudwatchLogs) (\s@DBEngineVersion' {} a -> s {supportsLogExportsToCloudwatchLogs = a} :: DBEngineVersion)

-- | The Amazon Web Services KMS key identifier for an encrypted CEV. This
-- parameter is required for RDS Custom, but optional for Amazon RDS.
dbEngineVersion_kmsKeyId :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_kmsKeyId = Lens.lens (\DBEngineVersion' {kmsKeyId} -> kmsKeyId) (\s@DBEngineVersion' {} a -> s {kmsKeyId = a} :: DBEngineVersion)

-- | The name of the database engine.
dbEngineVersion_engine :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engine = Lens.lens (\DBEngineVersion' {engine} -> engine) (\s@DBEngineVersion' {} a -> s {engine = a} :: DBEngineVersion)

-- | The name of the DB parameter group family for the database engine.
dbEngineVersion_dbParameterGroupFamily :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbParameterGroupFamily = Lens.lens (\DBEngineVersion' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBEngineVersion' {} a -> s {dbParameterGroupFamily = a} :: DBEngineVersion)

-- | The Amazon S3 directory that contains the database installation files.
-- If not specified, then no prefix is assumed.
dbEngineVersion_databaseInstallationFilesS3Prefix :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_databaseInstallationFilesS3Prefix = Lens.lens (\DBEngineVersion' {databaseInstallationFilesS3Prefix} -> databaseInstallationFilesS3Prefix) (\s@DBEngineVersion' {} a -> s {databaseInstallationFilesS3Prefix = a} :: DBEngineVersion)

-- | The creation time of the DB engine version.
dbEngineVersion_createTime :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.UTCTime)
dbEngineVersion_createTime = Lens.lens (\DBEngineVersion' {createTime} -> createTime) (\s@DBEngineVersion' {} a -> s {createTime = a} :: DBEngineVersion) Prelude.. Lens.mapping Data._Time

-- | A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
dbEngineVersion_supportedTimezones :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Timezone])
dbEngineVersion_supportedTimezones = Lens.lens (\DBEngineVersion' {supportedTimezones} -> supportedTimezones) (\s@DBEngineVersion' {} a -> s {supportedTimezones = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether you can use Aurora global databases with
-- a specific DB engine version.
dbEngineVersion_supportsGlobalDatabases :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsGlobalDatabases = Lens.lens (\DBEngineVersion' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@DBEngineVersion' {} a -> s {supportsGlobalDatabases = a} :: DBEngineVersion)

-- | A list of the character sets supported by the Oracle DB engine for the
-- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedNcharCharacterSets :: Lens.Lens' DBEngineVersion (Prelude.Maybe [CharacterSet])
dbEngineVersion_supportedNcharCharacterSets = Lens.lens (\DBEngineVersion' {supportedNcharCharacterSets} -> supportedNcharCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedNcharCharacterSets = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the database engine.
dbEngineVersion_engineVersion :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engineVersion = Lens.lens (\DBEngineVersion' {engineVersion} -> engineVersion) (\s@DBEngineVersion' {} a -> s {engineVersion = a} :: DBEngineVersion)

-- | The description of the database engine.
dbEngineVersion_dbEngineDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineDescription = Lens.lens (\DBEngineVersion' {dbEngineDescription} -> dbEngineDescription) (\s@DBEngineVersion' {} a -> s {dbEngineDescription = a} :: DBEngineVersion)

instance Data.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Prelude.<$> ( x Data..@? "ValidUpgradeTarget"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "UpgradeTarget")
                  )
      Prelude.<*> ( x Data..@? "ExportableLogTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "SupportsReadReplica")
      Prelude.<*> (x Data..@? "SupportsBabelfish")
      Prelude.<*> ( x Data..@? "SupportedCharacterSets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CharacterSet")
                  )
      Prelude.<*> ( x Data..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> ( x Data..@? "SupportedFeatureNames"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "DBEngineVersionArn")
      Prelude.<*> ( x Data..@? "SupportedEngineModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "DefaultCharacterSet")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "CustomDBEngineVersionManifest")
      Prelude.<*> (x Data..@? "MajorEngineVersion")
      Prelude.<*> (x Data..@? "DatabaseInstallationFilesS3BucketName")
      Prelude.<*> (x Data..@? "DBEngineVersionDescription")
      Prelude.<*> (x Data..@? "SupportsParallelQuery")
      Prelude.<*> (x Data..@? "SupportsLogExportsToCloudwatchLogs")
      Prelude.<*> (x Data..@? "KMSKeyId")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "DBParameterGroupFamily")
      Prelude.<*> (x Data..@? "DatabaseInstallationFilesS3Prefix")
      Prelude.<*> (x Data..@? "CreateTime")
      Prelude.<*> ( x Data..@? "SupportedTimezones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Timezone")
                  )
      Prelude.<*> (x Data..@? "SupportsGlobalDatabases")
      Prelude.<*> ( x Data..@? "SupportedNcharCharacterSets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CharacterSet")
                  )
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "DBEngineDescription")

instance Prelude.Hashable DBEngineVersion where
  hashWithSalt _salt DBEngineVersion' {..} =
    _salt `Prelude.hashWithSalt` validUpgradeTarget
      `Prelude.hashWithSalt` exportableLogTypes
      `Prelude.hashWithSalt` supportsReadReplica
      `Prelude.hashWithSalt` supportsBabelfish
      `Prelude.hashWithSalt` supportedCharacterSets
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` supportedFeatureNames
      `Prelude.hashWithSalt` dbEngineVersionArn
      `Prelude.hashWithSalt` supportedEngineModes
      `Prelude.hashWithSalt` defaultCharacterSet
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` customDBEngineVersionManifest
      `Prelude.hashWithSalt` majorEngineVersion
      `Prelude.hashWithSalt` databaseInstallationFilesS3BucketName
      `Prelude.hashWithSalt` dbEngineVersionDescription
      `Prelude.hashWithSalt` supportsParallelQuery
      `Prelude.hashWithSalt` supportsLogExportsToCloudwatchLogs
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` databaseInstallationFilesS3Prefix
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` supportedTimezones
      `Prelude.hashWithSalt` supportsGlobalDatabases
      `Prelude.hashWithSalt` supportedNcharCharacterSets
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` dbEngineDescription

instance Prelude.NFData DBEngineVersion where
  rnf DBEngineVersion' {..} =
    Prelude.rnf validUpgradeTarget
      `Prelude.seq` Prelude.rnf exportableLogTypes
      `Prelude.seq` Prelude.rnf supportsReadReplica
      `Prelude.seq` Prelude.rnf supportsBabelfish
      `Prelude.seq` Prelude.rnf supportedCharacterSets
      `Prelude.seq` Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf supportedFeatureNames
      `Prelude.seq` Prelude.rnf dbEngineVersionArn
      `Prelude.seq` Prelude.rnf supportedEngineModes
      `Prelude.seq` Prelude.rnf defaultCharacterSet
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf customDBEngineVersionManifest
      `Prelude.seq` Prelude.rnf majorEngineVersion
      `Prelude.seq` Prelude.rnf
        databaseInstallationFilesS3BucketName
      `Prelude.seq` Prelude.rnf dbEngineVersionDescription
      `Prelude.seq` Prelude.rnf supportsParallelQuery
      `Prelude.seq` Prelude.rnf
        supportsLogExportsToCloudwatchLogs
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf
        dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf
        databaseInstallationFilesS3Prefix
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf
        supportedTimezones
      `Prelude.seq` Prelude.rnf
        supportsGlobalDatabases
      `Prelude.seq` Prelude.rnf
        supportedNcharCharacterSets
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        dbEngineDescription
