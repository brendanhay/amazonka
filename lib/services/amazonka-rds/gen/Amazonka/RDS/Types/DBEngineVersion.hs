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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import Amazonka.RDS.Types.CustomDBEngineVersionAMI
import Amazonka.RDS.Types.Tag
import Amazonka.RDS.Types.Timezone
import Amazonka.RDS.Types.UpgradeTarget

-- | This data type is used as a response element in the action
-- @DescribeDBEngineVersions@.
--
-- /See:/ 'newDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { -- | The creation time of the DB engine version.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | JSON string that lists the installation files and parameters that RDS
    -- Custom uses to create a custom engine version (CEV). RDS Custom applies
    -- the patches in the order in which they\'re listed in the manifest. You
    -- can set the Oracle home, Oracle base, and UNIX\/Linux user and group
    -- using the installation parameters. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.preparing.html#custom-cev.preparing.manifest.fields JSON fields in the CEV manifest>
    -- in the /Amazon RDS User Guide/.
    customDBEngineVersionManifest :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine.
    dbEngineDescription :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates the source media provider of the AMI based on the
    -- usage operation. Applicable for RDS Custom for SQL Server.
    dbEngineMediaType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the custom engine version.
    dbEngineVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine version.
    dbEngineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group family for the database engine.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket that contains your database
    -- installation files.
    databaseInstallationFilesS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 directory that contains the database installation files.
    -- If not specified, then no prefix is assumed.
    databaseInstallationFilesS3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The default character set for new instances of this engine version, if
    -- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
    -- specified.
    defaultCharacterSet :: Prelude.Maybe CharacterSet,
    -- | The name of the database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The version number of the database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The types of logs that the database engine has available for export to
    -- CloudWatch Logs.
    exportableLogTypes :: Prelude.Maybe [Prelude.Text],
    -- | The EC2 image
    image :: Prelude.Maybe CustomDBEngineVersionAMI,
    -- | The Amazon Web Services KMS key identifier for an encrypted CEV. This
    -- parameter is required for RDS Custom, but optional for Amazon RDS.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The major engine version of the CEV.
    majorEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of the DB engine version, either @available@ or @deprecated@.
    status :: Prelude.Maybe Prelude.Text,
    -- | A list of the supported CA certificate identifiers.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
    -- in the /Amazon Aurora User Guide/.
    supportedCACertificateIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | A list of the character sets supported by this engine for the
    -- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedCharacterSets :: Prelude.Maybe [CharacterSet],
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
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
    -- | A list of the character sets supported by the Oracle DB engine for the
    -- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedNcharCharacterSets :: Prelude.Maybe [CharacterSet],
    -- | A list of the time zones supported by this engine for the @Timezone@
    -- parameter of the @CreateDBInstance@ action.
    supportedTimezones :: Prelude.Maybe [Timezone],
    -- | A value that indicates whether the engine version supports Babelfish for
    -- Aurora PostgreSQL.
    supportsBabelfish :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the engine version supports rotating the
    -- server certificate without rebooting the DB instance.
    supportsCertificateRotationWithoutRestart :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether you can use Aurora global databases with
    -- a specific DB engine version.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the engine version supports exporting the
    -- log types specified by ExportableLogTypes to CloudWatch Logs.
    supportsLogExportsToCloudwatchLogs :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether you can use Aurora parallel query with a
    -- specific DB engine version.
    supportsParallelQuery :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the database engine version supports read replicas.
    supportsReadReplica :: Prelude.Maybe Prelude.Bool,
    tagList :: Prelude.Maybe [Tag],
    -- | A list of engine versions that this database engine version can be
    -- upgraded to.
    validUpgradeTarget :: Prelude.Maybe [UpgradeTarget]
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
-- 'createTime', 'dbEngineVersion_createTime' - The creation time of the DB engine version.
--
-- 'customDBEngineVersionManifest', 'dbEngineVersion_customDBEngineVersionManifest' - JSON string that lists the installation files and parameters that RDS
-- Custom uses to create a custom engine version (CEV). RDS Custom applies
-- the patches in the order in which they\'re listed in the manifest. You
-- can set the Oracle home, Oracle base, and UNIX\/Linux user and group
-- using the installation parameters. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.preparing.html#custom-cev.preparing.manifest.fields JSON fields in the CEV manifest>
-- in the /Amazon RDS User Guide/.
--
-- 'dbEngineDescription', 'dbEngineVersion_dbEngineDescription' - The description of the database engine.
--
-- 'dbEngineMediaType', 'dbEngineVersion_dbEngineMediaType' - A value that indicates the source media provider of the AMI based on the
-- usage operation. Applicable for RDS Custom for SQL Server.
--
-- 'dbEngineVersionArn', 'dbEngineVersion_dbEngineVersionArn' - The ARN of the custom engine version.
--
-- 'dbEngineVersionDescription', 'dbEngineVersion_dbEngineVersionDescription' - The description of the database engine version.
--
-- 'dbParameterGroupFamily', 'dbEngineVersion_dbParameterGroupFamily' - The name of the DB parameter group family for the database engine.
--
-- 'databaseInstallationFilesS3BucketName', 'dbEngineVersion_databaseInstallationFilesS3BucketName' - The name of the Amazon S3 bucket that contains your database
-- installation files.
--
-- 'databaseInstallationFilesS3Prefix', 'dbEngineVersion_databaseInstallationFilesS3Prefix' - The Amazon S3 directory that contains the database installation files.
-- If not specified, then no prefix is assumed.
--
-- 'defaultCharacterSet', 'dbEngineVersion_defaultCharacterSet' - The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
-- specified.
--
-- 'engine', 'dbEngineVersion_engine' - The name of the database engine.
--
-- 'engineVersion', 'dbEngineVersion_engineVersion' - The version number of the database engine.
--
-- 'exportableLogTypes', 'dbEngineVersion_exportableLogTypes' - The types of logs that the database engine has available for export to
-- CloudWatch Logs.
--
-- 'image', 'dbEngineVersion_image' - The EC2 image
--
-- 'kmsKeyId', 'dbEngineVersion_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted CEV. This
-- parameter is required for RDS Custom, but optional for Amazon RDS.
--
-- 'majorEngineVersion', 'dbEngineVersion_majorEngineVersion' - The major engine version of the CEV.
--
-- 'status', 'dbEngineVersion_status' - The status of the DB engine version, either @available@ or @deprecated@.
--
-- 'supportedCACertificateIdentifiers', 'dbEngineVersion_supportedCACertificateIdentifiers' - A list of the supported CA certificate identifiers.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'supportedCharacterSets', 'dbEngineVersion_supportedCharacterSets' - A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- 'supportedEngineModes', 'dbEngineVersion_supportedEngineModes' - A list of the supported DB engine modes.
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
-- 'supportedNcharCharacterSets', 'dbEngineVersion_supportedNcharCharacterSets' - A list of the character sets supported by the Oracle DB engine for the
-- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- 'supportedTimezones', 'dbEngineVersion_supportedTimezones' - A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
--
-- 'supportsBabelfish', 'dbEngineVersion_supportsBabelfish' - A value that indicates whether the engine version supports Babelfish for
-- Aurora PostgreSQL.
--
-- 'supportsCertificateRotationWithoutRestart', 'dbEngineVersion_supportsCertificateRotationWithoutRestart' - A value that indicates whether the engine version supports rotating the
-- server certificate without rebooting the DB instance.
--
-- 'supportsGlobalDatabases', 'dbEngineVersion_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- a specific DB engine version.
--
-- 'supportsLogExportsToCloudwatchLogs', 'dbEngineVersion_supportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- 'supportsParallelQuery', 'dbEngineVersion_supportsParallelQuery' - A value that indicates whether you can use Aurora parallel query with a
-- specific DB engine version.
--
-- 'supportsReadReplica', 'dbEngineVersion_supportsReadReplica' - Indicates whether the database engine version supports read replicas.
--
-- 'tagList', 'dbEngineVersion_tagList' - Undocumented member.
--
-- 'validUpgradeTarget', 'dbEngineVersion_validUpgradeTarget' - A list of engine versions that this database engine version can be
-- upgraded to.
newDBEngineVersion ::
  DBEngineVersion
newDBEngineVersion =
  DBEngineVersion'
    { createTime = Prelude.Nothing,
      customDBEngineVersionManifest = Prelude.Nothing,
      dbEngineDescription = Prelude.Nothing,
      dbEngineMediaType = Prelude.Nothing,
      dbEngineVersionArn = Prelude.Nothing,
      dbEngineVersionDescription = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      databaseInstallationFilesS3BucketName =
        Prelude.Nothing,
      databaseInstallationFilesS3Prefix = Prelude.Nothing,
      defaultCharacterSet = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      exportableLogTypes = Prelude.Nothing,
      image = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      majorEngineVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      supportedCACertificateIdentifiers = Prelude.Nothing,
      supportedCharacterSets = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      supportedFeatureNames = Prelude.Nothing,
      supportedNcharCharacterSets = Prelude.Nothing,
      supportedTimezones = Prelude.Nothing,
      supportsBabelfish = Prelude.Nothing,
      supportsCertificateRotationWithoutRestart =
        Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      supportsLogExportsToCloudwatchLogs = Prelude.Nothing,
      supportsParallelQuery = Prelude.Nothing,
      supportsReadReplica = Prelude.Nothing,
      tagList = Prelude.Nothing,
      validUpgradeTarget = Prelude.Nothing
    }

-- | The creation time of the DB engine version.
dbEngineVersion_createTime :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.UTCTime)
dbEngineVersion_createTime = Lens.lens (\DBEngineVersion' {createTime} -> createTime) (\s@DBEngineVersion' {} a -> s {createTime = a} :: DBEngineVersion) Prelude.. Lens.mapping Data._Time

-- | JSON string that lists the installation files and parameters that RDS
-- Custom uses to create a custom engine version (CEV). RDS Custom applies
-- the patches in the order in which they\'re listed in the manifest. You
-- can set the Oracle home, Oracle base, and UNIX\/Linux user and group
-- using the installation parameters. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.preparing.html#custom-cev.preparing.manifest.fields JSON fields in the CEV manifest>
-- in the /Amazon RDS User Guide/.
dbEngineVersion_customDBEngineVersionManifest :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_customDBEngineVersionManifest = Lens.lens (\DBEngineVersion' {customDBEngineVersionManifest} -> customDBEngineVersionManifest) (\s@DBEngineVersion' {} a -> s {customDBEngineVersionManifest = a} :: DBEngineVersion)

-- | The description of the database engine.
dbEngineVersion_dbEngineDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineDescription = Lens.lens (\DBEngineVersion' {dbEngineDescription} -> dbEngineDescription) (\s@DBEngineVersion' {} a -> s {dbEngineDescription = a} :: DBEngineVersion)

-- | A value that indicates the source media provider of the AMI based on the
-- usage operation. Applicable for RDS Custom for SQL Server.
dbEngineVersion_dbEngineMediaType :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineMediaType = Lens.lens (\DBEngineVersion' {dbEngineMediaType} -> dbEngineMediaType) (\s@DBEngineVersion' {} a -> s {dbEngineMediaType = a} :: DBEngineVersion)

-- | The ARN of the custom engine version.
dbEngineVersion_dbEngineVersionArn :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineVersionArn = Lens.lens (\DBEngineVersion' {dbEngineVersionArn} -> dbEngineVersionArn) (\s@DBEngineVersion' {} a -> s {dbEngineVersionArn = a} :: DBEngineVersion)

-- | The description of the database engine version.
dbEngineVersion_dbEngineVersionDescription :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbEngineVersionDescription = Lens.lens (\DBEngineVersion' {dbEngineVersionDescription} -> dbEngineVersionDescription) (\s@DBEngineVersion' {} a -> s {dbEngineVersionDescription = a} :: DBEngineVersion)

-- | The name of the DB parameter group family for the database engine.
dbEngineVersion_dbParameterGroupFamily :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_dbParameterGroupFamily = Lens.lens (\DBEngineVersion' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DBEngineVersion' {} a -> s {dbParameterGroupFamily = a} :: DBEngineVersion)

-- | The name of the Amazon S3 bucket that contains your database
-- installation files.
dbEngineVersion_databaseInstallationFilesS3BucketName :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_databaseInstallationFilesS3BucketName = Lens.lens (\DBEngineVersion' {databaseInstallationFilesS3BucketName} -> databaseInstallationFilesS3BucketName) (\s@DBEngineVersion' {} a -> s {databaseInstallationFilesS3BucketName = a} :: DBEngineVersion)

-- | The Amazon S3 directory that contains the database installation files.
-- If not specified, then no prefix is assumed.
dbEngineVersion_databaseInstallationFilesS3Prefix :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_databaseInstallationFilesS3Prefix = Lens.lens (\DBEngineVersion' {databaseInstallationFilesS3Prefix} -> databaseInstallationFilesS3Prefix) (\s@DBEngineVersion' {} a -> s {databaseInstallationFilesS3Prefix = a} :: DBEngineVersion)

-- | The default character set for new instances of this engine version, if
-- the @CharacterSetName@ parameter of the CreateDBInstance API isn\'t
-- specified.
dbEngineVersion_defaultCharacterSet :: Lens.Lens' DBEngineVersion (Prelude.Maybe CharacterSet)
dbEngineVersion_defaultCharacterSet = Lens.lens (\DBEngineVersion' {defaultCharacterSet} -> defaultCharacterSet) (\s@DBEngineVersion' {} a -> s {defaultCharacterSet = a} :: DBEngineVersion)

-- | The name of the database engine.
dbEngineVersion_engine :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engine = Lens.lens (\DBEngineVersion' {engine} -> engine) (\s@DBEngineVersion' {} a -> s {engine = a} :: DBEngineVersion)

-- | The version number of the database engine.
dbEngineVersion_engineVersion :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_engineVersion = Lens.lens (\DBEngineVersion' {engineVersion} -> engineVersion) (\s@DBEngineVersion' {} a -> s {engineVersion = a} :: DBEngineVersion)

-- | The types of logs that the database engine has available for export to
-- CloudWatch Logs.
dbEngineVersion_exportableLogTypes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_exportableLogTypes = Lens.lens (\DBEngineVersion' {exportableLogTypes} -> exportableLogTypes) (\s@DBEngineVersion' {} a -> s {exportableLogTypes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 image
dbEngineVersion_image :: Lens.Lens' DBEngineVersion (Prelude.Maybe CustomDBEngineVersionAMI)
dbEngineVersion_image = Lens.lens (\DBEngineVersion' {image} -> image) (\s@DBEngineVersion' {} a -> s {image = a} :: DBEngineVersion)

-- | The Amazon Web Services KMS key identifier for an encrypted CEV. This
-- parameter is required for RDS Custom, but optional for Amazon RDS.
dbEngineVersion_kmsKeyId :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_kmsKeyId = Lens.lens (\DBEngineVersion' {kmsKeyId} -> kmsKeyId) (\s@DBEngineVersion' {} a -> s {kmsKeyId = a} :: DBEngineVersion)

-- | The major engine version of the CEV.
dbEngineVersion_majorEngineVersion :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_majorEngineVersion = Lens.lens (\DBEngineVersion' {majorEngineVersion} -> majorEngineVersion) (\s@DBEngineVersion' {} a -> s {majorEngineVersion = a} :: DBEngineVersion)

-- | The status of the DB engine version, either @available@ or @deprecated@.
dbEngineVersion_status :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Text)
dbEngineVersion_status = Lens.lens (\DBEngineVersion' {status} -> status) (\s@DBEngineVersion' {} a -> s {status = a} :: DBEngineVersion)

-- | A list of the supported CA certificate identifiers.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
dbEngineVersion_supportedCACertificateIdentifiers :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_supportedCACertificateIdentifiers = Lens.lens (\DBEngineVersion' {supportedCACertificateIdentifiers} -> supportedCACertificateIdentifiers) (\s@DBEngineVersion' {} a -> s {supportedCACertificateIdentifiers = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of the character sets supported by this engine for the
-- @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedCharacterSets :: Lens.Lens' DBEngineVersion (Prelude.Maybe [CharacterSet])
dbEngineVersion_supportedCharacterSets = Lens.lens (\DBEngineVersion' {supportedCharacterSets} -> supportedCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedCharacterSets = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of the supported DB engine modes.
dbEngineVersion_supportedEngineModes :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Prelude.Text])
dbEngineVersion_supportedEngineModes = Lens.lens (\DBEngineVersion' {supportedEngineModes} -> supportedEngineModes) (\s@DBEngineVersion' {} a -> s {supportedEngineModes = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

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

-- | A list of the character sets supported by the Oracle DB engine for the
-- @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
dbEngineVersion_supportedNcharCharacterSets :: Lens.Lens' DBEngineVersion (Prelude.Maybe [CharacterSet])
dbEngineVersion_supportedNcharCharacterSets = Lens.lens (\DBEngineVersion' {supportedNcharCharacterSets} -> supportedNcharCharacterSets) (\s@DBEngineVersion' {} a -> s {supportedNcharCharacterSets = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of the time zones supported by this engine for the @Timezone@
-- parameter of the @CreateDBInstance@ action.
dbEngineVersion_supportedTimezones :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Timezone])
dbEngineVersion_supportedTimezones = Lens.lens (\DBEngineVersion' {supportedTimezones} -> supportedTimezones) (\s@DBEngineVersion' {} a -> s {supportedTimezones = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the engine version supports Babelfish for
-- Aurora PostgreSQL.
dbEngineVersion_supportsBabelfish :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsBabelfish = Lens.lens (\DBEngineVersion' {supportsBabelfish} -> supportsBabelfish) (\s@DBEngineVersion' {} a -> s {supportsBabelfish = a} :: DBEngineVersion)

-- | A value that indicates whether the engine version supports rotating the
-- server certificate without rebooting the DB instance.
dbEngineVersion_supportsCertificateRotationWithoutRestart :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsCertificateRotationWithoutRestart = Lens.lens (\DBEngineVersion' {supportsCertificateRotationWithoutRestart} -> supportsCertificateRotationWithoutRestart) (\s@DBEngineVersion' {} a -> s {supportsCertificateRotationWithoutRestart = a} :: DBEngineVersion)

-- | A value that indicates whether you can use Aurora global databases with
-- a specific DB engine version.
dbEngineVersion_supportsGlobalDatabases :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsGlobalDatabases = Lens.lens (\DBEngineVersion' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@DBEngineVersion' {} a -> s {supportsGlobalDatabases = a} :: DBEngineVersion)

-- | A value that indicates whether the engine version supports exporting the
-- log types specified by ExportableLogTypes to CloudWatch Logs.
dbEngineVersion_supportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsLogExportsToCloudwatchLogs = Lens.lens (\DBEngineVersion' {supportsLogExportsToCloudwatchLogs} -> supportsLogExportsToCloudwatchLogs) (\s@DBEngineVersion' {} a -> s {supportsLogExportsToCloudwatchLogs = a} :: DBEngineVersion)

-- | A value that indicates whether you can use Aurora parallel query with a
-- specific DB engine version.
dbEngineVersion_supportsParallelQuery :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsParallelQuery = Lens.lens (\DBEngineVersion' {supportsParallelQuery} -> supportsParallelQuery) (\s@DBEngineVersion' {} a -> s {supportsParallelQuery = a} :: DBEngineVersion)

-- | Indicates whether the database engine version supports read replicas.
dbEngineVersion_supportsReadReplica :: Lens.Lens' DBEngineVersion (Prelude.Maybe Prelude.Bool)
dbEngineVersion_supportsReadReplica = Lens.lens (\DBEngineVersion' {supportsReadReplica} -> supportsReadReplica) (\s@DBEngineVersion' {} a -> s {supportsReadReplica = a} :: DBEngineVersion)

-- | Undocumented member.
dbEngineVersion_tagList :: Lens.Lens' DBEngineVersion (Prelude.Maybe [Tag])
dbEngineVersion_tagList = Lens.lens (\DBEngineVersion' {tagList} -> tagList) (\s@DBEngineVersion' {} a -> s {tagList = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of engine versions that this database engine version can be
-- upgraded to.
dbEngineVersion_validUpgradeTarget :: Lens.Lens' DBEngineVersion (Prelude.Maybe [UpgradeTarget])
dbEngineVersion_validUpgradeTarget = Lens.lens (\DBEngineVersion' {validUpgradeTarget} -> validUpgradeTarget) (\s@DBEngineVersion' {} a -> s {validUpgradeTarget = a} :: DBEngineVersion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Prelude.<$> (x Data..@? "CreateTime")
      Prelude.<*> (x Data..@? "CustomDBEngineVersionManifest")
      Prelude.<*> (x Data..@? "DBEngineDescription")
      Prelude.<*> (x Data..@? "DBEngineMediaType")
      Prelude.<*> (x Data..@? "DBEngineVersionArn")
      Prelude.<*> (x Data..@? "DBEngineVersionDescription")
      Prelude.<*> (x Data..@? "DBParameterGroupFamily")
      Prelude.<*> (x Data..@? "DatabaseInstallationFilesS3BucketName")
      Prelude.<*> (x Data..@? "DatabaseInstallationFilesS3Prefix")
      Prelude.<*> (x Data..@? "DefaultCharacterSet")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> ( x
                      Data..@? "ExportableLogTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Image")
      Prelude.<*> (x Data..@? "KMSKeyId")
      Prelude.<*> (x Data..@? "MajorEngineVersion")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> ( x
                      Data..@? "SupportedCACertificateIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedCharacterSets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CharacterSet")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedEngineModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedFeatureNames"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedNcharCharacterSets"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CharacterSet")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedTimezones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Timezone")
                  )
      Prelude.<*> (x Data..@? "SupportsBabelfish")
      Prelude.<*> ( x
                      Data..@? "SupportsCertificateRotationWithoutRestart"
                  )
      Prelude.<*> (x Data..@? "SupportsGlobalDatabases")
      Prelude.<*> (x Data..@? "SupportsLogExportsToCloudwatchLogs")
      Prelude.<*> (x Data..@? "SupportsParallelQuery")
      Prelude.<*> (x Data..@? "SupportsReadReplica")
      Prelude.<*> ( x
                      Data..@? "TagList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )
      Prelude.<*> ( x
                      Data..@? "ValidUpgradeTarget"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "UpgradeTarget")
                  )

instance Prelude.Hashable DBEngineVersion where
  hashWithSalt _salt DBEngineVersion' {..} =
    _salt
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` customDBEngineVersionManifest
      `Prelude.hashWithSalt` dbEngineDescription
      `Prelude.hashWithSalt` dbEngineMediaType
      `Prelude.hashWithSalt` dbEngineVersionArn
      `Prelude.hashWithSalt` dbEngineVersionDescription
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` databaseInstallationFilesS3BucketName
      `Prelude.hashWithSalt` databaseInstallationFilesS3Prefix
      `Prelude.hashWithSalt` defaultCharacterSet
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` exportableLogTypes
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` majorEngineVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` supportedCACertificateIdentifiers
      `Prelude.hashWithSalt` supportedCharacterSets
      `Prelude.hashWithSalt` supportedEngineModes
      `Prelude.hashWithSalt` supportedFeatureNames
      `Prelude.hashWithSalt` supportedNcharCharacterSets
      `Prelude.hashWithSalt` supportedTimezones
      `Prelude.hashWithSalt` supportsBabelfish
      `Prelude.hashWithSalt` supportsCertificateRotationWithoutRestart
      `Prelude.hashWithSalt` supportsGlobalDatabases
      `Prelude.hashWithSalt` supportsLogExportsToCloudwatchLogs
      `Prelude.hashWithSalt` supportsParallelQuery
      `Prelude.hashWithSalt` supportsReadReplica
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` validUpgradeTarget

instance Prelude.NFData DBEngineVersion where
  rnf DBEngineVersion' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf customDBEngineVersionManifest
      `Prelude.seq` Prelude.rnf dbEngineDescription
      `Prelude.seq` Prelude.rnf dbEngineMediaType
      `Prelude.seq` Prelude.rnf dbEngineVersionArn
      `Prelude.seq` Prelude.rnf dbEngineVersionDescription
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf databaseInstallationFilesS3BucketName
      `Prelude.seq` Prelude.rnf databaseInstallationFilesS3Prefix
      `Prelude.seq` Prelude.rnf defaultCharacterSet
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf exportableLogTypes
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf majorEngineVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        supportedCACertificateIdentifiers
      `Prelude.seq` Prelude.rnf
        supportedCharacterSets
      `Prelude.seq` Prelude.rnf
        supportedEngineModes
      `Prelude.seq` Prelude.rnf
        supportedFeatureNames
      `Prelude.seq` Prelude.rnf
        supportedNcharCharacterSets
      `Prelude.seq` Prelude.rnf
        supportedTimezones
      `Prelude.seq` Prelude.rnf
        supportsBabelfish
      `Prelude.seq` Prelude.rnf
        supportsCertificateRotationWithoutRestart
      `Prelude.seq` Prelude.rnf
        supportsGlobalDatabases
      `Prelude.seq` Prelude.rnf
        supportsLogExportsToCloudwatchLogs
      `Prelude.seq` Prelude.rnf
        supportsParallelQuery
      `Prelude.seq` Prelude.rnf
        supportsReadReplica
      `Prelude.seq` Prelude.rnf
        tagList
      `Prelude.seq` Prelude.rnf
        validUpgradeTarget
