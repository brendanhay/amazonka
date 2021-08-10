{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
module Network.AWS.RDS.CreateDBInstance
  ( -- * Creating a Request
    CreateDBInstance (..),
    newCreateDBInstance,

    -- * Request Lenses
    createDBInstance_backupRetentionPeriod,
    createDBInstance_deletionProtection,
    createDBInstance_storageEncrypted,
    createDBInstance_preferredBackupWindow,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_dbSecurityGroups,
    createDBInstance_maxAllocatedStorage,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_storageType,
    createDBInstance_monitoringInterval,
    createDBInstance_optionGroupName,
    createDBInstance_domain,
    createDBInstance_monitoringRoleArn,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_ncharCharacterSetName,
    createDBInstance_masterUserPassword,
    createDBInstance_masterUsername,
    createDBInstance_multiAZ,
    createDBInstance_publiclyAccessible,
    createDBInstance_vpcSecurityGroupIds,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_kmsKeyId,
    createDBInstance_dbParameterGroupName,
    createDBInstance_availabilityZone,
    createDBInstance_dbClusterIdentifier,
    createDBInstance_engineVersion,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_characterSetName,
    createDBInstance_performanceInsightsRetentionPeriod,
    createDBInstance_licenseModel,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_promotionTier,
    createDBInstance_tags,
    createDBInstance_processorFeatures,
    createDBInstance_port,
    createDBInstance_dbName,
    createDBInstance_domainIAMRoleName,
    createDBInstance_tdeCredentialArn,
    createDBInstance_enableCustomerOwnedIp,
    createDBInstance_timezone,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_allocatedStorage,
    createDBInstance_iops,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,

    -- * Destructuring the Response
    CreateDBInstanceResponse (..),
    newCreateDBInstanceResponse,

    -- * Response Lenses
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { -- | The number of days for which automated backups are retained. Setting
    -- this parameter to a positive number enables backups. Setting this
    -- parameter to 0 disables automated backups.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The retention period for automated backups is managed by
    -- the DB cluster.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 0 to 35
    --
    -- -   Can\'t be set to 0 if the DB instance is a source to read replicas
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. You can enable or disable deletion protection for the DB
    -- cluster. For more information, see @CreateDBCluster@. DB instances in a
    -- DB cluster can be deleted even when deletion protection is enabled for
    -- the DB cluster.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance is encrypted. By default,
    -- it isn\'t encrypted.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The encryption for DB instances is managed by the DB
    -- cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, using the @BackupRetentionPeriod@
    -- parameter. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window>
    -- in the /Amazon RDS User Guide/.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The daily time range for creating automated backups is
    -- managed by the DB cluster.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window>
    -- in the /Amazon RDS User Guide/.
    --
    -- Constraints:
    --
    -- -   Must be in the format @hh24:mi-hh24:mi@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon Relational Database Service User Guide/.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The upper limit to which Amazon RDS can automatically scale the storage
    -- of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- This setting doesn\'t apply to Amazon Aurora. Mapping AWS IAM accounts
    -- to database accounts is managed by the DB cluster.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of log types that need to be enabled for exporting to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Relational Database Service User Guide/.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
    --
    -- __MariaDB__
    --
    -- Possible values are @audit@, @error@, @general@, and @slowquery@.
    --
    -- __Microsoft SQL Server__
    --
    -- Possible values are @agent@ and @error@.
    --
    -- __MySQL__
    --
    -- Possible values are @audit@, @error@, @general@, and @slowquery@.
    --
    -- __Oracle__
    --
    -- Possible values are @alert@, @audit@, @listener@, @trace@, and
    -- @oemagent@.
    --
    -- __PostgreSQL__
    --
    -- Possible values are @postgresql@ and @upgrade@.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Valid values: @standard | gp2 | io1@
    --
    -- If you specify @io1@, you must also include a value for the @Iops@
    -- parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates that the DB instance should be associated with
    -- the specified option group.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group. Also, that option group
    -- can\'t be removed from a DB instance once it is associated with a DB
    -- instance
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory directory ID to create the DB instance in.
    -- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
    -- instances can be created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, go to
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
    -- in the /Amazon RDS User Guide/.
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A DB subnet group to associate with this DB instance.
    --
    -- If there is no DB subnet group, then it is a non-VPC DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the NCHAR character set for the Oracle DB instance.
    ncharCharacterSetName :: Prelude.Maybe Prelude.Text,
    -- | The password for the master user. The password can include any printable
    -- ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The password for the master user is managed by the DB
    -- cluster.
    --
    -- __MariaDB__
    --
    -- Constraints: Must contain from 8 to 41 characters.
    --
    -- __Microsoft SQL Server__
    --
    -- Constraints: Must contain from 8 to 128 characters.
    --
    -- __MySQL__
    --
    -- Constraints: Must contain from 8 to 41 characters.
    --
    -- __Oracle__
    --
    -- Constraints: Must contain from 8 to 30 characters.
    --
    -- __PostgreSQL__
    --
    -- Constraints: Must contain from 8 to 128 characters.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The name for the master user.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The name for the master user is managed by the DB
    -- cluster.
    --
    -- __MariaDB__
    --
    -- Constraints:
    --
    -- -   Required for MariaDB.
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    --
    -- __Microsoft SQL Server__
    --
    -- Constraints:
    --
    -- -   Required for SQL Server.
    --
    -- -   Must be 1 to 128 letters or numbers.
    --
    -- -   The first character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    --
    -- __MySQL__
    --
    -- Constraints:
    --
    -- -   Required for MySQL.
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    --
    -- __Oracle__
    --
    -- Constraints:
    --
    -- -   Required for Oracle.
    --
    -- -   Must be 1 to 30 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    --
    -- __PostgreSQL__
    --
    -- Constraints:
    --
    -- -   Required for PostgreSQL.
    --
    -- -   Must be 1 to 63 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    -- You can\'t set the @AvailabilityZone@ parameter if the DB instance is a
    -- Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves
    -- to the private IP address from within the DB instance\'s VPC, and to the
    -- public IP address from outside of the DB instance\'s VPC. Access to the
    -- DB instance is ultimately controlled by the security group it uses, and
    -- that public access is not permitted if the security group assigned to
    -- the DB instance doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- Default: The default behavior varies depending on whether
    -- @DBSubnetGroupName@ is specified.
    --
    -- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the default VPC in the target region doesn’t have an Internet
    --     gateway attached to it, the DB instance is private.
    --
    -- -   If the default VPC in the target region has an Internet gateway
    --     attached to it, the DB instance is public.
    --
    -- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the subnets are part of a VPC that doesn’t have an Internet
    --     gateway attached to it, the DB instance is private.
    --
    -- -   If the subnets are part of a VPC that has an Internet gateway
    --     attached to it, the DB instance is public.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | A list of Amazon EC2 VPC security groups to associate with this DB
    -- instance.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The associated list of EC2 VPC security groups is
    -- managed by the DB cluster.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The AWS KMS key identifier for encryption of Performance Insights data.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default CMK. There is a default CMK for your AWS
    -- account. Your AWS account has a different default CMK for each AWS
    -- Region.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The AWS KMS key identifier for an encrypted DB instance.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK). To use a CMK in a
    -- different AWS account, specify the key ARN or alias ARN.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The AWS KMS key identifier is managed by the DB cluster.
    -- For more information, see @CreateDBCluster@.
    --
    -- If @StorageEncrypted@ is enabled, and you do not specify a value for the
    -- @KmsKeyId@ parameter, then Amazon RDS uses your default CMK. There is a
    -- default CMK for your AWS account. Your AWS account has a different
    -- default CMK for each AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    -- If you do not specify a value, then the default DB parameter group for
    -- the specified DB engine and version is used.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone (AZ) where the database will be created. For
    -- information on AWS Regions and Availability Zones, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- AWS Region.
    --
    -- Example: @us-east-1d@
    --
    -- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
    -- DB instance is a Multi-AZ deployment. The specified Availability Zone
    -- must be in the same AWS Region as the current endpoint.
    --
    -- If you\'re creating a DB instance in an RDS on VMware environment,
    -- specify the identifier of the custom Availability Zone to create the DB
    -- instance in.
    --
    -- For more information about RDS on VMware, see the
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the DB cluster that the instance will belong to.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The version number of the database engine to use.
    --
    -- For a list of valid engine versions, use the @DescribeDBEngineVersions@
    -- action.
    --
    -- The following are the database engines and links to information about
    -- the major and minor versions that are available with Amazon RDS. Not
    -- every database engine is available for every AWS Region.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The version number of the database engine to be used by
    -- the DB instance is managed by the DB cluster.
    --
    -- __MariaDB__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions>
    -- in the /Amazon RDS User Guide./
    --
    -- __Microsoft SQL Server__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS>
    -- in the /Amazon RDS User Guide./
    --
    -- __MySQL__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
    -- in the /Amazon RDS User Guide./
    --
    -- __Oracle__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes>
    -- in the /Amazon RDS User Guide./
    --
    -- __PostgreSQL__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
    -- in the /Amazon RDS User Guide./
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The time range each week during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC). For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>.
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region, occurring on a random day of the
    -- week.
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | For supported engines, indicates that the DB instance should be
    -- associated with the specified CharacterSet.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The character set is managed by the DB cluster. For more
    -- information, see @CreateDBCluster@.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | License model information for this DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Default: 1
    --
    -- Valid Values: 0 - 15
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | Tags to assign to the DB instance.
    tags :: Prelude.Maybe [Tag],
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The port number on which the database accepts connections.
    --
    -- __MySQL__
    --
    -- Default: @3306@
    --
    -- Valid values: @1150-65535@
    --
    -- Type: Integer
    --
    -- __MariaDB__
    --
    -- Default: @3306@
    --
    -- Valid values: @1150-65535@
    --
    -- Type: Integer
    --
    -- __PostgreSQL__
    --
    -- Default: @5432@
    --
    -- Valid values: @1150-65535@
    --
    -- Type: Integer
    --
    -- __Oracle__
    --
    -- Default: @1521@
    --
    -- Valid values: @1150-65535@
    --
    -- __SQL Server__
    --
    -- Default: @1433@
    --
    -- Valid values: @1150-65535@ except @1234@, @1434@, @3260@, @3343@,
    -- @3389@, @47001@, and @49152-49156@.
    --
    -- __Amazon Aurora__
    --
    -- Default: @3306@
    --
    -- Valid values: @1150-65535@
    --
    -- Type: Integer
    port :: Prelude.Maybe Prelude.Int,
    -- | The meaning of this parameter differs according to the database engine
    -- you use.
    --
    -- __MySQL__
    --
    -- The name of the database to create when the DB instance is created. If
    -- this parameter isn\'t specified, no database is created in the DB
    -- instance.
    --
    -- Constraints:
    --
    -- -   Must contain 1 to 64 letters or numbers.
    --
    -- -   Must begin with a letter. Subsequent characters can be letters,
    --     underscores, or digits (0-9).
    --
    -- -   Can\'t be a word reserved by the specified database engine
    --
    -- __MariaDB__
    --
    -- The name of the database to create when the DB instance is created. If
    -- this parameter isn\'t specified, no database is created in the DB
    -- instance.
    --
    -- Constraints:
    --
    -- -   Must contain 1 to 64 letters or numbers.
    --
    -- -   Must begin with a letter. Subsequent characters can be letters,
    --     underscores, or digits (0-9).
    --
    -- -   Can\'t be a word reserved by the specified database engine
    --
    -- __PostgreSQL__
    --
    -- The name of the database to create when the DB instance is created. If
    -- this parameter isn\'t specified, a database named @postgres@ is created
    -- in the DB instance.
    --
    -- Constraints:
    --
    -- -   Must contain 1 to 63 letters, numbers, or underscores.
    --
    -- -   Must begin with a letter. Subsequent characters can be letters,
    --     underscores, or digits (0-9).
    --
    -- -   Can\'t be a word reserved by the specified database engine
    --
    -- __Oracle__
    --
    -- The Oracle System ID (SID) of the created DB instance. If you specify
    -- @null@, the default value @ORCL@ is used. You can\'t specify the string
    -- NULL, or any other reserved word, for @DBName@.
    --
    -- Default: @ORCL@
    --
    -- Constraints:
    --
    -- -   Can\'t be longer than 8 characters
    --
    -- __SQL Server__
    --
    -- Not applicable. Must be null.
    --
    -- __Amazon Aurora MySQL__
    --
    -- The name of the database to create when the primary DB instance of the
    -- Aurora MySQL DB cluster is created. If this parameter isn\'t specified
    -- for an Aurora MySQL DB cluster, no database is created in the DB
    -- cluster.
    --
    -- Constraints:
    --
    -- -   It must contain 1 to 64 alphanumeric characters.
    --
    -- -   It can\'t be a word reserved by the database engine.
    --
    -- __Amazon Aurora PostgreSQL__
    --
    -- The name of the database to create when the primary DB instance of the
    -- Aurora PostgreSQL DB cluster is created. If this parameter isn\'t
    -- specified for an Aurora PostgreSQL DB cluster, a database named
    -- @postgres@ is created in the DB cluster.
    --
    -- Constraints:
    --
    -- -   It must contain 1 to 63 alphanumeric characters.
    --
    -- -   It must begin with a letter or an underscore. Subsequent characters
    --     can be letters, underscores, or digits (0 to 9).
    --
    -- -   It can\'t be a word reserved by the database engine.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable a customer-owned IP address
    -- (CoIP) for an RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /AWS Outposts User Guide/.
    enableCustomerOwnedIp :: Prelude.Maybe Prelude.Bool,
    -- | The time zone of the DB instance. The time zone parameter is currently
    -- supported only by
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server>.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster.
    -- Setting this value for an Aurora DB instance has no effect on the DB
    -- cluster setting.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The amount of storage (in gibibytes) to allocate for the DB instance.
    --
    -- Type: Integer
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. Aurora cluster volumes automatically grow as the amount
    -- of data in your database increases, though you are only charged for the
    -- space that you use in an Aurora cluster volume.
    --
    -- __MySQL__
    --
    -- Constraints to the amount of storage for each storage type are the
    -- following:
    --
    -- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
    --     65536.
    --
    -- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --     65536.
    --
    -- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    -- __MariaDB__
    --
    -- Constraints to the amount of storage for each storage type are the
    -- following:
    --
    -- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
    --     65536.
    --
    -- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --     65536.
    --
    -- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    -- __PostgreSQL__
    --
    -- Constraints to the amount of storage for each storage type are the
    -- following:
    --
    -- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
    --     65536.
    --
    -- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --     65536.
    --
    -- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    -- __Oracle__
    --
    -- Constraints to the amount of storage for each storage type are the
    -- following:
    --
    -- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
    --     65536.
    --
    -- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --     65536.
    --
    -- -   Magnetic storage (standard): Must be an integer from 10 to 3072.
    --
    -- __SQL Server__
    --
    -- Constraints to the amount of storage for each storage type are the
    -- following:
    --
    -- -   General Purpose (SSD) storage (gp2):
    --
    --     -   Enterprise and Standard editions: Must be an integer from 200 to
    --         16384.
    --
    --     -   Web and Express editions: Must be an integer from 20 to 16384.
    --
    -- -   Provisioned IOPS storage (io1):
    --
    --     -   Enterprise and Standard editions: Must be an integer from 200 to
    --         16384.
    --
    --     -   Web and Express editions: Must be an integer from 100 to 16384.
    --
    -- -   Magnetic storage (standard):
    --
    --     -   Enterprise and Standard editions: Must be an integer from 200 to
    --         1024.
    --
    --     -   Web and Express editions: Must be an integer from 20 to 1024.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for the DB instance. For information about valid
    -- Iops values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
    -- in the /Amazon RDS User Guide/.
    --
    -- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances,
    -- must be a multiple between .5 and 50 of the storage amount for the DB
    -- instance. For SQL Server DB instances, must be a multiple between 1 and
    -- 50 of the storage amount for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the DB instance during the maintenance window. By
    -- default, minor engine upgrades are applied automatically.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance identifier. This parameter is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @mydbinstance@
    dbInstanceIdentifier :: Prelude.Text,
    -- | The compute and memory capacity of the DB instance, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all AWS
    -- Regions, or for all database engines. For the full list of DB instance
    -- classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    dbInstanceClass :: Prelude.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- Not every database engine is available for every AWS Region.
    --
    -- Valid Values:
    --
    -- -   @aurora@ (for MySQL 5.6-compatible Aurora)
    --
    -- -   @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
    --
    -- -   @aurora-postgresql@
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se1@
    --
    -- -   @oracle-se@
    --
    -- -   @postgres@
    --
    -- -   @sqlserver-ee@
    --
    -- -   @sqlserver-se@
    --
    -- -   @sqlserver-ex@
    --
    -- -   @sqlserver-web@
    engine :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'createDBInstance_backupRetentionPeriod' - The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to 0 disables automated backups.
--
-- __Amazon Aurora__
--
-- Not applicable. The retention period for automated backups is managed by
-- the DB cluster.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas
--
-- 'deletionProtection', 'createDBInstance_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- __Amazon Aurora__
--
-- Not applicable. You can enable or disable deletion protection for the DB
-- cluster. For more information, see @CreateDBCluster@. DB instances in a
-- DB cluster can be deleted even when deletion protection is enabled for
-- the DB cluster.
--
-- 'storageEncrypted', 'createDBInstance_storageEncrypted' - A value that indicates whether the DB instance is encrypted. By default,
-- it isn\'t encrypted.
--
-- __Amazon Aurora__
--
-- Not applicable. The encryption for DB instances is managed by the DB
-- cluster.
--
-- 'preferredBackupWindow', 'createDBInstance_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window>
-- in the /Amazon RDS User Guide/.
--
-- __Amazon Aurora__
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window>
-- in the /Amazon RDS User Guide/.
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- 'enablePerformanceInsights', 'createDBInstance_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
--
-- 'dbSecurityGroups', 'createDBInstance_dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- 'maxAllocatedStorage', 'createDBInstance_maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'createDBInstance_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- This setting doesn\'t apply to Amazon Aurora. Mapping AWS IAM accounts
-- to database accounts is managed by the DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'enableCloudwatchLogsExports', 'createDBInstance_enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Relational Database Service User Guide/.
--
-- __Amazon Aurora__
--
-- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
--
-- __MariaDB__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Microsoft SQL Server__
--
-- Possible values are @agent@ and @error@.
--
-- __MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Oracle__
--
-- Possible values are @alert@, @audit@, @listener@, @trace@, and
-- @oemagent@.
--
-- __PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
--
-- 'storageType', 'createDBInstance_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'monitoringInterval', 'createDBInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- 'optionGroupName', 'createDBInstance_optionGroupName' - A value that indicates that the DB instance should be associated with
-- the specified option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group. Also, that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
--
-- 'domain', 'createDBInstance_domain' - The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- 'monitoringRoleArn', 'createDBInstance_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- 'dbSubnetGroupName', 'createDBInstance_dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
--
-- 'ncharCharacterSetName', 'createDBInstance_ncharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance.
--
-- 'masterUserPassword', 'createDBInstance_masterUserPassword' - The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- __Amazon Aurora__
--
-- Not applicable. The password for the master user is managed by the DB
-- cluster.
--
-- __MariaDB__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Microsoft SQL Server__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- __MySQL__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Oracle__
--
-- Constraints: Must contain from 8 to 30 characters.
--
-- __PostgreSQL__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- 'masterUsername', 'createDBInstance_masterUsername' - The name for the master user.
--
-- __Amazon Aurora__
--
-- Not applicable. The name for the master user is managed by the DB
-- cluster.
--
-- __MariaDB__
--
-- Constraints:
--
-- -   Required for MariaDB.
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __Microsoft SQL Server__
--
-- Constraints:
--
-- -   Required for SQL Server.
--
-- -   Must be 1 to 128 letters or numbers.
--
-- -   The first character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __MySQL__
--
-- Constraints:
--
-- -   Required for MySQL.
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __Oracle__
--
-- Constraints:
--
-- -   Required for Oracle.
--
-- -   Must be 1 to 30 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __PostgreSQL__
--
-- Constraints:
--
-- -   Required for PostgreSQL.
--
-- -   Must be 1 to 63 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'multiAZ', 'createDBInstance_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
-- You can\'t set the @AvailabilityZone@ parameter if the DB instance is a
-- Multi-AZ deployment.
--
-- 'publiclyAccessible', 'createDBInstance_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target region doesn’t have an Internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the default VPC in the target region has an Internet gateway
--     attached to it, the DB instance is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an Internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the subnets are part of a VPC that has an Internet gateway
--     attached to it, the DB instance is public.
--
-- 'vpcSecurityGroupIds', 'createDBInstance_vpcSecurityGroupIds' - A list of Amazon EC2 VPC security groups to associate with this DB
-- instance.
--
-- __Amazon Aurora__
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
--
-- 'performanceInsightsKMSKeyId', 'createDBInstance_performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
--
-- 'kmsKeyId', 'createDBInstance_kmsKeyId' - The AWS KMS key identifier for an encrypted DB instance.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- __Amazon Aurora__
--
-- Not applicable. The AWS KMS key identifier is managed by the DB cluster.
-- For more information, see @CreateDBCluster@.
--
-- If @StorageEncrypted@ is enabled, and you do not specify a value for the
-- @KmsKeyId@ parameter, then Amazon RDS uses your default CMK. There is a
-- default CMK for your AWS account. Your AWS account has a different
-- default CMK for each AWS Region.
--
-- 'dbParameterGroupName', 'createDBInstance_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
-- If you do not specify a value, then the default DB parameter group for
-- the specified DB engine and version is used.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- 'availabilityZone', 'createDBInstance_availabilityZone' - The Availability Zone (AZ) where the database will be created. For
-- information on AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- AWS Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
-- DB instance is a Multi-AZ deployment. The specified Availability Zone
-- must be in the same AWS Region as the current endpoint.
--
-- If you\'re creating a DB instance in an RDS on VMware environment,
-- specify the identifier of the custom Availability Zone to create the DB
-- instance in.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
--
-- 'dbClusterIdentifier', 'createDBInstance_dbClusterIdentifier' - The identifier of the DB cluster that the instance will belong to.
--
-- 'engineVersion', 'createDBInstance_engineVersion' - The version number of the database engine to use.
--
-- For a list of valid engine versions, use the @DescribeDBEngineVersions@
-- action.
--
-- The following are the database engines and links to information about
-- the major and minor versions that are available with Amazon RDS. Not
-- every database engine is available for every AWS Region.
--
-- __Amazon Aurora__
--
-- Not applicable. The version number of the database engine to be used by
-- the DB instance is managed by the DB cluster.
--
-- __MariaDB__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions>
-- in the /Amazon RDS User Guide./
--
-- __Microsoft SQL Server__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- __MySQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
-- in the /Amazon RDS User Guide./
--
-- __Oracle__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes>
-- in the /Amazon RDS User Guide./
--
-- __PostgreSQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
-- in the /Amazon RDS User Guide./
--
-- 'preferredMaintenanceWindow', 'createDBInstance_preferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC). For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'characterSetName', 'createDBInstance_characterSetName' - For supported engines, indicates that the DB instance should be
-- associated with the specified CharacterSet.
--
-- __Amazon Aurora__
--
-- Not applicable. The character set is managed by the DB cluster. For more
-- information, see @CreateDBCluster@.
--
-- 'performanceInsightsRetentionPeriod', 'createDBInstance_performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
--
-- 'licenseModel', 'createDBInstance_licenseModel' - License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'tdeCredentialPassword', 'createDBInstance_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- 'promotionTier', 'createDBInstance_promotionTier' - A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- Default: 1
--
-- Valid Values: 0 - 15
--
-- 'tags', 'createDBInstance_tags' - Tags to assign to the DB instance.
--
-- 'processorFeatures', 'createDBInstance_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'port', 'createDBInstance_port' - The port number on which the database accepts connections.
--
-- __MySQL__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __MariaDB__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __PostgreSQL__
--
-- Default: @5432@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __Oracle__
--
-- Default: @1521@
--
-- Valid values: @1150-65535@
--
-- __SQL Server__
--
-- Default: @1433@
--
-- Valid values: @1150-65535@ except @1234@, @1434@, @3260@, @3343@,
-- @3389@, @47001@, and @49152-49156@.
--
-- __Amazon Aurora__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- 'dbName', 'createDBInstance_dbName' - The meaning of this parameter differs according to the database engine
-- you use.
--
-- __MySQL__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter isn\'t specified, no database is created in the DB
-- instance.
--
-- Constraints:
--
-- -   Must contain 1 to 64 letters or numbers.
--
-- -   Must begin with a letter. Subsequent characters can be letters,
--     underscores, or digits (0-9).
--
-- -   Can\'t be a word reserved by the specified database engine
--
-- __MariaDB__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter isn\'t specified, no database is created in the DB
-- instance.
--
-- Constraints:
--
-- -   Must contain 1 to 64 letters or numbers.
--
-- -   Must begin with a letter. Subsequent characters can be letters,
--     underscores, or digits (0-9).
--
-- -   Can\'t be a word reserved by the specified database engine
--
-- __PostgreSQL__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter isn\'t specified, a database named @postgres@ is created
-- in the DB instance.
--
-- Constraints:
--
-- -   Must contain 1 to 63 letters, numbers, or underscores.
--
-- -   Must begin with a letter. Subsequent characters can be letters,
--     underscores, or digits (0-9).
--
-- -   Can\'t be a word reserved by the specified database engine
--
-- __Oracle__
--
-- The Oracle System ID (SID) of the created DB instance. If you specify
-- @null@, the default value @ORCL@ is used. You can\'t specify the string
-- NULL, or any other reserved word, for @DBName@.
--
-- Default: @ORCL@
--
-- Constraints:
--
-- -   Can\'t be longer than 8 characters
--
-- __SQL Server__
--
-- Not applicable. Must be null.
--
-- __Amazon Aurora MySQL__
--
-- The name of the database to create when the primary DB instance of the
-- Aurora MySQL DB cluster is created. If this parameter isn\'t specified
-- for an Aurora MySQL DB cluster, no database is created in the DB
-- cluster.
--
-- Constraints:
--
-- -   It must contain 1 to 64 alphanumeric characters.
--
-- -   It can\'t be a word reserved by the database engine.
--
-- __Amazon Aurora PostgreSQL__
--
-- The name of the database to create when the primary DB instance of the
-- Aurora PostgreSQL DB cluster is created. If this parameter isn\'t
-- specified for an Aurora PostgreSQL DB cluster, a database named
-- @postgres@ is created in the DB cluster.
--
-- Constraints:
--
-- -   It must contain 1 to 63 alphanumeric characters.
--
-- -   It must begin with a letter or an underscore. Subsequent characters
--     can be letters, underscores, or digits (0 to 9).
--
-- -   It can\'t be a word reserved by the database engine.
--
-- 'domainIAMRoleName', 'createDBInstance_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'tdeCredentialArn', 'createDBInstance_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'enableCustomerOwnedIp', 'createDBInstance_enableCustomerOwnedIp' - A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
--
-- 'timezone', 'createDBInstance_timezone' - The time zone of the DB instance. The time zone parameter is currently
-- supported only by
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server>.
--
-- 'copyTagsToSnapshot', 'createDBInstance_copyTagsToSnapshot' - A value that indicates whether to copy tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting.
--
-- 'allocatedStorage', 'createDBInstance_allocatedStorage' - The amount of storage (in gibibytes) to allocate for the DB instance.
--
-- Type: Integer
--
-- __Amazon Aurora__
--
-- Not applicable. Aurora cluster volumes automatically grow as the amount
-- of data in your database increases, though you are only charged for the
-- space that you use in an Aurora cluster volume.
--
-- __MySQL__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
-- __MariaDB__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
-- __PostgreSQL__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
-- __Oracle__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 10 to 3072.
--
-- __SQL Server__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2):
--
--     -   Enterprise and Standard editions: Must be an integer from 200 to
--         16384.
--
--     -   Web and Express editions: Must be an integer from 20 to 16384.
--
-- -   Provisioned IOPS storage (io1):
--
--     -   Enterprise and Standard editions: Must be an integer from 200 to
--         16384.
--
--     -   Web and Express editions: Must be an integer from 100 to 16384.
--
-- -   Magnetic storage (standard):
--
--     -   Enterprise and Standard editions: Must be an integer from 200 to
--         1024.
--
--     -   Web and Express editions: Must be an integer from 20 to 1024.
--
-- 'iops', 'createDBInstance_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance. For information about valid
-- Iops values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances,
-- must be a multiple between .5 and 50 of the storage amount for the DB
-- instance. For SQL Server DB instances, must be a multiple between 1 and
-- 50 of the storage amount for the DB instance.
--
-- 'autoMinorVersionUpgrade', 'createDBInstance_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are applied automatically.
--
-- 'dbInstanceIdentifier', 'createDBInstance_dbInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
--
-- 'dbInstanceClass', 'createDBInstance_dbInstanceClass' - The compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- 'engine', 'createDBInstance_engine' - The name of the database engine to be used for this instance.
--
-- Not every database engine is available for every AWS Region.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
newCreateDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'dbInstanceClass'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  CreateDBInstance
newCreateDBInstance
  pDBInstanceIdentifier_
  pDBInstanceClass_
  pEngine_ =
    CreateDBInstance'
      { backupRetentionPeriod =
          Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        dbSecurityGroups = Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        storageType = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        domain = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        ncharCharacterSetName = Prelude.Nothing,
        masterUserPassword = Prelude.Nothing,
        masterUsername = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        dbClusterIdentifier = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        characterSetName = Prelude.Nothing,
        performanceInsightsRetentionPeriod = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        tdeCredentialPassword = Prelude.Nothing,
        promotionTier = Prelude.Nothing,
        tags = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        port = Prelude.Nothing,
        dbName = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        tdeCredentialArn = Prelude.Nothing,
        enableCustomerOwnedIp = Prelude.Nothing,
        timezone = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        allocatedStorage = Prelude.Nothing,
        iops = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbInstanceClass = pDBInstanceClass_,
        engine = pEngine_
      }

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to 0 disables automated backups.
--
-- __Amazon Aurora__
--
-- Not applicable. The retention period for automated backups is managed by
-- the DB cluster.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas
createDBInstance_backupRetentionPeriod :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_backupRetentionPeriod = Lens.lens (\CreateDBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@CreateDBInstance' {} a -> s {backupRetentionPeriod = a} :: CreateDBInstance)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- __Amazon Aurora__
--
-- Not applicable. You can enable or disable deletion protection for the DB
-- cluster. For more information, see @CreateDBCluster@. DB instances in a
-- DB cluster can be deleted even when deletion protection is enabled for
-- the DB cluster.
createDBInstance_deletionProtection :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_deletionProtection = Lens.lens (\CreateDBInstance' {deletionProtection} -> deletionProtection) (\s@CreateDBInstance' {} a -> s {deletionProtection = a} :: CreateDBInstance)

-- | A value that indicates whether the DB instance is encrypted. By default,
-- it isn\'t encrypted.
--
-- __Amazon Aurora__
--
-- Not applicable. The encryption for DB instances is managed by the DB
-- cluster.
createDBInstance_storageEncrypted :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_storageEncrypted = Lens.lens (\CreateDBInstance' {storageEncrypted} -> storageEncrypted) (\s@CreateDBInstance' {} a -> s {storageEncrypted = a} :: CreateDBInstance)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow The Backup Window>
-- in the /Amazon RDS User Guide/.
--
-- __Amazon Aurora__
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window>
-- in the /Amazon RDS User Guide/.
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
createDBInstance_preferredBackupWindow :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_preferredBackupWindow = Lens.lens (\CreateDBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateDBInstance' {} a -> s {preferredBackupWindow = a} :: CreateDBInstance)

-- | A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
createDBInstance_enablePerformanceInsights :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enablePerformanceInsights = Lens.lens (\CreateDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBInstance' {} a -> s {enablePerformanceInsights = a} :: CreateDBInstance)

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
createDBInstance_dbSecurityGroups :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_dbSecurityGroups = Lens.lens (\CreateDBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@CreateDBInstance' {} a -> s {dbSecurityGroups = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens._Coerce

-- | The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
createDBInstance_maxAllocatedStorage :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_maxAllocatedStorage = Lens.lens (\CreateDBInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@CreateDBInstance' {} a -> s {maxAllocatedStorage = a} :: CreateDBInstance)

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- This setting doesn\'t apply to Amazon Aurora. Mapping AWS IAM accounts
-- to database accounts is managed by the DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
createDBInstance_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBInstance' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBInstance' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstance)

-- | The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Relational Database Service User Guide/.
--
-- __Amazon Aurora__
--
-- Not applicable. CloudWatch Logs exports are managed by the DB cluster.
--
-- __MariaDB__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Microsoft SQL Server__
--
-- Possible values are @agent@ and @error@.
--
-- __MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Oracle__
--
-- Possible values are @alert@, @audit@, @listener@, @trace@, and
-- @oemagent@.
--
-- __PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
createDBInstance_enableCloudwatchLogsExports :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_enableCloudwatchLogsExports = Lens.lens (\CreateDBInstance' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBInstance' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
createDBInstance_storageType :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_storageType = Lens.lens (\CreateDBInstance' {storageType} -> storageType) (\s@CreateDBInstance' {} a -> s {storageType = a} :: CreateDBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
createDBInstance_monitoringInterval :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_monitoringInterval = Lens.lens (\CreateDBInstance' {monitoringInterval} -> monitoringInterval) (\s@CreateDBInstance' {} a -> s {monitoringInterval = a} :: CreateDBInstance)

-- | A value that indicates that the DB instance should be associated with
-- the specified option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group. Also, that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
createDBInstance_optionGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_optionGroupName = Lens.lens (\CreateDBInstance' {optionGroupName} -> optionGroupName) (\s@CreateDBInstance' {} a -> s {optionGroupName = a} :: CreateDBInstance)

-- | The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
createDBInstance_domain :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_domain = Lens.lens (\CreateDBInstance' {domain} -> domain) (\s@CreateDBInstance' {} a -> s {domain = a} :: CreateDBInstance)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
createDBInstance_monitoringRoleArn :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_monitoringRoleArn = Lens.lens (\CreateDBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@CreateDBInstance' {} a -> s {monitoringRoleArn = a} :: CreateDBInstance)

-- | A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
createDBInstance_dbSubnetGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbSubnetGroupName = Lens.lens (\CreateDBInstance' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBInstance' {} a -> s {dbSubnetGroupName = a} :: CreateDBInstance)

-- | The name of the NCHAR character set for the Oracle DB instance.
createDBInstance_ncharCharacterSetName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_ncharCharacterSetName = Lens.lens (\CreateDBInstance' {ncharCharacterSetName} -> ncharCharacterSetName) (\s@CreateDBInstance' {} a -> s {ncharCharacterSetName = a} :: CreateDBInstance)

-- | The password for the master user. The password can include any printable
-- ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- __Amazon Aurora__
--
-- Not applicable. The password for the master user is managed by the DB
-- cluster.
--
-- __MariaDB__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Microsoft SQL Server__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- __MySQL__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Oracle__
--
-- Constraints: Must contain from 8 to 30 characters.
--
-- __PostgreSQL__
--
-- Constraints: Must contain from 8 to 128 characters.
createDBInstance_masterUserPassword :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_masterUserPassword = Lens.lens (\CreateDBInstance' {masterUserPassword} -> masterUserPassword) (\s@CreateDBInstance' {} a -> s {masterUserPassword = a} :: CreateDBInstance)

-- | The name for the master user.
--
-- __Amazon Aurora__
--
-- Not applicable. The name for the master user is managed by the DB
-- cluster.
--
-- __MariaDB__
--
-- Constraints:
--
-- -   Required for MariaDB.
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __Microsoft SQL Server__
--
-- Constraints:
--
-- -   Required for SQL Server.
--
-- -   Must be 1 to 128 letters or numbers.
--
-- -   The first character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __MySQL__
--
-- Constraints:
--
-- -   Required for MySQL.
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __Oracle__
--
-- Constraints:
--
-- -   Required for Oracle.
--
-- -   Must be 1 to 30 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- __PostgreSQL__
--
-- Constraints:
--
-- -   Required for PostgreSQL.
--
-- -   Must be 1 to 63 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
createDBInstance_masterUsername :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_masterUsername = Lens.lens (\CreateDBInstance' {masterUsername} -> masterUsername) (\s@CreateDBInstance' {} a -> s {masterUsername = a} :: CreateDBInstance)

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
-- You can\'t set the @AvailabilityZone@ parameter if the DB instance is a
-- Multi-AZ deployment.
createDBInstance_multiAZ :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_multiAZ = Lens.lens (\CreateDBInstance' {multiAZ} -> multiAZ) (\s@CreateDBInstance' {} a -> s {multiAZ = a} :: CreateDBInstance)

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target region doesn’t have an Internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the default VPC in the target region has an Internet gateway
--     attached to it, the DB instance is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an Internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the subnets are part of a VPC that has an Internet gateway
--     attached to it, the DB instance is public.
createDBInstance_publiclyAccessible :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_publiclyAccessible = Lens.lens (\CreateDBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@CreateDBInstance' {} a -> s {publiclyAccessible = a} :: CreateDBInstance)

-- | A list of Amazon EC2 VPC security groups to associate with this DB
-- instance.
--
-- __Amazon Aurora__
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
createDBInstance_vpcSecurityGroupIds :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_vpcSecurityGroupIds = Lens.lens (\CreateDBInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBInstance' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens._Coerce

-- | The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
createDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_performanceInsightsKMSKeyId = Lens.lens (\CreateDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstance)

-- | The AWS KMS key identifier for an encrypted DB instance.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- __Amazon Aurora__
--
-- Not applicable. The AWS KMS key identifier is managed by the DB cluster.
-- For more information, see @CreateDBCluster@.
--
-- If @StorageEncrypted@ is enabled, and you do not specify a value for the
-- @KmsKeyId@ parameter, then Amazon RDS uses your default CMK. There is a
-- default CMK for your AWS account. Your AWS account has a different
-- default CMK for each AWS Region.
createDBInstance_kmsKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_kmsKeyId = Lens.lens (\CreateDBInstance' {kmsKeyId} -> kmsKeyId) (\s@CreateDBInstance' {} a -> s {kmsKeyId = a} :: CreateDBInstance)

-- | The name of the DB parameter group to associate with this DB instance.
-- If you do not specify a value, then the default DB parameter group for
-- the specified DB engine and version is used.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
createDBInstance_dbParameterGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbParameterGroupName = Lens.lens (\CreateDBInstance' {dbParameterGroupName} -> dbParameterGroupName) (\s@CreateDBInstance' {} a -> s {dbParameterGroupName = a} :: CreateDBInstance)

-- | The Availability Zone (AZ) where the database will be created. For
-- information on AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- AWS Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The @AvailabilityZone@ parameter can\'t be specified if the
-- DB instance is a Multi-AZ deployment. The specified Availability Zone
-- must be in the same AWS Region as the current endpoint.
--
-- If you\'re creating a DB instance in an RDS on VMware environment,
-- specify the identifier of the custom Availability Zone to create the DB
-- instance in.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
createDBInstance_availabilityZone :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_availabilityZone = Lens.lens (\CreateDBInstance' {availabilityZone} -> availabilityZone) (\s@CreateDBInstance' {} a -> s {availabilityZone = a} :: CreateDBInstance)

-- | The identifier of the DB cluster that the instance will belong to.
createDBInstance_dbClusterIdentifier :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbClusterIdentifier = Lens.lens (\CreateDBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBInstance' {} a -> s {dbClusterIdentifier = a} :: CreateDBInstance)

-- | The version number of the database engine to use.
--
-- For a list of valid engine versions, use the @DescribeDBEngineVersions@
-- action.
--
-- The following are the database engines and links to information about
-- the major and minor versions that are available with Amazon RDS. Not
-- every database engine is available for every AWS Region.
--
-- __Amazon Aurora__
--
-- Not applicable. The version number of the database engine to be used by
-- the DB instance is managed by the DB cluster.
--
-- __MariaDB__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS Versions>
-- in the /Amazon RDS User Guide./
--
-- __Microsoft SQL Server__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- __MySQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
-- in the /Amazon RDS User Guide./
--
-- __Oracle__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine Release Notes>
-- in the /Amazon RDS User Guide./
--
-- __PostgreSQL__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
-- in the /Amazon RDS User Guide./
createDBInstance_engineVersion :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_engineVersion = Lens.lens (\CreateDBInstance' {engineVersion} -> engineVersion) (\s@CreateDBInstance' {} a -> s {engineVersion = a} :: CreateDBInstance)

-- | The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC). For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
createDBInstance_preferredMaintenanceWindow :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_preferredMaintenanceWindow = Lens.lens (\CreateDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBInstance)

-- | For supported engines, indicates that the DB instance should be
-- associated with the specified CharacterSet.
--
-- __Amazon Aurora__
--
-- Not applicable. The character set is managed by the DB cluster. For more
-- information, see @CreateDBCluster@.
createDBInstance_characterSetName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_characterSetName = Lens.lens (\CreateDBInstance' {characterSetName} -> characterSetName) (\s@CreateDBInstance' {} a -> s {characterSetName = a} :: CreateDBInstance)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
createDBInstance_performanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_performanceInsightsRetentionPeriod = Lens.lens (\CreateDBInstance' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@CreateDBInstance' {} a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBInstance)

-- | License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
createDBInstance_licenseModel :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_licenseModel = Lens.lens (\CreateDBInstance' {licenseModel} -> licenseModel) (\s@CreateDBInstance' {} a -> s {licenseModel = a} :: CreateDBInstance)

-- | The password for the given ARN from the key store in order to access the
-- device.
createDBInstance_tdeCredentialPassword :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_tdeCredentialPassword = Lens.lens (\CreateDBInstance' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@CreateDBInstance' {} a -> s {tdeCredentialPassword = a} :: CreateDBInstance)

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- Default: 1
--
-- Valid Values: 0 - 15
createDBInstance_promotionTier :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_promotionTier = Lens.lens (\CreateDBInstance' {promotionTier} -> promotionTier) (\s@CreateDBInstance' {} a -> s {promotionTier = a} :: CreateDBInstance)

-- | Tags to assign to the DB instance.
createDBInstance_tags :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Tag])
createDBInstance_tags = Lens.lens (\CreateDBInstance' {tags} -> tags) (\s@CreateDBInstance' {} a -> s {tags = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens._Coerce

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
createDBInstance_processorFeatures :: Lens.Lens' CreateDBInstance (Prelude.Maybe [ProcessorFeature])
createDBInstance_processorFeatures = Lens.lens (\CreateDBInstance' {processorFeatures} -> processorFeatures) (\s@CreateDBInstance' {} a -> s {processorFeatures = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens._Coerce

-- | The port number on which the database accepts connections.
--
-- __MySQL__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __MariaDB__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __PostgreSQL__
--
-- Default: @5432@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __Oracle__
--
-- Default: @1521@
--
-- Valid values: @1150-65535@
--
-- __SQL Server__
--
-- Default: @1433@
--
-- Valid values: @1150-65535@ except @1234@, @1434@, @3260@, @3343@,
-- @3389@, @47001@, and @49152-49156@.
--
-- __Amazon Aurora__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
createDBInstance_port :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_port = Lens.lens (\CreateDBInstance' {port} -> port) (\s@CreateDBInstance' {} a -> s {port = a} :: CreateDBInstance)

-- | The meaning of this parameter differs according to the database engine
-- you use.
--
-- __MySQL__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter isn\'t specified, no database is created in the DB
-- instance.
--
-- Constraints:
--
-- -   Must contain 1 to 64 letters or numbers.
--
-- -   Must begin with a letter. Subsequent characters can be letters,
--     underscores, or digits (0-9).
--
-- -   Can\'t be a word reserved by the specified database engine
--
-- __MariaDB__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter isn\'t specified, no database is created in the DB
-- instance.
--
-- Constraints:
--
-- -   Must contain 1 to 64 letters or numbers.
--
-- -   Must begin with a letter. Subsequent characters can be letters,
--     underscores, or digits (0-9).
--
-- -   Can\'t be a word reserved by the specified database engine
--
-- __PostgreSQL__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter isn\'t specified, a database named @postgres@ is created
-- in the DB instance.
--
-- Constraints:
--
-- -   Must contain 1 to 63 letters, numbers, or underscores.
--
-- -   Must begin with a letter. Subsequent characters can be letters,
--     underscores, or digits (0-9).
--
-- -   Can\'t be a word reserved by the specified database engine
--
-- __Oracle__
--
-- The Oracle System ID (SID) of the created DB instance. If you specify
-- @null@, the default value @ORCL@ is used. You can\'t specify the string
-- NULL, or any other reserved word, for @DBName@.
--
-- Default: @ORCL@
--
-- Constraints:
--
-- -   Can\'t be longer than 8 characters
--
-- __SQL Server__
--
-- Not applicable. Must be null.
--
-- __Amazon Aurora MySQL__
--
-- The name of the database to create when the primary DB instance of the
-- Aurora MySQL DB cluster is created. If this parameter isn\'t specified
-- for an Aurora MySQL DB cluster, no database is created in the DB
-- cluster.
--
-- Constraints:
--
-- -   It must contain 1 to 64 alphanumeric characters.
--
-- -   It can\'t be a word reserved by the database engine.
--
-- __Amazon Aurora PostgreSQL__
--
-- The name of the database to create when the primary DB instance of the
-- Aurora PostgreSQL DB cluster is created. If this parameter isn\'t
-- specified for an Aurora PostgreSQL DB cluster, a database named
-- @postgres@ is created in the DB cluster.
--
-- Constraints:
--
-- -   It must contain 1 to 63 alphanumeric characters.
--
-- -   It must begin with a letter or an underscore. Subsequent characters
--     can be letters, underscores, or digits (0 to 9).
--
-- -   It can\'t be a word reserved by the database engine.
createDBInstance_dbName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbName = Lens.lens (\CreateDBInstance' {dbName} -> dbName) (\s@CreateDBInstance' {} a -> s {dbName = a} :: CreateDBInstance)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
createDBInstance_domainIAMRoleName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_domainIAMRoleName = Lens.lens (\CreateDBInstance' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBInstance' {} a -> s {domainIAMRoleName = a} :: CreateDBInstance)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
createDBInstance_tdeCredentialArn :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_tdeCredentialArn = Lens.lens (\CreateDBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@CreateDBInstance' {} a -> s {tdeCredentialArn = a} :: CreateDBInstance)

-- | A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
createDBInstance_enableCustomerOwnedIp :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enableCustomerOwnedIp = Lens.lens (\CreateDBInstance' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@CreateDBInstance' {} a -> s {enableCustomerOwnedIp = a} :: CreateDBInstance)

-- | The time zone of the DB instance. The time zone parameter is currently
-- supported only by
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server>.
createDBInstance_timezone :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_timezone = Lens.lens (\CreateDBInstance' {timezone} -> timezone) (\s@CreateDBInstance' {} a -> s {timezone = a} :: CreateDBInstance)

-- | A value that indicates whether to copy tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting.
createDBInstance_copyTagsToSnapshot :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_copyTagsToSnapshot = Lens.lens (\CreateDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBInstance' {} a -> s {copyTagsToSnapshot = a} :: CreateDBInstance)

-- | The amount of storage (in gibibytes) to allocate for the DB instance.
--
-- Type: Integer
--
-- __Amazon Aurora__
--
-- Not applicable. Aurora cluster volumes automatically grow as the amount
-- of data in your database increases, though you are only charged for the
-- space that you use in an Aurora cluster volume.
--
-- __MySQL__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
-- __MariaDB__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
-- __PostgreSQL__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
-- __Oracle__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2): Must be an integer from 20 to
--     65536.
--
-- -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--     65536.
--
-- -   Magnetic storage (standard): Must be an integer from 10 to 3072.
--
-- __SQL Server__
--
-- Constraints to the amount of storage for each storage type are the
-- following:
--
-- -   General Purpose (SSD) storage (gp2):
--
--     -   Enterprise and Standard editions: Must be an integer from 200 to
--         16384.
--
--     -   Web and Express editions: Must be an integer from 20 to 16384.
--
-- -   Provisioned IOPS storage (io1):
--
--     -   Enterprise and Standard editions: Must be an integer from 200 to
--         16384.
--
--     -   Web and Express editions: Must be an integer from 100 to 16384.
--
-- -   Magnetic storage (standard):
--
--     -   Enterprise and Standard editions: Must be an integer from 200 to
--         1024.
--
--     -   Web and Express editions: Must be an integer from 20 to 1024.
createDBInstance_allocatedStorage :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_allocatedStorage = Lens.lens (\CreateDBInstance' {allocatedStorage} -> allocatedStorage) (\s@CreateDBInstance' {} a -> s {allocatedStorage = a} :: CreateDBInstance)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance. For information about valid
-- Iops values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS Storage to Improve Performance>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL DB instances,
-- must be a multiple between .5 and 50 of the storage amount for the DB
-- instance. For SQL Server DB instances, must be a multiple between 1 and
-- 50 of the storage amount for the DB instance.
createDBInstance_iops :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_iops = Lens.lens (\CreateDBInstance' {iops} -> iops) (\s@CreateDBInstance' {} a -> s {iops = a} :: CreateDBInstance)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the DB instance during the maintenance window. By
-- default, minor engine upgrades are applied automatically.
createDBInstance_autoMinorVersionUpgrade :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_autoMinorVersionUpgrade = Lens.lens (\CreateDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstance)

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
createDBInstance_dbInstanceIdentifier :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbInstanceIdentifier = Lens.lens (\CreateDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@CreateDBInstance' {} a -> s {dbInstanceIdentifier = a} :: CreateDBInstance)

-- | The compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
createDBInstance_dbInstanceClass :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbInstanceClass = Lens.lens (\CreateDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@CreateDBInstance' {} a -> s {dbInstanceClass = a} :: CreateDBInstance)

-- | The name of the database engine to be used for this instance.
--
-- Not every database engine is available for every AWS Region.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
createDBInstance_engine :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_engine = Lens.lens (\CreateDBInstance' {engine} -> engine) (\s@CreateDBInstance' {} a -> s {engine = a} :: CreateDBInstance)

instance Core.AWSRequest CreateDBInstance where
  type
    AWSResponse CreateDBInstance =
      CreateDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceResult"
      ( \s h x ->
          CreateDBInstanceResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBInstance

instance Prelude.NFData CreateDBInstance

instance Core.ToHeaders CreateDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDBInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDBInstance where
  toQuery CreateDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateDBInstance" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "EnablePerformanceInsights"
          Core.=: enablePerformanceInsights,
        "DBSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroups
            ),
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "StorageType" Core.=: storageType,
        "MonitoringInterval" Core.=: monitoringInterval,
        "OptionGroupName" Core.=: optionGroupName,
        "Domain" Core.=: domain,
        "MonitoringRoleArn" Core.=: monitoringRoleArn,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "NcharCharacterSetName"
          Core.=: ncharCharacterSetName,
        "MasterUserPassword" Core.=: masterUserPassword,
        "MasterUsername" Core.=: masterUsername,
        "MultiAZ" Core.=: multiAZ,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "PerformanceInsightsKMSKeyId"
          Core.=: performanceInsightsKMSKeyId,
        "KmsKeyId" Core.=: kmsKeyId,
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "AvailabilityZone" Core.=: availabilityZone,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "EngineVersion" Core.=: engineVersion,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "CharacterSetName" Core.=: characterSetName,
        "PerformanceInsightsRetentionPeriod"
          Core.=: performanceInsightsRetentionPeriod,
        "LicenseModel" Core.=: licenseModel,
        "TdeCredentialPassword"
          Core.=: tdeCredentialPassword,
        "PromotionTier" Core.=: promotionTier,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "Port" Core.=: port,
        "DBName" Core.=: dbName,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "TdeCredentialArn" Core.=: tdeCredentialArn,
        "EnableCustomerOwnedIp"
          Core.=: enableCustomerOwnedIp,
        "Timezone" Core.=: timezone,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "AllocatedStorage" Core.=: allocatedStorage,
        "Iops" Core.=: iops,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "Engine" Core.=: engine
      ]

-- | /See:/ 'newCreateDBInstanceResponse' smart constructor.
data CreateDBInstanceResponse = CreateDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'createDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'createDBInstanceResponse_httpStatus' - The response's http status code.
newCreateDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBInstanceResponse
newCreateDBInstanceResponse pHttpStatus_ =
  CreateDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBInstanceResponse_dbInstance :: Lens.Lens' CreateDBInstanceResponse (Prelude.Maybe DBInstance)
createDBInstanceResponse_dbInstance = Lens.lens (\CreateDBInstanceResponse' {dbInstance} -> dbInstance) (\s@CreateDBInstanceResponse' {} a -> s {dbInstance = a} :: CreateDBInstanceResponse)

-- | The response's http status code.
createDBInstanceResponse_httpStatus :: Lens.Lens' CreateDBInstanceResponse Prelude.Int
createDBInstanceResponse_httpStatus = Lens.lens (\CreateDBInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateDBInstanceResponse' {} a -> s {httpStatus = a} :: CreateDBInstanceResponse)

instance Prelude.NFData CreateDBInstanceResponse
