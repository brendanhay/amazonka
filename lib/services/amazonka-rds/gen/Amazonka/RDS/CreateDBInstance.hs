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
-- Module      : Amazonka.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
--
-- The new DB instance can be an RDS DB instance, or it can be a DB
-- instance in an Aurora DB cluster. For an Aurora DB cluster, you can call
-- this operation multiple times to add more than one DB instance to the
-- cluster.
--
-- For more information about creating an RDS DB instance, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CreateDBInstance.html Creating an Amazon RDS DB instance>
-- in the /Amazon RDS User Guide/.
--
-- For more information about creating a DB instance in an Aurora DB
-- cluster, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.CreateInstance.html Creating an Amazon Aurora DB cluster>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDS.CreateDBInstance
  ( -- * Creating a Request
    CreateDBInstance (..),
    newCreateDBInstance,

    -- * Request Lenses
    createDBInstance_allocatedStorage,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_availabilityZone,
    createDBInstance_backupRetentionPeriod,
    createDBInstance_backupTarget,
    createDBInstance_cACertificateIdentifier,
    createDBInstance_characterSetName,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_customIamInstanceProfile,
    createDBInstance_dbClusterIdentifier,
    createDBInstance_dbName,
    createDBInstance_dbParameterGroupName,
    createDBInstance_dbSecurityGroups,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_deletionProtection,
    createDBInstance_domain,
    createDBInstance_domainIAMRoleName,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_enableCustomerOwnedIp,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_engineVersion,
    createDBInstance_iops,
    createDBInstance_kmsKeyId,
    createDBInstance_licenseModel,
    createDBInstance_manageMasterUserPassword,
    createDBInstance_masterUserPassword,
    createDBInstance_masterUserSecretKmsKeyId,
    createDBInstance_masterUsername,
    createDBInstance_maxAllocatedStorage,
    createDBInstance_monitoringInterval,
    createDBInstance_monitoringRoleArn,
    createDBInstance_multiAZ,
    createDBInstance_ncharCharacterSetName,
    createDBInstance_networkType,
    createDBInstance_optionGroupName,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_performanceInsightsRetentionPeriod,
    createDBInstance_port,
    createDBInstance_preferredBackupWindow,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_processorFeatures,
    createDBInstance_promotionTier,
    createDBInstance_publiclyAccessible,
    createDBInstance_storageEncrypted,
    createDBInstance_storageThroughput,
    createDBInstance_storageType,
    createDBInstance_tags,
    createDBInstance_tdeCredentialArn,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_timezone,
    createDBInstance_vpcSecurityGroupIds,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { -- | The amount of storage in gibibytes (GiB) to allocate for the DB
    -- instance.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. Aurora
    -- cluster volumes automatically grow as the amount of data in your
    -- database increases, though you are only charged for the space that you
    -- use in an Aurora cluster volume.
    --
    -- [Amazon RDS Custom
    -- RDS for MariaDB
    -- RDS for MySQL
    -- RDS for Oracle
    -- RDS for PostgreSQL
    -- RDS for SQL Server]
    --     Constraints to the amount of storage for each storage type are the
    --     following:
    --
    --     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
    --         from 40 to 65536 for RDS Custom for Oracle, 16384 for RDS Custom
    --         for SQL Server.
    --
    --     -   Provisioned IOPS storage (io1): Must be an integer from 40 to
    --         65536 for RDS Custom for Oracle, 16384 for RDS Custom for SQL
    --         Server.
    --
    --     Constraints to the amount of storage for each storage type are the
    --     following:
    --
    --     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
    --         from 20 to 65536.
    --
    --     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --         65536.
    --
    --     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --     Constraints to the amount of storage for each storage type are the
    --     following:
    --
    --     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
    --         from 20 to 65536.
    --
    --     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --         65536.
    --
    --     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --     Constraints to the amount of storage for each storage type are the
    --     following:
    --
    --     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
    --         from 20 to 65536.
    --
    --     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --         65536.
    --
    --     -   Magnetic storage (standard): Must be an integer from 10 to 3072.
    --
    --     Constraints to the amount of storage for each storage type are the
    --     following:
    --
    --     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
    --         from 20 to 65536.
    --
    --     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
    --         65536.
    --
    --     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
    --
    --     Constraints to the amount of storage for each storage type are the
    --     following:
    --
    --     -   General Purpose (SSD) storage (gp2, gp3):
    --
    --         -   Enterprise and Standard editions: Must be an integer from 20
    --             to 16384.
    --
    --         -   Web and Express editions: Must be an integer from 20 to
    --             16384.
    --
    --     -   Provisioned IOPS storage (io1):
    --
    --         -   Enterprise and Standard editions: Must be an integer from
    --             100 to 16384.
    --
    --         -   Web and Express editions: Must be an integer from 100 to
    --             16384.
    --
    --     -   Magnetic storage (standard):
    --
    --         -   Enterprise and Standard editions: Must be an integer from 20
    --             to 1024.
    --
    --         -   Web and Express editions: Must be an integer from 20 to
    --             1024.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether minor engine upgrades are applied automatically to the
    -- DB instance during the maintenance window. By default, minor engine
    -- upgrades are applied automatically.
    --
    -- If you create an RDS Custom DB instance, you must set
    -- @AutoMinorVersionUpgrade@ to @false@.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone (AZ) where the database will be created. For
    -- information on Amazon Web Services Regions and Availability Zones, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
    --
    -- For Amazon Aurora, each Aurora DB cluster hosts copies of its storage in
    -- three separate Availability Zones. Specify one of these Availability
    -- Zones. Aurora automatically chooses an appropriate Availability Zone if
    -- you don\'t specify one.
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- Amazon Web Services Region.
    --
    -- Constraints:
    --
    -- -   The @AvailabilityZone@ parameter can\'t be specified if the DB
    --     instance is a Multi-AZ deployment.
    --
    -- -   The specified Availability Zone must be in the same Amazon Web
    --     Services Region as the current endpoint.
    --
    -- Example: @us-east-1d@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which automated backups are retained. Setting
    -- this parameter to a positive number enables backups. Setting this
    -- parameter to @0@ disables automated backups.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The retention
    -- period for automated backups is managed by the DB cluster.
    --
    -- Default: @1@
    --
    -- Constraints:
    --
    -- -   Must be a value from 0 to 35.
    --
    -- -   Can\'t be set to 0 if the DB instance is a source to read replicas.
    --
    -- -   Can\'t be set to 0 for an RDS Custom for Oracle DB instance.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The location for storing automated backups and manual snapshots.
    --
    -- Valie Values:
    --
    -- -   @outposts@ (Amazon Web Services Outposts)
    --
    -- -   @region@ (Amazon Web Services Region)
    --
    -- Default: @region@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide/.
    backupTarget :: Prelude.Maybe Prelude.Text,
    -- | The CA certificate identifier to use for the DB instance\'s server
    -- certificate.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
    -- in the /Amazon Aurora User Guide/.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | For supported engines, the character set (@CharacterSet@) to associate
    -- the DB instance with.
    --
    -- This setting doesn\'t apply to the following DB instances:
    --
    -- -   Amazon Aurora - The character set is managed by the DB cluster. For
    --     more information, see @CreateDBCluster@.
    --
    -- -   RDS Custom - However, if you need to change the character set, you
    --     can change it on the database itself.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | Spcifies whether to copy tags from the DB instance to snapshots of the
    -- DB instance. By default, tags are not copied.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. Copying tags
    -- to snapshots is managed by the DB cluster. Setting this value for an
    -- Aurora DB instance has no effect on the DB cluster setting.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The instance profile associated with the underlying Amazon EC2 instance
    -- of an RDS Custom DB instance.
    --
    -- This setting is required for RDS Custom.
    --
    -- Constraints:
    --
    -- -   The profile must exist in your account.
    --
    -- -   The profile must have an IAM role that Amazon EC2 has permissions to
    --     assume.
    --
    -- -   The instance profile name and the associated IAM role name must
    --     start with the prefix @AWSRDSCustom@.
    --
    -- For the list of permissions required for the IAM role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
    -- in the /Amazon RDS User Guide/.
    customIamInstanceProfile :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the DB cluster that this DB instance will belong to.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The meaning of this parameter differs depending on the database engine.
    --
    -- [Amazon Aurora MySQL
    -- Amazon Aurora PostgreSQL
    -- Amazon RDS Custom for Oracle
    -- Amazon RDS Custom for SQL Server
    -- RDS for MariaDB
    -- RDS for MySQL
    -- RDS for Oracle
    -- RDS for PostgreSQL
    -- RDS for SQL Server]
    --     The name of the database to create when the primary DB instance of
    --     the Aurora MySQL DB cluster is created. If you don\'t specify a
    --     value, Amazon RDS doesn\'t create a database in the DB cluster.
    --
    --     Constraints:
    --
    --     -   Must contain 1 to 64 alphanumeric characters.
    --
    --     -   Can\'t be a word reserved by the database engine.
    --
    --     The name of the database to create when the primary DB instance of
    --     the Aurora PostgreSQL DB cluster is created.
    --
    --     Default: @postgres@
    --
    --     Constraints:
    --
    --     -   Must contain 1 to 63 alphanumeric characters.
    --
    --     -   Must begin with a letter. Subsequent characters can be letters,
    --         underscores, or digits (0 to 9).
    --
    --     -   Can\'t be a word reserved by the database engine.
    --
    --     The Oracle System ID (SID) of the created RDS Custom DB instance.
    --
    --     Default: @ORCL@
    --
    --     Constraints:
    --
    --     -   Must contain 1 to 8 alphanumeric characters.
    --
    --     -   Must contain a letter.
    --
    --     -   Can\'t be a word reserved by the database engine.
    --
    --     Not applicable. Must be null.
    --
    --     The name of the database to create when the DB instance is created.
    --     If you don\'t specify a value, Amazon RDS doesn\'t create a database
    --     in the DB instance.
    --
    --     Constraints:
    --
    --     -   Must contain 1 to 64 letters or numbers.
    --
    --     -   Must begin with a letter. Subsequent characters can be letters,
    --         underscores, or digits (0-9).
    --
    --     -   Can\'t be a word reserved by the database engine.
    --
    --     The name of the database to create when the DB instance is created.
    --     If you don\'t specify a value, Amazon RDS doesn\'t create a database
    --     in the DB instance.
    --
    --     Constraints:
    --
    --     -   Must contain 1 to 64 letters or numbers.
    --
    --     -   Must begin with a letter. Subsequent characters can be letters,
    --         underscores, or digits (0-9).
    --
    --     -   Can\'t be a word reserved by the database engine.
    --
    --     The Oracle System ID (SID) of the created DB instance.
    --
    --     Default: @ORCL@
    --
    --     Constraints:
    --
    --     -   Can\'t be longer than 8 characters.
    --
    --     -   Can\'t be a word reserved by the database engine, such as the
    --         string @NULL@.
    --
    --     The name of the database to create when the DB instance is created.
    --
    --     Default: @postgres@
    --
    --     Constraints:
    --
    --     -   Must contain 1 to 63 letters, numbers, or underscores.
    --
    --     -   Must begin with a letter. Subsequent characters can be letters,
    --         underscores, or digits (0-9).
    --
    --     -   Can\'t be a word reserved by the database engine.
    --
    --     Not applicable. Must be null.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    -- If you don\'t specify a value, then Amazon RDS uses the default DB
    -- parameter group for the specified DB engine and version.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- This setting applies to the legacy EC2-Classic platform, which is no
    -- longer used to create new DB instances. Use the @VpcSecurityGroupIds@
    -- setting instead.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | A DB subnet group to associate with this DB instance.
    --
    -- Constraints:
    --
    -- -   Must match the name of an existing DB subnet group.
    --
    -- -   Must not be @default@.
    --
    -- Example: @mydbsubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB instance has deletion protection enabled. The
    -- database can\'t be deleted when deletion protection is enabled. By
    -- default, deletion protection isn\'t enabled. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. You can
    -- enable or disable deletion protection for the DB cluster. For more
    -- information, see @CreateDBCluster@. DB instances in a DB cluster can be
    -- deleted even when deletion protection is enabled for the DB cluster.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Active Directory directory ID to create the DB instance in.
    -- Currently, only Microsoft SQL Server, MySQL, Oracle, and PostgreSQL DB
    -- instances can be created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to the following DB instances:
    --
    -- -   Amazon Aurora (The domain is managed by the DB cluster.)
    --
    -- -   RDS Custom
    domain :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role to use when making API calls to the Directory
    -- Service.
    --
    -- This setting doesn\'t apply to the following DB instances:
    --
    -- -   Amazon Aurora (The domain is managed by the DB cluster.)
    --
    -- -   RDS Custom
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of log types that need to be enabled for exporting to
    -- CloudWatch Logs. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to the following DB instances:
    --
    -- -   Amazon Aurora (CloudWatch Logs exports are managed by the DB
    --     cluster.)
    --
    -- -   RDS Custom
    --
    -- The following values are valid for each DB engine:
    --
    -- -   RDS for MariaDB - @audit | error | general | slowquery@
    --
    -- -   RDS for Microsoft SQL Server - @agent | error@
    --
    -- -   RDS for MySQL - @audit | error | general | slowquery@
    --
    -- -   RDS for Oracle - @alert | audit | listener | trace | oemagent@
    --
    -- -   RDS for PostgreSQL - @postgresql | upgrade@
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether to enable a customer-owned IP address (CoIP) for an
    -- RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
    -- in the /Amazon Web Services Outposts User Guide/.
    enableCustomerOwnedIp :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to enable mapping of Amazon Web Services Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping isn\'t enabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to the following DB instances:
    --
    -- -   Amazon Aurora (Mapping Amazon Web Services IAM accounts to database
    --     accounts is managed by the DB cluster.)
    --
    -- -   RDS Custom
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to enable Performance Insights for the DB instance.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to use.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The version
    -- number of the database engine the DB instance uses is managed by the DB
    -- cluster.
    --
    -- For a list of valid engine versions, use the @DescribeDBEngineVersions@
    -- operation.
    --
    -- The following are the database engines and links to information about
    -- the major and minor versions that are available with Amazon RDS. Not
    -- every database engine is available for every Amazon Web Services Region.
    --
    -- [Amazon RDS Custom for Oracle
    -- Amazon RDS Custom for SQL Server
    -- RDS for MariaDB
    -- RDS for Microsoft SQL Server
    -- RDS for MySQL
    -- RDS for Oracle
    -- RDS for PostgreSQL]
    --     A custom engine version (CEV) that you have previously created. This
    --     setting is required for RDS Custom for Oracle. The CEV name has the
    --     following format: 19./customized_string/. A valid CEV name is
    --     @19.my_cev1@. For more information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-creating.html#custom-creating.create Creating an RDS Custom for Oracle DB instance>
    --     in the /Amazon RDS User Guide/.
    --
    --     See
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-reqs-limits-MS.html RDS Custom for SQL Server general requirements>
    --     in the /Amazon RDS User Guide/.
    --
    --     For information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS versions>
    --     in the /Amazon RDS User Guide/.
    --
    --     For information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server versions on Amazon RDS>
    --     in the /Amazon RDS User Guide/.
    --
    --     For information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS versions>
    --     in the /Amazon RDS User Guide/.
    --
    --     For information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine release notes>
    --     in the /Amazon RDS User Guide/.
    --
    --     For information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
    --     in the /Amazon RDS User Guide/.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- initially allocate for the DB instance. For information about valid IOPS
    -- values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html Amazon RDS DB instance storage>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. Storage is
    -- managed by the DB cluster.
    --
    -- Constraints:
    --
    -- -   For RDS for MariaDB, MySQL, Oracle, and PostgreSQL - Must be a
    --     multiple between .5 and 50 of the storage amount for the DB
    --     instance.
    --
    -- -   For RDS for SQL Server - Must be a multiple between 1 and 50 of the
    --     storage amount for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB instance.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The Amazon
    -- Web Services KMS key identifier is managed by the DB cluster. For more
    -- information, see @CreateDBCluster@.
    --
    -- If @StorageEncrypted@ is enabled, and you do not specify a value for the
    -- @KmsKeyId@ parameter, then Amazon RDS uses your default KMS key. There
    -- is a default KMS key for your Amazon Web Services account. Your Amazon
    -- Web Services account has a different default KMS key for each Amazon Web
    -- Services Region.
    --
    -- For Amazon RDS Custom, a KMS key is required for DB instances. For most
    -- RDS engines, if you leave this parameter empty while enabling
    -- @StorageEncrypted@, the engine uses the default KMS key. However, RDS
    -- Custom doesn\'t use the default key when this parameter is empty. You
    -- must explicitly specify a key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The license model information for this DB instance.
    --
    -- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
    --
    -- Valid Values:
    --
    -- -   RDS for MariaDB - @general-public-license@
    --
    -- -   RDS for Microsoft SQL Server - @license-included@
    --
    -- -   RDS for MySQL - @general-public-license@
    --
    -- -   RDS for Oracle - @bring-your-own-license | license-included@
    --
    -- -   RDS for PostgreSQL - @postgresql-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to manage the master user password with Amazon Web
    -- Services Secrets Manager.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon RDS User Guide./
    --
    -- Constraints:
    --
    -- -   Can\'t manage the master user password with Amazon Web Services
    --     Secrets Manager if @MasterUserPassword@ is specified.
    manageMasterUserPassword :: Prelude.Maybe Prelude.Bool,
    -- | The password for the master user.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The password
    -- for the master user is managed by the DB cluster.
    --
    -- Constraints:
    --
    -- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
    --
    -- -   Can include any printable ASCII character except \"\/\", \"\"\", or
    --     \"\@\".
    --
    -- Length Constraints:
    --
    -- -   RDS for MariaDB - Must contain from 8 to 41 characters.
    --
    -- -   RDS for Microsoft SQL Server - Must contain from 8 to 128
    --     characters.
    --
    -- -   RDS for MySQL - Must contain from 8 to 41 characters.
    --
    -- -   RDS for Oracle - Must contain from 8 to 30 characters.
    --
    -- -   RDS for PostgreSQL - Must contain from 8 to 128 characters.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier to encrypt a secret that is
    -- automatically generated and managed in Amazon Web Services Secrets
    -- Manager.
    --
    -- This setting is valid only if the master user password is managed by RDS
    -- in Amazon Web Services Secrets Manager for the DB instance.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
    -- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
    -- secret is in a different Amazon Web Services account, then you can\'t
    -- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
    -- must use a customer managed KMS key.
    --
    -- There is a default KMS key for your Amazon Web Services account. Your
    -- Amazon Web Services account has a different default KMS key for each
    -- Amazon Web Services Region.
    masterUserSecretKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name for the master user.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The name for
    -- the master user is managed by the DB cluster.
    --
    -- This setting is required for RDS DB instances.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 16 letters, numbers, or underscores.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to the following DB instances:
    --
    -- -   Amazon Aurora (Storage is managed by the DB cluster.)
    --
    -- -   RDS Custom
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance. To disable collection of
    -- Enhanced Monitoring metrics, specify @0@.
    --
    -- If @MonitoringRoleArn@ is specified, then you must set
    -- @MonitoringInterval@ to a value other than @0@.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    --
    -- Valid Values: @0 | 1 | 5 | 10 | 15 | 30 | 60@
    --
    -- Default: @0@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
    -- in the /Amazon RDS User Guide/.
    --
    -- If @MonitoringInterval@ is set to a value other than @0@, then you must
    -- supply a @MonitoringRoleArn@ value.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the DB instance is a Multi-AZ deployment. You can\'t
    -- set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ
    -- deployment.
    --
    -- This setting doesn\'t apply to the following DB instances:
    --
    -- -   Amazon Aurora (DB instance Availability Zones (AZs) are managed by
    --     the DB cluster.)
    --
    -- -   RDS Custom
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The name of the NCHAR character set for the Oracle DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    ncharCharacterSetName :: Prelude.Maybe Prelude.Text,
    -- | The network type of the DB instance.
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for the
    -- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon RDS User Guide./
    --
    -- Valid Values: @IPV4 | DUAL@
    networkType :: Prelude.Maybe Prelude.Text,
    -- | The option group to associate the DB instance with.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group. Also, that option group
    -- can\'t be removed from a DB instance after it is associated with a DB
    -- instance.
    --
    -- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- If you don\'t specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default KMS key. There is a default KMS key for
    -- your Amazon Web Services account. Your Amazon Web Services account has a
    -- different default KMS key for each Amazon Web Services Region.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain Performance Insights data.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    --
    -- Valid Values:
    --
    -- -   @7@
    --
    -- -   /month/ * 31, where /month/ is a number of months from 1-23.
    --     Examples: @93@ (3 months * 31), @341@ (11 months * 31), @589@ (19
    --     months * 31)
    --
    -- -   @731@
    --
    -- Default: @7@ days
    --
    -- If you specify a retention period that isn\'t valid, such as @94@,
    -- Amazon RDS returns an error.
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The port number on which the database accepts connections.
    --
    -- This setting doesn\'t apply to Aurora DB instances. The port number is
    -- managed by the cluster.
    --
    -- Valid Values: @1150-65535@
    --
    -- Default:
    --
    -- -   RDS for MariaDB - @3306@
    --
    -- -   RDS for Microsoft SQL Server - @1433@
    --
    -- -   RDS for MySQL - @3306@
    --
    -- -   RDS for Oracle - @1521@
    --
    -- -   RDS for PostgreSQL - @5432@
    --
    -- Constraints:
    --
    -- -   For RDS for Microsoft SQL Server, the value can\'t be @1234@,
    --     @1434@, @3260@, @3343@, @3389@, @47001@, or @49152-49156@.
    port :: Prelude.Maybe Prelude.Int,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, using the @BackupRetentionPeriod@
    -- parameter. The default is a 30-minute window selected at random from an
    -- 8-hour block of time for each Amazon Web Services Region. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The daily
    -- time range for creating automated backups is managed by the DB cluster.
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
    -- | The time range each week during which system maintenance can occur. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
    -- in the /Amazon RDS User Guide./
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week.
    --
    -- Constraints:
    --
    -- -   Must be in the format @ddd:hh24:mi-ddd:hh24:mi@.
    --
    -- -   The day values must be @mon | tue | wed | thu | fri | sat | sun@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred backup window.
    --
    -- -   Must be at least 30 minutes.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    --
    -- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The order of priority in which an Aurora Replica is promoted to the
    -- primary instance after a failure of the existing primary instance. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.AuroraHighAvailability.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    --
    -- Default: @1@
    --
    -- Valid Values: @0 - 15@
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its Domain Name System
    -- (DNS) endpoint resolves to the private IP address from within the DB
    -- instance\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB instance\'s VPC. Access to the DB
    -- instance is ultimately controlled by the security group it uses. That
    -- public access is not permitted if the security group assigned to the DB
    -- instance doesn\'t permit it.
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
    -- -   If the default VPC in the target Region doesn’t have an internet
    --     gateway attached to it, the DB instance is private.
    --
    -- -   If the default VPC in the target Region has an internet gateway
    --     attached to it, the DB instance is public.
    --
    -- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the subnets are part of a VPC that doesn’t have an internet
    --     gateway attached to it, the DB instance is private.
    --
    -- -   If the subnets are part of a VPC that has an internet gateway
    --     attached to it, the DB instance is public.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Specifes whether the DB instance is encrypted. By default, it isn\'t
    -- encrypted.
    --
    -- For RDS Custom DB instances, either enable this setting or leave it
    -- unset. Otherwise, Amazon RDS reports an error.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The
    -- encryption for DB instances is managed by the DB cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The storage throughput value for the DB instance.
    --
    -- This setting applies only to the @gp3@ storage type.
    --
    -- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | The storage type to associate with the DB instance.
    --
    -- If you specify @io1@ or @gp3@, you must also include a value for the
    -- @Iops@ parameter.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. Storage is
    -- managed by the DB cluster.
    --
    -- Valid Values: @gp2 | gp3 | io1 | standard@
    --
    -- Default: @io1@, if the @Iops@ parameter is specified. Otherwise, @gp2@.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Tags to assign to the DB instance.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    --
    -- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    --
    -- This setting doesn\'t apply to RDS Custom DB instances.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB instance. The time zone parameter is currently
    -- supported only by
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server>.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | A list of Amazon EC2 VPC security groups to associate with this DB
    -- instance.
    --
    -- This setting doesn\'t apply to Amazon Aurora DB instances. The
    -- associated list of EC2 VPC security groups is managed by the DB cluster.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier for this DB instance. This parameter is stored as a
    -- lowercase string.
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
    -- | The compute and memory capacity of the DB instance, for example
    -- @db.m5.large@. Not all DB instance classes are available in all Amazon
    -- Web Services Regions, or for all database engines. For the full list of
    -- DB instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance classes>
    -- in the /Amazon RDS User Guide/ or
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.DBInstanceClass.html Aurora DB instance classes>
    -- in the /Amazon Aurora User Guide/.
    dbInstanceClass :: Prelude.Text,
    -- | The database engine to use for this DB instance.
    --
    -- Not every database engine is available in every Amazon Web Services
    -- Region.
    --
    -- Valid Values:
    --
    -- -   @aurora-mysql@ (for Aurora MySQL DB instances)
    --
    -- -   @aurora-postgresql@ (for Aurora PostgreSQL DB instances)
    --
    -- -   @custom-oracle-ee@ (for RDS Custom for Oracle DB instances)
    --
    -- -   @custom-oracle-ee-cdb@ (for RDS Custom for Oracle DB instances)
    --
    -- -   @custom-sqlserver-ee@ (for RDS Custom for SQL Server DB instances)
    --
    -- -   @custom-sqlserver-se@ (for RDS Custom for SQL Server DB instances)
    --
    -- -   @custom-sqlserver-web@ (for RDS Custom for SQL Server DB instances)
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-ee-cdb@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se2-cdb@
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
-- 'allocatedStorage', 'createDBInstance_allocatedStorage' - The amount of storage in gibibytes (GiB) to allocate for the DB
-- instance.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Aurora
-- cluster volumes automatically grow as the amount of data in your
-- database increases, though you are only charged for the space that you
-- use in an Aurora cluster volume.
--
-- [Amazon RDS Custom
-- RDS for MariaDB
-- RDS for MySQL
-- RDS for Oracle
-- RDS for PostgreSQL
-- RDS for SQL Server]
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 40 to 65536 for RDS Custom for Oracle, 16384 for RDS Custom
--         for SQL Server.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 40 to
--         65536 for RDS Custom for Oracle, 16384 for RDS Custom for SQL
--         Server.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 10 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3):
--
--         -   Enterprise and Standard editions: Must be an integer from 20
--             to 16384.
--
--         -   Web and Express editions: Must be an integer from 20 to
--             16384.
--
--     -   Provisioned IOPS storage (io1):
--
--         -   Enterprise and Standard editions: Must be an integer from
--             100 to 16384.
--
--         -   Web and Express editions: Must be an integer from 100 to
--             16384.
--
--     -   Magnetic storage (standard):
--
--         -   Enterprise and Standard editions: Must be an integer from 20
--             to 1024.
--
--         -   Web and Express editions: Must be an integer from 20 to
--             1024.
--
-- 'autoMinorVersionUpgrade', 'createDBInstance_autoMinorVersionUpgrade' - Specifies whether minor engine upgrades are applied automatically to the
-- DB instance during the maintenance window. By default, minor engine
-- upgrades are applied automatically.
--
-- If you create an RDS Custom DB instance, you must set
-- @AutoMinorVersionUpgrade@ to @false@.
--
-- 'availabilityZone', 'createDBInstance_availabilityZone' - The Availability Zone (AZ) where the database will be created. For
-- information on Amazon Web Services Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- For Amazon Aurora, each Aurora DB cluster hosts copies of its storage in
-- three separate Availability Zones. Specify one of these Availability
-- Zones. Aurora automatically chooses an appropriate Availability Zone if
-- you don\'t specify one.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Constraints:
--
-- -   The @AvailabilityZone@ parameter can\'t be specified if the DB
--     instance is a Multi-AZ deployment.
--
-- -   The specified Availability Zone must be in the same Amazon Web
--     Services Region as the current endpoint.
--
-- Example: @us-east-1d@
--
-- 'backupRetentionPeriod', 'createDBInstance_backupRetentionPeriod' - The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to @0@ disables automated backups.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The retention
-- period for automated backups is managed by the DB cluster.
--
-- Default: @1@
--
-- Constraints:
--
-- -   Must be a value from 0 to 35.
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas.
--
-- -   Can\'t be set to 0 for an RDS Custom for Oracle DB instance.
--
-- 'backupTarget', 'createDBInstance_backupTarget' - The location for storing automated backups and manual snapshots.
--
-- Valie Values:
--
-- -   @outposts@ (Amazon Web Services Outposts)
--
-- -   @region@ (Amazon Web Services Region)
--
-- Default: @region@
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- 'cACertificateIdentifier', 'createDBInstance_cACertificateIdentifier' - The CA certificate identifier to use for the DB instance\'s server
-- certificate.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
--
-- 'characterSetName', 'createDBInstance_characterSetName' - For supported engines, the character set (@CharacterSet@) to associate
-- the DB instance with.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora - The character set is managed by the DB cluster. For
--     more information, see @CreateDBCluster@.
--
-- -   RDS Custom - However, if you need to change the character set, you
--     can change it on the database itself.
--
-- 'copyTagsToSnapshot', 'createDBInstance_copyTagsToSnapshot' - Spcifies whether to copy tags from the DB instance to snapshots of the
-- DB instance. By default, tags are not copied.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Copying tags
-- to snapshots is managed by the DB cluster. Setting this value for an
-- Aurora DB instance has no effect on the DB cluster setting.
--
-- 'customIamInstanceProfile', 'createDBInstance_customIamInstanceProfile' - The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance.
--
-- This setting is required for RDS Custom.
--
-- Constraints:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
--
-- 'dbClusterIdentifier', 'createDBInstance_dbClusterIdentifier' - The identifier of the DB cluster that this DB instance will belong to.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- 'dbName', 'createDBInstance_dbName' - The meaning of this parameter differs depending on the database engine.
--
-- [Amazon Aurora MySQL
-- Amazon Aurora PostgreSQL
-- Amazon RDS Custom for Oracle
-- Amazon RDS Custom for SQL Server
-- RDS for MariaDB
-- RDS for MySQL
-- RDS for Oracle
-- RDS for PostgreSQL
-- RDS for SQL Server]
--     The name of the database to create when the primary DB instance of
--     the Aurora MySQL DB cluster is created. If you don\'t specify a
--     value, Amazon RDS doesn\'t create a database in the DB cluster.
--
--     Constraints:
--
--     -   Must contain 1 to 64 alphanumeric characters.
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The name of the database to create when the primary DB instance of
--     the Aurora PostgreSQL DB cluster is created.
--
--     Default: @postgres@
--
--     Constraints:
--
--     -   Must contain 1 to 63 alphanumeric characters.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0 to 9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The Oracle System ID (SID) of the created RDS Custom DB instance.
--
--     Default: @ORCL@
--
--     Constraints:
--
--     -   Must contain 1 to 8 alphanumeric characters.
--
--     -   Must contain a letter.
--
--     -   Can\'t be a word reserved by the database engine.
--
--     Not applicable. Must be null.
--
--     The name of the database to create when the DB instance is created.
--     If you don\'t specify a value, Amazon RDS doesn\'t create a database
--     in the DB instance.
--
--     Constraints:
--
--     -   Must contain 1 to 64 letters or numbers.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0-9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The name of the database to create when the DB instance is created.
--     If you don\'t specify a value, Amazon RDS doesn\'t create a database
--     in the DB instance.
--
--     Constraints:
--
--     -   Must contain 1 to 64 letters or numbers.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0-9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The Oracle System ID (SID) of the created DB instance.
--
--     Default: @ORCL@
--
--     Constraints:
--
--     -   Can\'t be longer than 8 characters.
--
--     -   Can\'t be a word reserved by the database engine, such as the
--         string @NULL@.
--
--     The name of the database to create when the DB instance is created.
--
--     Default: @postgres@
--
--     Constraints:
--
--     -   Must contain 1 to 63 letters, numbers, or underscores.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0-9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     Not applicable. Must be null.
--
-- 'dbParameterGroupName', 'createDBInstance_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
-- If you don\'t specify a value, then Amazon RDS uses the default DB
-- parameter group for the specified DB engine and version.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'dbSecurityGroups', 'createDBInstance_dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- This setting applies to the legacy EC2-Classic platform, which is no
-- longer used to create new DB instances. Use the @VpcSecurityGroupIds@
-- setting instead.
--
-- 'dbSubnetGroupName', 'createDBInstance_dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- Constraints:
--
-- -   Must match the name of an existing DB subnet group.
--
-- -   Must not be @default@.
--
-- Example: @mydbsubnetgroup@
--
-- 'deletionProtection', 'createDBInstance_deletionProtection' - Specifies whether the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. By
-- default, deletion protection isn\'t enabled. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. You can
-- enable or disable deletion protection for the DB cluster. For more
-- information, see @CreateDBCluster@. DB instances in a DB cluster can be
-- deleted even when deletion protection is enabled for the DB cluster.
--
-- 'domain', 'createDBInstance_domain' - The Active Directory directory ID to create the DB instance in.
-- Currently, only Microsoft SQL Server, MySQL, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (The domain is managed by the DB cluster.)
--
-- -   RDS Custom
--
-- 'domainIAMRoleName', 'createDBInstance_domainIAMRoleName' - The name of the IAM role to use when making API calls to the Directory
-- Service.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (The domain is managed by the DB cluster.)
--
-- -   RDS Custom
--
-- 'enableCloudwatchLogsExports', 'createDBInstance_enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (CloudWatch Logs exports are managed by the DB
--     cluster.)
--
-- -   RDS Custom
--
-- The following values are valid for each DB engine:
--
-- -   RDS for MariaDB - @audit | error | general | slowquery@
--
-- -   RDS for Microsoft SQL Server - @agent | error@
--
-- -   RDS for MySQL - @audit | error | general | slowquery@
--
-- -   RDS for Oracle - @alert | audit | listener | trace | oemagent@
--
-- -   RDS for PostgreSQL - @postgresql | upgrade@
--
-- 'enableCustomerOwnedIp', 'createDBInstance_enableCustomerOwnedIp' - Specifies whether to enable a customer-owned IP address (CoIP) for an
-- RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'createDBInstance_enableIAMDatabaseAuthentication' - Specifies whether to enable mapping of Amazon Web Services Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (Mapping Amazon Web Services IAM accounts to database
--     accounts is managed by the DB cluster.)
--
-- -   RDS Custom
--
-- 'enablePerformanceInsights', 'createDBInstance_enablePerformanceInsights' - Specifies whether to enable Performance Insights for the DB instance.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- 'engineVersion', 'createDBInstance_engineVersion' - The version number of the database engine to use.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The version
-- number of the database engine the DB instance uses is managed by the DB
-- cluster.
--
-- For a list of valid engine versions, use the @DescribeDBEngineVersions@
-- operation.
--
-- The following are the database engines and links to information about
-- the major and minor versions that are available with Amazon RDS. Not
-- every database engine is available for every Amazon Web Services Region.
--
-- [Amazon RDS Custom for Oracle
-- Amazon RDS Custom for SQL Server
-- RDS for MariaDB
-- RDS for Microsoft SQL Server
-- RDS for MySQL
-- RDS for Oracle
-- RDS for PostgreSQL]
--     A custom engine version (CEV) that you have previously created. This
--     setting is required for RDS Custom for Oracle. The CEV name has the
--     following format: 19./customized_string/. A valid CEV name is
--     @19.my_cev1@. For more information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-creating.html#custom-creating.create Creating an RDS Custom for Oracle DB instance>
--     in the /Amazon RDS User Guide/.
--
--     See
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-reqs-limits-MS.html RDS Custom for SQL Server general requirements>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS versions>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server versions on Amazon RDS>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS versions>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine release notes>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
--     in the /Amazon RDS User Guide/.
--
-- 'iops', 'createDBInstance_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- initially allocate for the DB instance. For information about valid IOPS
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html Amazon RDS DB instance storage>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Storage is
-- managed by the DB cluster.
--
-- Constraints:
--
-- -   For RDS for MariaDB, MySQL, Oracle, and PostgreSQL - Must be a
--     multiple between .5 and 50 of the storage amount for the DB
--     instance.
--
-- -   For RDS for SQL Server - Must be a multiple between 1 and 50 of the
--     storage amount for the DB instance.
--
-- 'kmsKeyId', 'createDBInstance_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The Amazon
-- Web Services KMS key identifier is managed by the DB cluster. For more
-- information, see @CreateDBCluster@.
--
-- If @StorageEncrypted@ is enabled, and you do not specify a value for the
-- @KmsKeyId@ parameter, then Amazon RDS uses your default KMS key. There
-- is a default KMS key for your Amazon Web Services account. Your Amazon
-- Web Services account has a different default KMS key for each Amazon Web
-- Services Region.
--
-- For Amazon RDS Custom, a KMS key is required for DB instances. For most
-- RDS engines, if you leave this parameter empty while enabling
-- @StorageEncrypted@, the engine uses the default KMS key. However, RDS
-- Custom doesn\'t use the default key when this parameter is empty. You
-- must explicitly specify a key.
--
-- 'licenseModel', 'createDBInstance_licenseModel' - The license model information for this DB instance.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
--
-- Valid Values:
--
-- -   RDS for MariaDB - @general-public-license@
--
-- -   RDS for Microsoft SQL Server - @license-included@
--
-- -   RDS for MySQL - @general-public-license@
--
-- -   RDS for Oracle - @bring-your-own-license | license-included@
--
-- -   RDS for PostgreSQL - @postgresql-license@
--
-- 'manageMasterUserPassword', 'createDBInstance_manageMasterUserPassword' - Specifies whether to manage the master user password with Amazon Web
-- Services Secrets Manager.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Can\'t manage the master user password with Amazon Web Services
--     Secrets Manager if @MasterUserPassword@ is specified.
--
-- 'masterUserPassword', 'createDBInstance_masterUserPassword' - The password for the master user.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The password
-- for the master user is managed by the DB cluster.
--
-- Constraints:
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
--
-- -   Can include any printable ASCII character except \"\/\", \"\"\", or
--     \"\@\".
--
-- Length Constraints:
--
-- -   RDS for MariaDB - Must contain from 8 to 41 characters.
--
-- -   RDS for Microsoft SQL Server - Must contain from 8 to 128
--     characters.
--
-- -   RDS for MySQL - Must contain from 8 to 41 characters.
--
-- -   RDS for Oracle - Must contain from 8 to 30 characters.
--
-- -   RDS for PostgreSQL - Must contain from 8 to 128 characters.
--
-- 'masterUserSecretKmsKeyId', 'createDBInstance_masterUserSecretKmsKeyId' - The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
-- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
-- secret is in a different Amazon Web Services account, then you can\'t
-- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
-- must use a customer managed KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- 'masterUsername', 'createDBInstance_masterUsername' - The name for the master user.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The name for
-- the master user is managed by the DB cluster.
--
-- This setting is required for RDS DB instances.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters, numbers, or underscores.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'maxAllocatedStorage', 'createDBInstance_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (Storage is managed by the DB cluster.)
--
-- -   RDS Custom
--
-- 'monitoringInterval', 'createDBInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collection of
-- Enhanced Monitoring metrics, specify @0@.
--
-- If @MonitoringRoleArn@ is specified, then you must set
-- @MonitoringInterval@ to a value other than @0@.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Valid Values: @0 | 1 | 5 | 10 | 15 | 30 | 60@
--
-- Default: @0@
--
-- 'monitoringRoleArn', 'createDBInstance_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than @0@, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- 'multiAZ', 'createDBInstance_multiAZ' - Specifies whether the DB instance is a Multi-AZ deployment. You can\'t
-- set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ
-- deployment.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (DB instance Availability Zones (AZs) are managed by
--     the DB cluster.)
--
-- -   RDS Custom
--
-- 'ncharCharacterSetName', 'createDBInstance_ncharCharacterSetName' - The name of the NCHAR character set for the Oracle DB instance.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- 'networkType', 'createDBInstance_networkType' - The network type of the DB instance.
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
--
-- Valid Values: @IPV4 | DUAL@
--
-- 'optionGroupName', 'createDBInstance_optionGroupName' - The option group to associate the DB instance with.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group. Also, that option group
-- can\'t be removed from a DB instance after it is associated with a DB
-- instance.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
--
-- 'performanceInsightsKMSKeyId', 'createDBInstance_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you don\'t specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- 'performanceInsightsRetentionPeriod', 'createDBInstance_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Valid Values:
--
-- -   @7@
--
-- -   /month/ * 31, where /month/ is a number of months from 1-23.
--     Examples: @93@ (3 months * 31), @341@ (11 months * 31), @589@ (19
--     months * 31)
--
-- -   @731@
--
-- Default: @7@ days
--
-- If you specify a retention period that isn\'t valid, such as @94@,
-- Amazon RDS returns an error.
--
-- 'port', 'createDBInstance_port' - The port number on which the database accepts connections.
--
-- This setting doesn\'t apply to Aurora DB instances. The port number is
-- managed by the cluster.
--
-- Valid Values: @1150-65535@
--
-- Default:
--
-- -   RDS for MariaDB - @3306@
--
-- -   RDS for Microsoft SQL Server - @1433@
--
-- -   RDS for MySQL - @3306@
--
-- -   RDS for Oracle - @1521@
--
-- -   RDS for PostgreSQL - @5432@
--
-- Constraints:
--
-- -   For RDS for Microsoft SQL Server, the value can\'t be @1234@,
--     @1434@, @3260@, @3343@, @3389@, @47001@, or @49152-49156@.
--
-- 'preferredBackupWindow', 'createDBInstance_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter. The default is a 30-minute window selected at random from an
-- 8-hour block of time for each Amazon Web Services Region. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The daily
-- time range for creating automated backups is managed by the DB cluster.
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
-- 'preferredMaintenanceWindow', 'createDBInstance_preferredMaintenanceWindow' - The time range each week during which system maintenance can occur. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
--
-- Constraints:
--
-- -   Must be in the format @ddd:hh24:mi-ddd:hh24:mi@.
--
-- -   The day values must be @mon | tue | wed | thu | fri | sat | sun@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred backup window.
--
-- -   Must be at least 30 minutes.
--
-- 'processorFeatures', 'createDBInstance_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
--
-- 'promotionTier', 'createDBInstance_promotionTier' - The order of priority in which an Aurora Replica is promoted to the
-- primary instance after a failure of the existing primary instance. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.AuroraHighAvailability.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Default: @1@
--
-- Valid Values: @0 - 15@
--
-- 'publiclyAccessible', 'createDBInstance_publiclyAccessible' - Specifies whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its Domain Name System
-- (DNS) endpoint resolves to the private IP address from within the DB
-- instance\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB instance\'s VPC. Access to the DB
-- instance is ultimately controlled by the security group it uses. That
-- public access is not permitted if the security group assigned to the DB
-- instance doesn\'t permit it.
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
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB instance is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB instance is public.
--
-- 'storageEncrypted', 'createDBInstance_storageEncrypted' - Specifes whether the DB instance is encrypted. By default, it isn\'t
-- encrypted.
--
-- For RDS Custom DB instances, either enable this setting or leave it
-- unset. Otherwise, Amazon RDS reports an error.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The
-- encryption for DB instances is managed by the DB cluster.
--
-- 'storageThroughput', 'createDBInstance_storageThroughput' - The storage throughput value for the DB instance.
--
-- This setting applies only to the @gp3@ storage type.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
--
-- 'storageType', 'createDBInstance_storageType' - The storage type to associate with the DB instance.
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Storage is
-- managed by the DB cluster.
--
-- Valid Values: @gp2 | gp3 | io1 | standard@
--
-- Default: @io1@, if the @Iops@ parameter is specified. Otherwise, @gp2@.
--
-- 'tags', 'createDBInstance_tags' - Tags to assign to the DB instance.
--
-- 'tdeCredentialArn', 'createDBInstance_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
--
-- 'tdeCredentialPassword', 'createDBInstance_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- 'timezone', 'createDBInstance_timezone' - The time zone of the DB instance. The time zone parameter is currently
-- supported only by
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server>.
--
-- 'vpcSecurityGroupIds', 'createDBInstance_vpcSecurityGroupIds' - A list of Amazon EC2 VPC security groups to associate with this DB
-- instance.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The
-- associated list of EC2 VPC security groups is managed by the DB cluster.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
--
-- 'dbInstanceIdentifier', 'createDBInstance_dbInstanceIdentifier' - The identifier for this DB instance. This parameter is stored as a
-- lowercase string.
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
-- 'dbInstanceClass', 'createDBInstance_dbInstanceClass' - The compute and memory capacity of the DB instance, for example
-- @db.m5.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance classes>
-- in the /Amazon RDS User Guide/ or
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.DBInstanceClass.html Aurora DB instance classes>
-- in the /Amazon Aurora User Guide/.
--
-- 'engine', 'createDBInstance_engine' - The database engine to use for this DB instance.
--
-- Not every database engine is available in every Amazon Web Services
-- Region.
--
-- Valid Values:
--
-- -   @aurora-mysql@ (for Aurora MySQL DB instances)
--
-- -   @aurora-postgresql@ (for Aurora PostgreSQL DB instances)
--
-- -   @custom-oracle-ee@ (for RDS Custom for Oracle DB instances)
--
-- -   @custom-oracle-ee-cdb@ (for RDS Custom for Oracle DB instances)
--
-- -   @custom-sqlserver-ee@ (for RDS Custom for SQL Server DB instances)
--
-- -   @custom-sqlserver-se@ (for RDS Custom for SQL Server DB instances)
--
-- -   @custom-sqlserver-web@ (for RDS Custom for SQL Server DB instances)
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
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
      { allocatedStorage =
          Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        backupRetentionPeriod = Prelude.Nothing,
        backupTarget = Prelude.Nothing,
        cACertificateIdentifier = Prelude.Nothing,
        characterSetName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        customIamInstanceProfile = Prelude.Nothing,
        dbClusterIdentifier = Prelude.Nothing,
        dbName = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        dbSecurityGroups = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        domain = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        enableCustomerOwnedIp = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        iops = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        manageMasterUserPassword = Prelude.Nothing,
        masterUserPassword = Prelude.Nothing,
        masterUserSecretKmsKeyId = Prelude.Nothing,
        masterUsername = Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        ncharCharacterSetName = Prelude.Nothing,
        networkType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        performanceInsightsRetentionPeriod = Prelude.Nothing,
        port = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        promotionTier = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        storageThroughput = Prelude.Nothing,
        storageType = Prelude.Nothing,
        tags = Prelude.Nothing,
        tdeCredentialArn = Prelude.Nothing,
        tdeCredentialPassword = Prelude.Nothing,
        timezone = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbInstanceClass = pDBInstanceClass_,
        engine = pEngine_
      }

-- | The amount of storage in gibibytes (GiB) to allocate for the DB
-- instance.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Aurora
-- cluster volumes automatically grow as the amount of data in your
-- database increases, though you are only charged for the space that you
-- use in an Aurora cluster volume.
--
-- [Amazon RDS Custom
-- RDS for MariaDB
-- RDS for MySQL
-- RDS for Oracle
-- RDS for PostgreSQL
-- RDS for SQL Server]
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 40 to 65536 for RDS Custom for Oracle, 16384 for RDS Custom
--         for SQL Server.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 40 to
--         65536 for RDS Custom for Oracle, 16384 for RDS Custom for SQL
--         Server.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 10 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3): Must be an integer
--         from 20 to 65536.
--
--     -   Provisioned IOPS storage (io1): Must be an integer from 100 to
--         65536.
--
--     -   Magnetic storage (standard): Must be an integer from 5 to 3072.
--
--     Constraints to the amount of storage for each storage type are the
--     following:
--
--     -   General Purpose (SSD) storage (gp2, gp3):
--
--         -   Enterprise and Standard editions: Must be an integer from 20
--             to 16384.
--
--         -   Web and Express editions: Must be an integer from 20 to
--             16384.
--
--     -   Provisioned IOPS storage (io1):
--
--         -   Enterprise and Standard editions: Must be an integer from
--             100 to 16384.
--
--         -   Web and Express editions: Must be an integer from 100 to
--             16384.
--
--     -   Magnetic storage (standard):
--
--         -   Enterprise and Standard editions: Must be an integer from 20
--             to 1024.
--
--         -   Web and Express editions: Must be an integer from 20 to
--             1024.
createDBInstance_allocatedStorage :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_allocatedStorage = Lens.lens (\CreateDBInstance' {allocatedStorage} -> allocatedStorage) (\s@CreateDBInstance' {} a -> s {allocatedStorage = a} :: CreateDBInstance)

-- | Specifies whether minor engine upgrades are applied automatically to the
-- DB instance during the maintenance window. By default, minor engine
-- upgrades are applied automatically.
--
-- If you create an RDS Custom DB instance, you must set
-- @AutoMinorVersionUpgrade@ to @false@.
createDBInstance_autoMinorVersionUpgrade :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_autoMinorVersionUpgrade = Lens.lens (\CreateDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstance)

-- | The Availability Zone (AZ) where the database will be created. For
-- information on Amazon Web Services Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- For Amazon Aurora, each Aurora DB cluster hosts copies of its storage in
-- three separate Availability Zones. Specify one of these Availability
-- Zones. Aurora automatically chooses an appropriate Availability Zone if
-- you don\'t specify one.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Constraints:
--
-- -   The @AvailabilityZone@ parameter can\'t be specified if the DB
--     instance is a Multi-AZ deployment.
--
-- -   The specified Availability Zone must be in the same Amazon Web
--     Services Region as the current endpoint.
--
-- Example: @us-east-1d@
createDBInstance_availabilityZone :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_availabilityZone = Lens.lens (\CreateDBInstance' {availabilityZone} -> availabilityZone) (\s@CreateDBInstance' {} a -> s {availabilityZone = a} :: CreateDBInstance)

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to @0@ disables automated backups.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The retention
-- period for automated backups is managed by the DB cluster.
--
-- Default: @1@
--
-- Constraints:
--
-- -   Must be a value from 0 to 35.
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas.
--
-- -   Can\'t be set to 0 for an RDS Custom for Oracle DB instance.
createDBInstance_backupRetentionPeriod :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_backupRetentionPeriod = Lens.lens (\CreateDBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@CreateDBInstance' {} a -> s {backupRetentionPeriod = a} :: CreateDBInstance)

-- | The location for storing automated backups and manual snapshots.
--
-- Valie Values:
--
-- -   @outposts@ (Amazon Web Services Outposts)
--
-- -   @region@ (Amazon Web Services Region)
--
-- Default: @region@
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
createDBInstance_backupTarget :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_backupTarget = Lens.lens (\CreateDBInstance' {backupTarget} -> backupTarget) (\s@CreateDBInstance' {} a -> s {backupTarget = a} :: CreateDBInstance)

-- | The CA certificate identifier to use for the DB instance\'s server
-- certificate.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
createDBInstance_cACertificateIdentifier :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_cACertificateIdentifier = Lens.lens (\CreateDBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@CreateDBInstance' {} a -> s {cACertificateIdentifier = a} :: CreateDBInstance)

-- | For supported engines, the character set (@CharacterSet@) to associate
-- the DB instance with.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora - The character set is managed by the DB cluster. For
--     more information, see @CreateDBCluster@.
--
-- -   RDS Custom - However, if you need to change the character set, you
--     can change it on the database itself.
createDBInstance_characterSetName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_characterSetName = Lens.lens (\CreateDBInstance' {characterSetName} -> characterSetName) (\s@CreateDBInstance' {} a -> s {characterSetName = a} :: CreateDBInstance)

-- | Spcifies whether to copy tags from the DB instance to snapshots of the
-- DB instance. By default, tags are not copied.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Copying tags
-- to snapshots is managed by the DB cluster. Setting this value for an
-- Aurora DB instance has no effect on the DB cluster setting.
createDBInstance_copyTagsToSnapshot :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_copyTagsToSnapshot = Lens.lens (\CreateDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBInstance' {} a -> s {copyTagsToSnapshot = a} :: CreateDBInstance)

-- | The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance.
--
-- This setting is required for RDS Custom.
--
-- Constraints:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
createDBInstance_customIamInstanceProfile :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_customIamInstanceProfile = Lens.lens (\CreateDBInstance' {customIamInstanceProfile} -> customIamInstanceProfile) (\s@CreateDBInstance' {} a -> s {customIamInstanceProfile = a} :: CreateDBInstance)

-- | The identifier of the DB cluster that this DB instance will belong to.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
createDBInstance_dbClusterIdentifier :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbClusterIdentifier = Lens.lens (\CreateDBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBInstance' {} a -> s {dbClusterIdentifier = a} :: CreateDBInstance)

-- | The meaning of this parameter differs depending on the database engine.
--
-- [Amazon Aurora MySQL
-- Amazon Aurora PostgreSQL
-- Amazon RDS Custom for Oracle
-- Amazon RDS Custom for SQL Server
-- RDS for MariaDB
-- RDS for MySQL
-- RDS for Oracle
-- RDS for PostgreSQL
-- RDS for SQL Server]
--     The name of the database to create when the primary DB instance of
--     the Aurora MySQL DB cluster is created. If you don\'t specify a
--     value, Amazon RDS doesn\'t create a database in the DB cluster.
--
--     Constraints:
--
--     -   Must contain 1 to 64 alphanumeric characters.
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The name of the database to create when the primary DB instance of
--     the Aurora PostgreSQL DB cluster is created.
--
--     Default: @postgres@
--
--     Constraints:
--
--     -   Must contain 1 to 63 alphanumeric characters.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0 to 9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The Oracle System ID (SID) of the created RDS Custom DB instance.
--
--     Default: @ORCL@
--
--     Constraints:
--
--     -   Must contain 1 to 8 alphanumeric characters.
--
--     -   Must contain a letter.
--
--     -   Can\'t be a word reserved by the database engine.
--
--     Not applicable. Must be null.
--
--     The name of the database to create when the DB instance is created.
--     If you don\'t specify a value, Amazon RDS doesn\'t create a database
--     in the DB instance.
--
--     Constraints:
--
--     -   Must contain 1 to 64 letters or numbers.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0-9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The name of the database to create when the DB instance is created.
--     If you don\'t specify a value, Amazon RDS doesn\'t create a database
--     in the DB instance.
--
--     Constraints:
--
--     -   Must contain 1 to 64 letters or numbers.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0-9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     The Oracle System ID (SID) of the created DB instance.
--
--     Default: @ORCL@
--
--     Constraints:
--
--     -   Can\'t be longer than 8 characters.
--
--     -   Can\'t be a word reserved by the database engine, such as the
--         string @NULL@.
--
--     The name of the database to create when the DB instance is created.
--
--     Default: @postgres@
--
--     Constraints:
--
--     -   Must contain 1 to 63 letters, numbers, or underscores.
--
--     -   Must begin with a letter. Subsequent characters can be letters,
--         underscores, or digits (0-9).
--
--     -   Can\'t be a word reserved by the database engine.
--
--     Not applicable. Must be null.
createDBInstance_dbName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbName = Lens.lens (\CreateDBInstance' {dbName} -> dbName) (\s@CreateDBInstance' {} a -> s {dbName = a} :: CreateDBInstance)

-- | The name of the DB parameter group to associate with this DB instance.
-- If you don\'t specify a value, then Amazon RDS uses the default DB
-- parameter group for the specified DB engine and version.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
createDBInstance_dbParameterGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbParameterGroupName = Lens.lens (\CreateDBInstance' {dbParameterGroupName} -> dbParameterGroupName) (\s@CreateDBInstance' {} a -> s {dbParameterGroupName = a} :: CreateDBInstance)

-- | A list of DB security groups to associate with this DB instance.
--
-- This setting applies to the legacy EC2-Classic platform, which is no
-- longer used to create new DB instances. Use the @VpcSecurityGroupIds@
-- setting instead.
createDBInstance_dbSecurityGroups :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_dbSecurityGroups = Lens.lens (\CreateDBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@CreateDBInstance' {} a -> s {dbSecurityGroups = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A DB subnet group to associate with this DB instance.
--
-- Constraints:
--
-- -   Must match the name of an existing DB subnet group.
--
-- -   Must not be @default@.
--
-- Example: @mydbsubnetgroup@
createDBInstance_dbSubnetGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbSubnetGroupName = Lens.lens (\CreateDBInstance' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBInstance' {} a -> s {dbSubnetGroupName = a} :: CreateDBInstance)

-- | Specifies whether the DB instance has deletion protection enabled. The
-- database can\'t be deleted when deletion protection is enabled. By
-- default, deletion protection isn\'t enabled. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. You can
-- enable or disable deletion protection for the DB cluster. For more
-- information, see @CreateDBCluster@. DB instances in a DB cluster can be
-- deleted even when deletion protection is enabled for the DB cluster.
createDBInstance_deletionProtection :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_deletionProtection = Lens.lens (\CreateDBInstance' {deletionProtection} -> deletionProtection) (\s@CreateDBInstance' {} a -> s {deletionProtection = a} :: CreateDBInstance)

-- | The Active Directory directory ID to create the DB instance in.
-- Currently, only Microsoft SQL Server, MySQL, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (The domain is managed by the DB cluster.)
--
-- -   RDS Custom
createDBInstance_domain :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_domain = Lens.lens (\CreateDBInstance' {domain} -> domain) (\s@CreateDBInstance' {} a -> s {domain = a} :: CreateDBInstance)

-- | The name of the IAM role to use when making API calls to the Directory
-- Service.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (The domain is managed by the DB cluster.)
--
-- -   RDS Custom
createDBInstance_domainIAMRoleName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_domainIAMRoleName = Lens.lens (\CreateDBInstance' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBInstance' {} a -> s {domainIAMRoleName = a} :: CreateDBInstance)

-- | The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (CloudWatch Logs exports are managed by the DB
--     cluster.)
--
-- -   RDS Custom
--
-- The following values are valid for each DB engine:
--
-- -   RDS for MariaDB - @audit | error | general | slowquery@
--
-- -   RDS for Microsoft SQL Server - @agent | error@
--
-- -   RDS for MySQL - @audit | error | general | slowquery@
--
-- -   RDS for Oracle - @alert | audit | listener | trace | oemagent@
--
-- -   RDS for PostgreSQL - @postgresql | upgrade@
createDBInstance_enableCloudwatchLogsExports :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_enableCloudwatchLogsExports = Lens.lens (\CreateDBInstance' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBInstance' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to enable a customer-owned IP address (CoIP) for an
-- RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/routing.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
createDBInstance_enableCustomerOwnedIp :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enableCustomerOwnedIp = Lens.lens (\CreateDBInstance' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@CreateDBInstance' {} a -> s {enableCustomerOwnedIp = a} :: CreateDBInstance)

-- | Specifies whether to enable mapping of Amazon Web Services Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (Mapping Amazon Web Services IAM accounts to database
--     accounts is managed by the DB cluster.)
--
-- -   RDS Custom
createDBInstance_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBInstance' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBInstance' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstance)

-- | Specifies whether to enable Performance Insights for the DB instance.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
createDBInstance_enablePerformanceInsights :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enablePerformanceInsights = Lens.lens (\CreateDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBInstance' {} a -> s {enablePerformanceInsights = a} :: CreateDBInstance)

-- | The version number of the database engine to use.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The version
-- number of the database engine the DB instance uses is managed by the DB
-- cluster.
--
-- For a list of valid engine versions, use the @DescribeDBEngineVersions@
-- operation.
--
-- The following are the database engines and links to information about
-- the major and minor versions that are available with Amazon RDS. Not
-- every database engine is available for every Amazon Web Services Region.
--
-- [Amazon RDS Custom for Oracle
-- Amazon RDS Custom for SQL Server
-- RDS for MariaDB
-- RDS for Microsoft SQL Server
-- RDS for MySQL
-- RDS for Oracle
-- RDS for PostgreSQL]
--     A custom engine version (CEV) that you have previously created. This
--     setting is required for RDS Custom for Oracle. The CEV name has the
--     following format: 19./customized_string/. A valid CEV name is
--     @19.my_cev1@. For more information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-creating.html#custom-creating.create Creating an RDS Custom for Oracle DB instance>
--     in the /Amazon RDS User Guide/.
--
--     See
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-reqs-limits-MS.html RDS Custom for SQL Server general requirements>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MariaDB.html#MariaDB.Concepts.VersionMgmt MariaDB on Amazon RDS versions>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server versions on Amazon RDS>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS versions>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Appendix.Oracle.PatchComposition.html Oracle Database Engine release notes>
--     in the /Amazon RDS User Guide/.
--
--     For information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
--     in the /Amazon RDS User Guide/.
createDBInstance_engineVersion :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_engineVersion = Lens.lens (\CreateDBInstance' {engineVersion} -> engineVersion) (\s@CreateDBInstance' {} a -> s {engineVersion = a} :: CreateDBInstance)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- initially allocate for the DB instance. For information about valid IOPS
-- values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html Amazon RDS DB instance storage>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Storage is
-- managed by the DB cluster.
--
-- Constraints:
--
-- -   For RDS for MariaDB, MySQL, Oracle, and PostgreSQL - Must be a
--     multiple between .5 and 50 of the storage amount for the DB
--     instance.
--
-- -   For RDS for SQL Server - Must be a multiple between 1 and 50 of the
--     storage amount for the DB instance.
createDBInstance_iops :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_iops = Lens.lens (\CreateDBInstance' {iops} -> iops) (\s@CreateDBInstance' {} a -> s {iops = a} :: CreateDBInstance)

-- | The Amazon Web Services KMS key identifier for an encrypted DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The Amazon
-- Web Services KMS key identifier is managed by the DB cluster. For more
-- information, see @CreateDBCluster@.
--
-- If @StorageEncrypted@ is enabled, and you do not specify a value for the
-- @KmsKeyId@ parameter, then Amazon RDS uses your default KMS key. There
-- is a default KMS key for your Amazon Web Services account. Your Amazon
-- Web Services account has a different default KMS key for each Amazon Web
-- Services Region.
--
-- For Amazon RDS Custom, a KMS key is required for DB instances. For most
-- RDS engines, if you leave this parameter empty while enabling
-- @StorageEncrypted@, the engine uses the default KMS key. However, RDS
-- Custom doesn\'t use the default key when this parameter is empty. You
-- must explicitly specify a key.
createDBInstance_kmsKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_kmsKeyId = Lens.lens (\CreateDBInstance' {kmsKeyId} -> kmsKeyId) (\s@CreateDBInstance' {} a -> s {kmsKeyId = a} :: CreateDBInstance)

-- | The license model information for this DB instance.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
--
-- Valid Values:
--
-- -   RDS for MariaDB - @general-public-license@
--
-- -   RDS for Microsoft SQL Server - @license-included@
--
-- -   RDS for MySQL - @general-public-license@
--
-- -   RDS for Oracle - @bring-your-own-license | license-included@
--
-- -   RDS for PostgreSQL - @postgresql-license@
createDBInstance_licenseModel :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_licenseModel = Lens.lens (\CreateDBInstance' {licenseModel} -> licenseModel) (\s@CreateDBInstance' {} a -> s {licenseModel = a} :: CreateDBInstance)

-- | Specifies whether to manage the master user password with Amazon Web
-- Services Secrets Manager.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Can\'t manage the master user password with Amazon Web Services
--     Secrets Manager if @MasterUserPassword@ is specified.
createDBInstance_manageMasterUserPassword :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_manageMasterUserPassword = Lens.lens (\CreateDBInstance' {manageMasterUserPassword} -> manageMasterUserPassword) (\s@CreateDBInstance' {} a -> s {manageMasterUserPassword = a} :: CreateDBInstance)

-- | The password for the master user.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The password
-- for the master user is managed by the DB cluster.
--
-- Constraints:
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
--
-- -   Can include any printable ASCII character except \"\/\", \"\"\", or
--     \"\@\".
--
-- Length Constraints:
--
-- -   RDS for MariaDB - Must contain from 8 to 41 characters.
--
-- -   RDS for Microsoft SQL Server - Must contain from 8 to 128
--     characters.
--
-- -   RDS for MySQL - Must contain from 8 to 41 characters.
--
-- -   RDS for Oracle - Must contain from 8 to 30 characters.
--
-- -   RDS for PostgreSQL - Must contain from 8 to 128 characters.
createDBInstance_masterUserPassword :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_masterUserPassword = Lens.lens (\CreateDBInstance' {masterUserPassword} -> masterUserPassword) (\s@CreateDBInstance' {} a -> s {masterUserPassword = a} :: CreateDBInstance)

-- | The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB instance.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
-- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
-- secret is in a different Amazon Web Services account, then you can\'t
-- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
-- must use a customer managed KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
createDBInstance_masterUserSecretKmsKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_masterUserSecretKmsKeyId = Lens.lens (\CreateDBInstance' {masterUserSecretKmsKeyId} -> masterUserSecretKmsKeyId) (\s@CreateDBInstance' {} a -> s {masterUserSecretKmsKeyId = a} :: CreateDBInstance)

-- | The name for the master user.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The name for
-- the master user is managed by the DB cluster.
--
-- This setting is required for RDS DB instances.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters, numbers, or underscores.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
createDBInstance_masterUsername :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_masterUsername = Lens.lens (\CreateDBInstance' {masterUsername} -> masterUsername) (\s@CreateDBInstance' {} a -> s {masterUsername = a} :: CreateDBInstance)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (Storage is managed by the DB cluster.)
--
-- -   RDS Custom
createDBInstance_maxAllocatedStorage :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_maxAllocatedStorage = Lens.lens (\CreateDBInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@CreateDBInstance' {} a -> s {maxAllocatedStorage = a} :: CreateDBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collection of
-- Enhanced Monitoring metrics, specify @0@.
--
-- If @MonitoringRoleArn@ is specified, then you must set
-- @MonitoringInterval@ to a value other than @0@.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Valid Values: @0 | 1 | 5 | 10 | 15 | 30 | 60@
--
-- Default: @0@
createDBInstance_monitoringInterval :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_monitoringInterval = Lens.lens (\CreateDBInstance' {monitoringInterval} -> monitoringInterval) (\s@CreateDBInstance' {} a -> s {monitoringInterval = a} :: CreateDBInstance)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than @0@, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
createDBInstance_monitoringRoleArn :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_monitoringRoleArn = Lens.lens (\CreateDBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@CreateDBInstance' {} a -> s {monitoringRoleArn = a} :: CreateDBInstance)

-- | Specifies whether the DB instance is a Multi-AZ deployment. You can\'t
-- set the @AvailabilityZone@ parameter if the DB instance is a Multi-AZ
-- deployment.
--
-- This setting doesn\'t apply to the following DB instances:
--
-- -   Amazon Aurora (DB instance Availability Zones (AZs) are managed by
--     the DB cluster.)
--
-- -   RDS Custom
createDBInstance_multiAZ :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_multiAZ = Lens.lens (\CreateDBInstance' {multiAZ} -> multiAZ) (\s@CreateDBInstance' {} a -> s {multiAZ = a} :: CreateDBInstance)

-- | The name of the NCHAR character set for the Oracle DB instance.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
createDBInstance_ncharCharacterSetName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_ncharCharacterSetName = Lens.lens (\CreateDBInstance' {ncharCharacterSetName} -> ncharCharacterSetName) (\s@CreateDBInstance' {} a -> s {ncharCharacterSetName = a} :: CreateDBInstance)

-- | The network type of the DB instance.
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB instance. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
--
-- Valid Values: @IPV4 | DUAL@
createDBInstance_networkType :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_networkType = Lens.lens (\CreateDBInstance' {networkType} -> networkType) (\s@CreateDBInstance' {} a -> s {networkType = a} :: CreateDBInstance)

-- | The option group to associate the DB instance with.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group. Also, that option group
-- can\'t be removed from a DB instance after it is associated with a DB
-- instance.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
createDBInstance_optionGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_optionGroupName = Lens.lens (\CreateDBInstance' {optionGroupName} -> optionGroupName) (\s@CreateDBInstance' {} a -> s {optionGroupName = a} :: CreateDBInstance)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you don\'t specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
createDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_performanceInsightsKMSKeyId = Lens.lens (\CreateDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstance)

-- | The number of days to retain Performance Insights data.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Valid Values:
--
-- -   @7@
--
-- -   /month/ * 31, where /month/ is a number of months from 1-23.
--     Examples: @93@ (3 months * 31), @341@ (11 months * 31), @589@ (19
--     months * 31)
--
-- -   @731@
--
-- Default: @7@ days
--
-- If you specify a retention period that isn\'t valid, such as @94@,
-- Amazon RDS returns an error.
createDBInstance_performanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_performanceInsightsRetentionPeriod = Lens.lens (\CreateDBInstance' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@CreateDBInstance' {} a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBInstance)

-- | The port number on which the database accepts connections.
--
-- This setting doesn\'t apply to Aurora DB instances. The port number is
-- managed by the cluster.
--
-- Valid Values: @1150-65535@
--
-- Default:
--
-- -   RDS for MariaDB - @3306@
--
-- -   RDS for Microsoft SQL Server - @1433@
--
-- -   RDS for MySQL - @3306@
--
-- -   RDS for Oracle - @1521@
--
-- -   RDS for PostgreSQL - @5432@
--
-- Constraints:
--
-- -   For RDS for Microsoft SQL Server, the value can\'t be @1234@,
--     @1434@, @3260@, @3343@, @3389@, @47001@, or @49152-49156@.
createDBInstance_port :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_port = Lens.lens (\CreateDBInstance' {port} -> port) (\s@CreateDBInstance' {} a -> s {port = a} :: CreateDBInstance)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter. The default is a 30-minute window selected at random from an
-- 8-hour block of time for each Amazon Web Services Region. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The daily
-- time range for creating automated backups is managed by the DB cluster.
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

-- | The time range each week during which system maintenance can occur. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
--
-- Constraints:
--
-- -   Must be in the format @ddd:hh24:mi-ddd:hh24:mi@.
--
-- -   The day values must be @mon | tue | wed | thu | fri | sat | sun@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred backup window.
--
-- -   Must be at least 30 minutes.
createDBInstance_preferredMaintenanceWindow :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_preferredMaintenanceWindow = Lens.lens (\CreateDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBInstance)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
createDBInstance_processorFeatures :: Lens.Lens' CreateDBInstance (Prelude.Maybe [ProcessorFeature])
createDBInstance_processorFeatures = Lens.lens (\CreateDBInstance' {processorFeatures} -> processorFeatures) (\s@CreateDBInstance' {} a -> s {processorFeatures = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The order of priority in which an Aurora Replica is promoted to the
-- primary instance after a failure of the existing primary instance. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.AuroraHighAvailability.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
--
-- Default: @1@
--
-- Valid Values: @0 - 15@
createDBInstance_promotionTier :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_promotionTier = Lens.lens (\CreateDBInstance' {promotionTier} -> promotionTier) (\s@CreateDBInstance' {} a -> s {promotionTier = a} :: CreateDBInstance)

-- | Specifies whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its Domain Name System
-- (DNS) endpoint resolves to the private IP address from within the DB
-- instance\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB instance\'s VPC. Access to the DB
-- instance is ultimately controlled by the security group it uses. That
-- public access is not permitted if the security group assigned to the DB
-- instance doesn\'t permit it.
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
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB instance is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB instance is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB instance is public.
createDBInstance_publiclyAccessible :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_publiclyAccessible = Lens.lens (\CreateDBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@CreateDBInstance' {} a -> s {publiclyAccessible = a} :: CreateDBInstance)

-- | Specifes whether the DB instance is encrypted. By default, it isn\'t
-- encrypted.
--
-- For RDS Custom DB instances, either enable this setting or leave it
-- unset. Otherwise, Amazon RDS reports an error.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The
-- encryption for DB instances is managed by the DB cluster.
createDBInstance_storageEncrypted :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_storageEncrypted = Lens.lens (\CreateDBInstance' {storageEncrypted} -> storageEncrypted) (\s@CreateDBInstance' {} a -> s {storageEncrypted = a} :: CreateDBInstance)

-- | The storage throughput value for the DB instance.
--
-- This setting applies only to the @gp3@ storage type.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
createDBInstance_storageThroughput :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_storageThroughput = Lens.lens (\CreateDBInstance' {storageThroughput} -> storageThroughput) (\s@CreateDBInstance' {} a -> s {storageThroughput = a} :: CreateDBInstance)

-- | The storage type to associate with the DB instance.
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. Storage is
-- managed by the DB cluster.
--
-- Valid Values: @gp2 | gp3 | io1 | standard@
--
-- Default: @io1@, if the @Iops@ parameter is specified. Otherwise, @gp2@.
createDBInstance_storageType :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_storageType = Lens.lens (\CreateDBInstance' {storageType} -> storageType) (\s@CreateDBInstance' {} a -> s {storageType = a} :: CreateDBInstance)

-- | Tags to assign to the DB instance.
createDBInstance_tags :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Tag])
createDBInstance_tags = Lens.lens (\CreateDBInstance' {tags} -> tags) (\s@CreateDBInstance' {} a -> s {tags = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- This setting doesn\'t apply to Amazon Aurora or RDS Custom DB instances.
createDBInstance_tdeCredentialArn :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_tdeCredentialArn = Lens.lens (\CreateDBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@CreateDBInstance' {} a -> s {tdeCredentialArn = a} :: CreateDBInstance)

-- | The password for the given ARN from the key store in order to access the
-- device.
--
-- This setting doesn\'t apply to RDS Custom DB instances.
createDBInstance_tdeCredentialPassword :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_tdeCredentialPassword = Lens.lens (\CreateDBInstance' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@CreateDBInstance' {} a -> s {tdeCredentialPassword = a} :: CreateDBInstance)

-- | The time zone of the DB instance. The time zone parameter is currently
-- supported only by
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server>.
createDBInstance_timezone :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_timezone = Lens.lens (\CreateDBInstance' {timezone} -> timezone) (\s@CreateDBInstance' {} a -> s {timezone = a} :: CreateDBInstance)

-- | A list of Amazon EC2 VPC security groups to associate with this DB
-- instance.
--
-- This setting doesn\'t apply to Amazon Aurora DB instances. The
-- associated list of EC2 VPC security groups is managed by the DB cluster.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
createDBInstance_vpcSecurityGroupIds :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_vpcSecurityGroupIds = Lens.lens (\CreateDBInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBInstance' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for this DB instance. This parameter is stored as a
-- lowercase string.
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

-- | The compute and memory capacity of the DB instance, for example
-- @db.m5.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance classes>
-- in the /Amazon RDS User Guide/ or
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.DBInstanceClass.html Aurora DB instance classes>
-- in the /Amazon Aurora User Guide/.
createDBInstance_dbInstanceClass :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbInstanceClass = Lens.lens (\CreateDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@CreateDBInstance' {} a -> s {dbInstanceClass = a} :: CreateDBInstance)

-- | The database engine to use for this DB instance.
--
-- Not every database engine is available in every Amazon Web Services
-- Region.
--
-- Valid Values:
--
-- -   @aurora-mysql@ (for Aurora MySQL DB instances)
--
-- -   @aurora-postgresql@ (for Aurora PostgreSQL DB instances)
--
-- -   @custom-oracle-ee@ (for RDS Custom for Oracle DB instances)
--
-- -   @custom-oracle-ee-cdb@ (for RDS Custom for Oracle DB instances)
--
-- -   @custom-sqlserver-ee@ (for RDS Custom for SQL Server DB instances)
--
-- -   @custom-sqlserver-se@ (for RDS Custom for SQL Server DB instances)
--
-- -   @custom-sqlserver-web@ (for RDS Custom for SQL Server DB instances)
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceResult"
      ( \s h x ->
          CreateDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBInstance where
  hashWithSalt _salt CreateDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` backupTarget
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` customIamInstanceProfile
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` dbSecurityGroups
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableCustomerOwnedIp
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` manageMasterUserPassword
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUserSecretKmsKeyId
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` maxAllocatedStorage
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` ncharCharacterSetName
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` storageThroughput
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` tdeCredentialPassword
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` engine

instance Prelude.NFData CreateDBInstance where
  rnf CreateDBInstance' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf backupTarget
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf customIamInstanceProfile
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf dbSecurityGroups
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf
        enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        enableCustomerOwnedIp
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf
        enablePerformanceInsights
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf
        licenseModel
      `Prelude.seq` Prelude.rnf
        manageMasterUserPassword
      `Prelude.seq` Prelude.rnf
        masterUserPassword
      `Prelude.seq` Prelude.rnf
        masterUserSecretKmsKeyId
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        maxAllocatedStorage
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        ncharCharacterSetName
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        optionGroupName
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf
        port
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        processorFeatures
      `Prelude.seq` Prelude.rnf
        promotionTier
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        storageThroughput
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        tdeCredentialPassword
      `Prelude.seq` Prelude.rnf
        timezone
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        dbInstanceClass
      `Prelude.seq` Prelude.rnf
        engine

instance Data.ToHeaders CreateDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBInstance where
  toQuery CreateDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllocatedStorage" Data.=: allocatedStorage,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "AvailabilityZone" Data.=: availabilityZone,
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "BackupTarget" Data.=: backupTarget,
        "CACertificateIdentifier"
          Data.=: cACertificateIdentifier,
        "CharacterSetName" Data.=: characterSetName,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "CustomIamInstanceProfile"
          Data.=: customIamInstanceProfile,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "DBName" Data.=: dbName,
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "DBSecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroups
            ),
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableCustomerOwnedIp"
          Data.=: enableCustomerOwnedIp,
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EnablePerformanceInsights"
          Data.=: enablePerformanceInsights,
        "EngineVersion" Data.=: engineVersion,
        "Iops" Data.=: iops,
        "KmsKeyId" Data.=: kmsKeyId,
        "LicenseModel" Data.=: licenseModel,
        "ManageMasterUserPassword"
          Data.=: manageMasterUserPassword,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MasterUserSecretKmsKeyId"
          Data.=: masterUserSecretKmsKeyId,
        "MasterUsername" Data.=: masterUsername,
        "MaxAllocatedStorage" Data.=: maxAllocatedStorage,
        "MonitoringInterval" Data.=: monitoringInterval,
        "MonitoringRoleArn" Data.=: monitoringRoleArn,
        "MultiAZ" Data.=: multiAZ,
        "NcharCharacterSetName"
          Data.=: ncharCharacterSetName,
        "NetworkType" Data.=: networkType,
        "OptionGroupName" Data.=: optionGroupName,
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "PerformanceInsightsRetentionPeriod"
          Data.=: performanceInsightsRetentionPeriod,
        "Port" Data.=: port,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "ProcessorFeatures"
          Data.=: Data.toQuery
            ( Data.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "PromotionTier" Data.=: promotionTier,
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "StorageEncrypted" Data.=: storageEncrypted,
        "StorageThroughput" Data.=: storageThroughput,
        "StorageType" Data.=: storageType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "TdeCredentialArn" Data.=: tdeCredentialArn,
        "TdeCredentialPassword"
          Data.=: tdeCredentialPassword,
        "Timezone" Data.=: timezone,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier,
        "DBInstanceClass" Data.=: dbInstanceClass,
        "Engine" Data.=: engine
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

instance Prelude.NFData CreateDBInstanceResponse where
  rnf CreateDBInstanceResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
