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
-- Module      : Network.AWS.RDS.CreateDBCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Aurora DB cluster.
--
-- You can use the @ReplicationSourceIdentifier@ parameter to create the DB
-- cluster as a read replica of another DB cluster or Amazon RDS MySQL DB
-- instance. For cross-region replication where the DB cluster identified
-- by @ReplicationSourceIdentifier@ is encrypted, you must also specify the
-- @PreSignedUrl@ parameter.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.CreateDBCluster
  ( -- * Creating a Request
    CreateDBCluster (..),
    newCreateDBCluster,

    -- * Request Lenses
    createDBCluster_backupRetentionPeriod,
    createDBCluster_deletionProtection,
    createDBCluster_storageEncrypted,
    createDBCluster_preferredBackupWindow,
    createDBCluster_availabilityZones,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_optionGroupName,
    createDBCluster_domain,
    createDBCluster_engineMode,
    createDBCluster_scalingConfiguration,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUsername,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_enableHttpEndpoint,
    createDBCluster_kmsKeyId,
    createDBCluster_engineVersion,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_characterSetName,
    createDBCluster_enableGlobalWriteForwarding,
    createDBCluster_tags,
    createDBCluster_port,
    createDBCluster_domainIAMRoleName,
    createDBCluster_preSignedUrl,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_backtrackWindow,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_databaseName,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,

    -- * Destructuring the Response
    CreateDBClusterResponse (..),
    newCreateDBClusterResponse,

    -- * Response Lenses
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,
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
-- /See:/ 'newCreateDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { -- | The number of days for which automated backups are retained.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
    -- in the /Amazon Aurora User Guide./
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
    -- | A list of Availability Zones (AZs) where instances in the DB cluster can
    -- be created. For information on AWS Regions and Availability Zones, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones>
    -- in the /Amazon Aurora User Guide/.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of log types that need to be enabled for exporting to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    --
    -- __Aurora MySQL__
    --
    -- Possible values are @audit@, @error@, @general@, and @slowquery@.
    --
    -- __Aurora PostgreSQL__
    --
    -- Possible value is @postgresql@.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates that the DB cluster should be associated with the
    -- specified option group.
    --
    -- Permanent options can\'t be removed from an option group. The option
    -- group can\'t be removed from a DB cluster once it is associated with a
    -- DB cluster.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory directory ID to create the DB cluster in.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
    -- Authentication to authenticate users that connect to the DB cluster. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- The @parallelquery@ engine mode isn\'t required for Aurora MySQL version
    -- 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
    --
    -- The @global@ engine mode isn\'t required for Aurora MySQL version 1.22
    -- and higher 1.x versions, and @global@ engine mode isn\'t required for
    -- any 2.x versions.
    --
    -- The @multimaster@ engine mode only applies for DB clusters created with
    -- Aurora MySQL version 5.6.10a.
    --
    -- For Aurora PostgreSQL, the @global@ engine mode isn\'t required, and
    -- both the @parallelquery@ and the @multimaster@ engine modes currently
    -- aren\'t supported.
    --
    -- Limitations and requirements apply to some DB engine modes. For more
    -- information, see the following sections in the /Amazon Aurora User
    -- Guide/:
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless>
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties
    -- of the DB cluster.
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
    -- | A DB subnet group to associate with this DB cluster.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not
    -- be default.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The password for the master database user. This password can contain any
    -- printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The name of the master user for the DB cluster.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | A list of EC2 VPC security groups to associate with this DB cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable the HTTP endpoint for an Aurora
    -- Serverless DB cluster. By default, the HTTP endpoint is disabled.
    --
    -- When enabled, the HTTP endpoint provides a connectionless web service
    -- API for running SQL queries on the Aurora Serverless DB cluster. You can
    -- also query your database from inside the RDS console with the query
    -- editor.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
    -- in the /Amazon Aurora User Guide/.
    enableHttpEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | The AWS KMS key identifier for an encrypted DB cluster.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK). To use a CMK in a
    -- different AWS account, specify the key ARN or alias ARN.
    --
    -- When a CMK isn\'t specified in @KmsKeyId@:
    --
    -- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
    --     then Amazon RDS will use the CMK used to encrypt the source.
    --     Otherwise, Amazon RDS will use your default CMK.
    --
    -- -   If the @StorageEncrypted@ parameter is enabled and
    --     @ReplicationSourceIdentifier@ isn\'t specified, then Amazon RDS will
    --     use your default CMK.
    --
    -- There is a default CMK for your AWS account. Your AWS account has a
    -- different default CMK for each AWS Region.
    --
    -- If you create a read replica of an encrypted DB cluster in another AWS
    -- Region, you must set @KmsKeyId@ to a AWS KMS key identifier that is
    -- valid in the destination AWS Region. This CMK is used to encrypt the
    -- read replica in that AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the database engine to use.
    --
    -- To list all of the available engine versions for @aurora@ (for MySQL
    -- 5.6-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-mysql@ (for
    -- MySQL 5.7-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-postgresql@,
    -- use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- __Aurora MySQL__
    --
    -- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
    -- @5.7.mysql_aurora.2.04.5@
    --
    -- __Aurora PostgreSQL__
    --
    -- Example: @9.6.3@, @10.7@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each AWS Region, occurring on a random day of the
    -- week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that the DB cluster should be associated with the
    -- specified CharacterSet.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable this DB cluster to forward
    -- write operations to the primary cluster of an Aurora global database
    -- (GlobalCluster). By default, write operations are not allowed on Aurora
    -- DB clusters that are secondary clusters in an Aurora global database.
    --
    -- You can set this value only on Aurora DB clusters that are members of an
    -- Aurora global database. With this parameter enabled, a secondary cluster
    -- can forward writes to the current primary cluster and the resulting
    -- changes are replicated back to this cluster. For the primary DB cluster
    -- of an Aurora global database, this value is used immediately if the
    -- primary is demoted by the FailoverGlobalCluster API operation, but it
    -- does nothing until then.
    enableGlobalWriteForwarding :: Prelude.Maybe Prelude.Bool,
    -- | Tags to assign to the DB cluster.
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the instances in the DB cluster accept
    -- connections.
    --
    -- Default: @3306@ if engine is set as aurora or @5432@ if set to
    -- aurora-postgresql.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | A URL that contains a Signature Version 4 signed request for the
    -- @CreateDBCluster@ action to be called in the source AWS Region where the
    -- DB cluster is replicated from. You only need to specify @PreSignedUrl@
    -- when you are performing cross-region replication from an encrypted DB
    -- cluster.
    --
    -- The pre-signed URL must be a valid request for the @CreateDBCluster@ API
    -- action that can be executed in the source AWS Region that contains the
    -- encrypted DB cluster to be copied.
    --
    -- The pre-signed URL request must contain the following parameter values:
    --
    -- -   @KmsKeyId@ - The AWS KMS key identifier for the key to use to
    --     encrypt the copy of the DB cluster in the destination AWS Region.
    --     This should refer to the same AWS KMS CMK for both the
    --     @CreateDBCluster@ action that is called in the destination AWS
    --     Region, and the action contained in the pre-signed URL.
    --
    -- -   @DestinationRegion@ - The name of the AWS Region that Aurora read
    --     replica will be created in.
    --
    -- -   @ReplicationSourceIdentifier@ - The DB cluster identifier for the
    --     encrypted DB cluster to be copied. This identifier must be in the
    --     Amazon Resource Name (ARN) format for the source AWS Region. For
    --     example, if you are copying an encrypted DB cluster from the
    --     us-west-2 AWS Region, then your @ReplicationSourceIdentifier@ would
    --     look like Example:
    --     @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@.
    --
    -- To learn how to generate a Signature Version 4 signed request, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
    --
    -- If you are using an AWS SDK tool or the AWS CLI, you can specify
    -- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
    -- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
    -- autogenerates a pre-signed URL that is a valid request for the operation
    -- that can be executed in the source AWS Region.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the DB cluster to
    -- snapshots of the DB cluster. The default is not to copy them.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The target backtrack window, in seconds. To disable backtracking, set
    -- this value to 0.
    --
    -- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
    --
    -- Default: 0
    --
    -- Constraints:
    --
    -- -   If specified, this value must be set to a number from 0 to 259,200
    --     (72 hours).
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | The name of the DB cluster parameter group to associate with this DB
    -- cluster. If you do not specify a value, then the default DB cluster
    -- parameter group for the specified DB engine and version is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing DB cluster parameter
    --     group.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster
    -- if this DB cluster is created as a read replica.
    replicationSourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The global cluster ID of an Aurora cluster that becomes the primary
    -- cluster in the new global database cluster.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name for your database of up to 64 alphanumeric characters. If you
    -- do not provide a name, Amazon RDS doesn\'t create a database in the DB
    -- cluster you are creating.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The DB cluster identifier. This parameter is stored as a lowercase
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
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    --
    -- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
    -- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
    engine :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'createDBCluster_backupRetentionPeriod' - The number of days for which automated backups are retained.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- 'deletionProtection', 'createDBCluster_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'storageEncrypted', 'createDBCluster_storageEncrypted' - A value that indicates whether the DB cluster is encrypted.
--
-- 'preferredBackupWindow', 'createDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
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
-- 'availabilityZones', 'createDBCluster_availabilityZones' - A list of Availability Zones (AZs) where instances in the DB cluster can
-- be created. For information on AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones>
-- in the /Amazon Aurora User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'createDBCluster_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
--
-- 'enableCloudwatchLogsExports', 'createDBCluster_enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
--
-- 'optionGroupName', 'createDBCluster_optionGroupName' - A value that indicates that the DB cluster should be associated with the
-- specified option group.
--
-- Permanent options can\'t be removed from an option group. The option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
--
-- 'domain', 'createDBCluster_domain' - The Active Directory directory ID to create the DB cluster in.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- 'engineMode', 'createDBCluster_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- The @parallelquery@ engine mode isn\'t required for Aurora MySQL version
-- 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
--
-- The @global@ engine mode isn\'t required for Aurora MySQL version 1.22
-- and higher 1.x versions, and @global@ engine mode isn\'t required for
-- any 2.x versions.
--
-- The @multimaster@ engine mode only applies for DB clusters created with
-- Aurora MySQL version 5.6.10a.
--
-- For Aurora PostgreSQL, the @global@ engine mode isn\'t required, and
-- both the @parallelquery@ and the @multimaster@ engine modes currently
-- aren\'t supported.
--
-- Limitations and requirements apply to some DB engine modes. For more
-- information, see the following sections in the /Amazon Aurora User
-- Guide/:
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
--
-- 'scalingConfiguration', 'createDBCluster_scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- 'dbSubnetGroupName', 'createDBCluster_dbSubnetGroupName' - A DB subnet group to associate with this DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
--
-- 'masterUserPassword', 'createDBCluster_masterUserPassword' - The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- 'masterUsername', 'createDBCluster_masterUsername' - The name of the master user for the DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'vpcSecurityGroupIds', 'createDBCluster_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB cluster.
--
-- 'enableHttpEndpoint', 'createDBCluster_enableHttpEndpoint' - A value that indicates whether to enable the HTTP endpoint for an Aurora
-- Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless DB cluster. You can
-- also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
--
-- 'kmsKeyId', 'createDBCluster_kmsKeyId' - The AWS KMS key identifier for an encrypted DB cluster.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- When a CMK isn\'t specified in @KmsKeyId@:
--
-- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
--     then Amazon RDS will use the CMK used to encrypt the source.
--     Otherwise, Amazon RDS will use your default CMK.
--
-- -   If the @StorageEncrypted@ parameter is enabled and
--     @ReplicationSourceIdentifier@ isn\'t specified, then Amazon RDS will
--     use your default CMK.
--
-- There is a default CMK for your AWS account. Your AWS account has a
-- different default CMK for each AWS Region.
--
-- If you create a read replica of an encrypted DB cluster in another AWS
-- Region, you must set @KmsKeyId@ to a AWS KMS key identifier that is
-- valid in the destination AWS Region. This CMK is used to encrypt the
-- read replica in that AWS Region.
--
-- 'engineVersion', 'createDBCluster_engineVersion' - The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
--
-- 'preferredMaintenanceWindow', 'createDBCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'characterSetName', 'createDBCluster_characterSetName' - A value that indicates that the DB cluster should be associated with the
-- specified CharacterSet.
--
-- 'enableGlobalWriteForwarding', 'createDBCluster_enableGlobalWriteForwarding' - A value that indicates whether to enable this DB cluster to forward
-- write operations to the primary cluster of an Aurora global database
-- (GlobalCluster). By default, write operations are not allowed on Aurora
-- DB clusters that are secondary clusters in an Aurora global database.
--
-- You can set this value only on Aurora DB clusters that are members of an
-- Aurora global database. With this parameter enabled, a secondary cluster
-- can forward writes to the current primary cluster and the resulting
-- changes are replicated back to this cluster. For the primary DB cluster
-- of an Aurora global database, this value is used immediately if the
-- primary is demoted by the FailoverGlobalCluster API operation, but it
-- does nothing until then.
--
-- 'tags', 'createDBCluster_tags' - Tags to assign to the DB cluster.
--
-- 'port', 'createDBCluster_port' - The port number on which the instances in the DB cluster accept
-- connections.
--
-- Default: @3306@ if engine is set as aurora or @5432@ if set to
-- aurora-postgresql.
--
-- 'domainIAMRoleName', 'createDBCluster_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'preSignedUrl', 'createDBCluster_preSignedUrl' - A URL that contains a Signature Version 4 signed request for the
-- @CreateDBCluster@ action to be called in the source AWS Region where the
-- DB cluster is replicated from. You only need to specify @PreSignedUrl@
-- when you are performing cross-region replication from an encrypted DB
-- cluster.
--
-- The pre-signed URL must be a valid request for the @CreateDBCluster@ API
-- action that can be executed in the source AWS Region that contains the
-- encrypted DB cluster to be copied.
--
-- The pre-signed URL request must contain the following parameter values:
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the key to use to
--     encrypt the copy of the DB cluster in the destination AWS Region.
--     This should refer to the same AWS KMS CMK for both the
--     @CreateDBCluster@ action that is called in the destination AWS
--     Region, and the action contained in the pre-signed URL.
--
-- -   @DestinationRegion@ - The name of the AWS Region that Aurora read
--     replica will be created in.
--
-- -   @ReplicationSourceIdentifier@ - The DB cluster identifier for the
--     encrypted DB cluster to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source AWS Region. For
--     example, if you are copying an encrypted DB cluster from the
--     us-west-2 AWS Region, then your @ReplicationSourceIdentifier@ would
--     look like Example:
--     @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an AWS SDK tool or the AWS CLI, you can specify
-- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a pre-signed URL that is a valid request for the operation
-- that can be executed in the source AWS Region.
--
-- 'copyTagsToSnapshot', 'createDBCluster_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
--
-- 'backtrackWindow', 'createDBCluster_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- 'dbClusterParameterGroupName', 'createDBCluster_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB
-- cluster. If you do not specify a value, then the default DB cluster
-- parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
--
-- 'replicationSourceIdentifier', 'createDBCluster_replicationSourceIdentifier' - The Amazon Resource Name (ARN) of the source DB instance or DB cluster
-- if this DB cluster is created as a read replica.
--
-- 'globalClusterIdentifier', 'createDBCluster_globalClusterIdentifier' - The global cluster ID of an Aurora cluster that becomes the primary
-- cluster in the new global database cluster.
--
-- 'databaseName', 'createDBCluster_databaseName' - The name for your database of up to 64 alphanumeric characters. If you
-- do not provide a name, Amazon RDS doesn\'t create a database in the DB
-- cluster you are creating.
--
-- 'dbClusterIdentifier', 'createDBCluster_dbClusterIdentifier' - The DB cluster identifier. This parameter is stored as a lowercase
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
-- Example: @my-cluster1@
--
-- 'engine', 'createDBCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
-- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
newCreateDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  CreateDBCluster
newCreateDBCluster pDBClusterIdentifier_ pEngine_ =
  CreateDBCluster'
    { backupRetentionPeriod =
        Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      enableCloudwatchLogsExports = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      domain = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      scalingConfiguration = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      enableHttpEndpoint = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      enableGlobalWriteForwarding = Prelude.Nothing,
      tags = Prelude.Nothing,
      port = Prelude.Nothing,
      domainIAMRoleName = Prelude.Nothing,
      preSignedUrl = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      backtrackWindow = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing,
      replicationSourceIdentifier = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      engine = pEngine_
    }

-- | The number of days for which automated backups are retained.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
createDBCluster_backupRetentionPeriod :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_backupRetentionPeriod = Lens.lens (\CreateDBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@CreateDBCluster' {} a -> s {backupRetentionPeriod = a} :: CreateDBCluster)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
createDBCluster_deletionProtection :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_deletionProtection = Lens.lens (\CreateDBCluster' {deletionProtection} -> deletionProtection) (\s@CreateDBCluster' {} a -> s {deletionProtection = a} :: CreateDBCluster)

-- | A value that indicates whether the DB cluster is encrypted.
createDBCluster_storageEncrypted :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_storageEncrypted = Lens.lens (\CreateDBCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateDBCluster' {} a -> s {storageEncrypted = a} :: CreateDBCluster)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
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
createDBCluster_preferredBackupWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preferredBackupWindow = Lens.lens (\CreateDBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateDBCluster' {} a -> s {preferredBackupWindow = a} :: CreateDBCluster)

-- | A list of Availability Zones (AZs) where instances in the DB cluster can
-- be created. For information on AWS Regions and Availability Zones, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones>
-- in the /Amazon Aurora User Guide/.
createDBCluster_availabilityZones :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_availabilityZones = Lens.lens (\CreateDBCluster' {availabilityZones} -> availabilityZones) (\s@CreateDBCluster' {} a -> s {availabilityZones = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
createDBCluster_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBCluster' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBCluster' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBCluster)

-- | The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
createDBCluster_enableCloudwatchLogsExports :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_enableCloudwatchLogsExports = Lens.lens (\CreateDBCluster' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBCluster' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates that the DB cluster should be associated with the
-- specified option group.
--
-- Permanent options can\'t be removed from an option group. The option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
createDBCluster_optionGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_optionGroupName = Lens.lens (\CreateDBCluster' {optionGroupName} -> optionGroupName) (\s@CreateDBCluster' {} a -> s {optionGroupName = a} :: CreateDBCluster)

-- | The Active Directory directory ID to create the DB cluster in.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
createDBCluster_domain :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_domain = Lens.lens (\CreateDBCluster' {domain} -> domain) (\s@CreateDBCluster' {} a -> s {domain = a} :: CreateDBCluster)

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- The @parallelquery@ engine mode isn\'t required for Aurora MySQL version
-- 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
--
-- The @global@ engine mode isn\'t required for Aurora MySQL version 1.22
-- and higher 1.x versions, and @global@ engine mode isn\'t required for
-- any 2.x versions.
--
-- The @multimaster@ engine mode only applies for DB clusters created with
-- Aurora MySQL version 5.6.10a.
--
-- For Aurora PostgreSQL, the @global@ engine mode isn\'t required, and
-- both the @parallelquery@ and the @multimaster@ engine modes currently
-- aren\'t supported.
--
-- Limitations and requirements apply to some DB engine modes. For more
-- information, see the following sections in the /Amazon Aurora User
-- Guide/:
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
createDBCluster_engineMode :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_engineMode = Lens.lens (\CreateDBCluster' {engineMode} -> engineMode) (\s@CreateDBCluster' {} a -> s {engineMode = a} :: CreateDBCluster)

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
createDBCluster_scalingConfiguration :: Lens.Lens' CreateDBCluster (Prelude.Maybe ScalingConfiguration)
createDBCluster_scalingConfiguration = Lens.lens (\CreateDBCluster' {scalingConfiguration} -> scalingConfiguration) (\s@CreateDBCluster' {} a -> s {scalingConfiguration = a} :: CreateDBCluster)

-- | A DB subnet group to associate with this DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
createDBCluster_dbSubnetGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbSubnetGroupName = Lens.lens (\CreateDBCluster' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBCluster' {} a -> s {dbSubnetGroupName = a} :: CreateDBCluster)

-- | The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
createDBCluster_masterUserPassword :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUserPassword = Lens.lens (\CreateDBCluster' {masterUserPassword} -> masterUserPassword) (\s@CreateDBCluster' {} a -> s {masterUserPassword = a} :: CreateDBCluster)

-- | The name of the master user for the DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
createDBCluster_masterUsername :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUsername = Lens.lens (\CreateDBCluster' {masterUsername} -> masterUsername) (\s@CreateDBCluster' {} a -> s {masterUsername = a} :: CreateDBCluster)

-- | A list of EC2 VPC security groups to associate with this DB cluster.
createDBCluster_vpcSecurityGroupIds :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_vpcSecurityGroupIds = Lens.lens (\CreateDBCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBCluster' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora
-- Serverless DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless DB cluster. You can
-- also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless>
-- in the /Amazon Aurora User Guide/.
createDBCluster_enableHttpEndpoint :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enableHttpEndpoint = Lens.lens (\CreateDBCluster' {enableHttpEndpoint} -> enableHttpEndpoint) (\s@CreateDBCluster' {} a -> s {enableHttpEndpoint = a} :: CreateDBCluster)

-- | The AWS KMS key identifier for an encrypted DB cluster.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK). To use a CMK in a
-- different AWS account, specify the key ARN or alias ARN.
--
-- When a CMK isn\'t specified in @KmsKeyId@:
--
-- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
--     then Amazon RDS will use the CMK used to encrypt the source.
--     Otherwise, Amazon RDS will use your default CMK.
--
-- -   If the @StorageEncrypted@ parameter is enabled and
--     @ReplicationSourceIdentifier@ isn\'t specified, then Amazon RDS will
--     use your default CMK.
--
-- There is a default CMK for your AWS account. Your AWS account has a
-- different default CMK for each AWS Region.
--
-- If you create a read replica of an encrypted DB cluster in another AWS
-- Region, you must set @KmsKeyId@ to a AWS KMS key identifier that is
-- valid in the destination AWS Region. This CMK is used to encrypt the
-- read replica in that AWS Region.
createDBCluster_kmsKeyId :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_kmsKeyId = Lens.lens (\CreateDBCluster' {kmsKeyId} -> kmsKeyId) (\s@CreateDBCluster' {} a -> s {kmsKeyId = a} :: CreateDBCluster)

-- | The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
createDBCluster_engineVersion :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_engineVersion = Lens.lens (\CreateDBCluster' {engineVersion} -> engineVersion) (\s@CreateDBCluster' {} a -> s {engineVersion = a} :: CreateDBCluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each AWS Region, occurring on a random day of the
-- week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
createDBCluster_preferredMaintenanceWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preferredMaintenanceWindow = Lens.lens (\CreateDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBCluster)

-- | A value that indicates that the DB cluster should be associated with the
-- specified CharacterSet.
createDBCluster_characterSetName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_characterSetName = Lens.lens (\CreateDBCluster' {characterSetName} -> characterSetName) (\s@CreateDBCluster' {} a -> s {characterSetName = a} :: CreateDBCluster)

-- | A value that indicates whether to enable this DB cluster to forward
-- write operations to the primary cluster of an Aurora global database
-- (GlobalCluster). By default, write operations are not allowed on Aurora
-- DB clusters that are secondary clusters in an Aurora global database.
--
-- You can set this value only on Aurora DB clusters that are members of an
-- Aurora global database. With this parameter enabled, a secondary cluster
-- can forward writes to the current primary cluster and the resulting
-- changes are replicated back to this cluster. For the primary DB cluster
-- of an Aurora global database, this value is used immediately if the
-- primary is demoted by the FailoverGlobalCluster API operation, but it
-- does nothing until then.
createDBCluster_enableGlobalWriteForwarding :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enableGlobalWriteForwarding = Lens.lens (\CreateDBCluster' {enableGlobalWriteForwarding} -> enableGlobalWriteForwarding) (\s@CreateDBCluster' {} a -> s {enableGlobalWriteForwarding = a} :: CreateDBCluster)

-- | Tags to assign to the DB cluster.
createDBCluster_tags :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Tag])
createDBCluster_tags = Lens.lens (\CreateDBCluster' {tags} -> tags) (\s@CreateDBCluster' {} a -> s {tags = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The port number on which the instances in the DB cluster accept
-- connections.
--
-- Default: @3306@ if engine is set as aurora or @5432@ if set to
-- aurora-postgresql.
createDBCluster_port :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_port = Lens.lens (\CreateDBCluster' {port} -> port) (\s@CreateDBCluster' {} a -> s {port = a} :: CreateDBCluster)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
createDBCluster_domainIAMRoleName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_domainIAMRoleName = Lens.lens (\CreateDBCluster' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBCluster' {} a -> s {domainIAMRoleName = a} :: CreateDBCluster)

-- | A URL that contains a Signature Version 4 signed request for the
-- @CreateDBCluster@ action to be called in the source AWS Region where the
-- DB cluster is replicated from. You only need to specify @PreSignedUrl@
-- when you are performing cross-region replication from an encrypted DB
-- cluster.
--
-- The pre-signed URL must be a valid request for the @CreateDBCluster@ API
-- action that can be executed in the source AWS Region that contains the
-- encrypted DB cluster to be copied.
--
-- The pre-signed URL request must contain the following parameter values:
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the key to use to
--     encrypt the copy of the DB cluster in the destination AWS Region.
--     This should refer to the same AWS KMS CMK for both the
--     @CreateDBCluster@ action that is called in the destination AWS
--     Region, and the action contained in the pre-signed URL.
--
-- -   @DestinationRegion@ - The name of the AWS Region that Aurora read
--     replica will be created in.
--
-- -   @ReplicationSourceIdentifier@ - The DB cluster identifier for the
--     encrypted DB cluster to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source AWS Region. For
--     example, if you are copying an encrypted DB cluster from the
--     us-west-2 AWS Region, then your @ReplicationSourceIdentifier@ would
--     look like Example:
--     @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an AWS SDK tool or the AWS CLI, you can specify
-- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a pre-signed URL that is a valid request for the operation
-- that can be executed in the source AWS Region.
createDBCluster_preSignedUrl :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preSignedUrl = Lens.lens (\CreateDBCluster' {preSignedUrl} -> preSignedUrl) (\s@CreateDBCluster' {} a -> s {preSignedUrl = a} :: CreateDBCluster)

-- | A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
createDBCluster_copyTagsToSnapshot :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_copyTagsToSnapshot = Lens.lens (\CreateDBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBCluster' {} a -> s {copyTagsToSnapshot = a} :: CreateDBCluster)

-- | The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
createDBCluster_backtrackWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Integer)
createDBCluster_backtrackWindow = Lens.lens (\CreateDBCluster' {backtrackWindow} -> backtrackWindow) (\s@CreateDBCluster' {} a -> s {backtrackWindow = a} :: CreateDBCluster)

-- | The name of the DB cluster parameter group to associate with this DB
-- cluster. If you do not specify a value, then the default DB cluster
-- parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
createDBCluster_dbClusterParameterGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbClusterParameterGroupName = Lens.lens (\CreateDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@CreateDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: CreateDBCluster)

-- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster
-- if this DB cluster is created as a read replica.
createDBCluster_replicationSourceIdentifier :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_replicationSourceIdentifier = Lens.lens (\CreateDBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@CreateDBCluster' {} a -> s {replicationSourceIdentifier = a} :: CreateDBCluster)

-- | The global cluster ID of an Aurora cluster that becomes the primary
-- cluster in the new global database cluster.
createDBCluster_globalClusterIdentifier :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_globalClusterIdentifier = Lens.lens (\CreateDBCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateDBCluster' {} a -> s {globalClusterIdentifier = a} :: CreateDBCluster)

-- | The name for your database of up to 64 alphanumeric characters. If you
-- do not provide a name, Amazon RDS doesn\'t create a database in the DB
-- cluster you are creating.
createDBCluster_databaseName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_databaseName = Lens.lens (\CreateDBCluster' {databaseName} -> databaseName) (\s@CreateDBCluster' {} a -> s {databaseName = a} :: CreateDBCluster)

-- | The DB cluster identifier. This parameter is stored as a lowercase
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
-- Example: @my-cluster1@
createDBCluster_dbClusterIdentifier :: Lens.Lens' CreateDBCluster Prelude.Text
createDBCluster_dbClusterIdentifier = Lens.lens (\CreateDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBCluster' {} a -> s {dbClusterIdentifier = a} :: CreateDBCluster)

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
-- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
createDBCluster_engine :: Lens.Lens' CreateDBCluster Prelude.Text
createDBCluster_engine = Lens.lens (\CreateDBCluster' {engine} -> engine) (\s@CreateDBCluster' {} a -> s {engine = a} :: CreateDBCluster)

instance Core.AWSRequest CreateDBCluster where
  type
    AWSResponse CreateDBCluster =
      CreateDBClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterResult"
      ( \s h x ->
          CreateDBClusterResponse'
            Prelude.<$> (x Core..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBCluster

instance Prelude.NFData CreateDBCluster

instance Core.ToHeaders CreateDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDBCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDBCluster where
  toQuery CreateDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateDBCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "AvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "OptionGroupName" Core.=: optionGroupName,
        "Domain" Core.=: domain,
        "EngineMode" Core.=: engineMode,
        "ScalingConfiguration" Core.=: scalingConfiguration,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "MasterUserPassword" Core.=: masterUserPassword,
        "MasterUsername" Core.=: masterUsername,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "EnableHttpEndpoint" Core.=: enableHttpEndpoint,
        "KmsKeyId" Core.=: kmsKeyId,
        "EngineVersion" Core.=: engineVersion,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "CharacterSetName" Core.=: characterSetName,
        "EnableGlobalWriteForwarding"
          Core.=: enableGlobalWriteForwarding,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "PreSignedUrl" Core.=: preSignedUrl,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "BacktrackWindow" Core.=: backtrackWindow,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "ReplicationSourceIdentifier"
          Core.=: replicationSourceIdentifier,
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier,
        "DatabaseName" Core.=: databaseName,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "Engine" Core.=: engine
      ]

-- | /See:/ 'newCreateDBClusterResponse' smart constructor.
data CreateDBClusterResponse = CreateDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'createDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'createDBClusterResponse_httpStatus' - The response's http status code.
newCreateDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBClusterResponse
newCreateDBClusterResponse pHttpStatus_ =
  CreateDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBClusterResponse_dbCluster :: Lens.Lens' CreateDBClusterResponse (Prelude.Maybe DBCluster)
createDBClusterResponse_dbCluster = Lens.lens (\CreateDBClusterResponse' {dbCluster} -> dbCluster) (\s@CreateDBClusterResponse' {} a -> s {dbCluster = a} :: CreateDBClusterResponse)

-- | The response's http status code.
createDBClusterResponse_httpStatus :: Lens.Lens' CreateDBClusterResponse Prelude.Int
createDBClusterResponse_httpStatus = Lens.lens (\CreateDBClusterResponse' {httpStatus} -> httpStatus) (\s@CreateDBClusterResponse' {} a -> s {httpStatus = a} :: CreateDBClusterResponse)

instance Prelude.NFData CreateDBClusterResponse
