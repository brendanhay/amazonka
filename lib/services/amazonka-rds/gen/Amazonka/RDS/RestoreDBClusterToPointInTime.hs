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
-- Module      : Amazonka.RDS.RestoreDBClusterToPointInTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB cluster to an arbitrary point in time. Users can restore
-- to any point in time before @LatestRestorableTime@ for up to
-- @BackupRetentionPeriod@ days. The target DB cluster is created from the
-- source DB cluster with the same configuration as the original DB
-- cluster, except that the new DB cluster is created with the default DB
-- security group.
--
-- For Aurora, this action only restores the DB cluster, not the DB
-- instances for that DB cluster. You must invoke the @CreateDBInstance@
-- action to create DB instances for the restored DB cluster, specifying
-- the identifier of the restored DB cluster in @DBClusterIdentifier@. You
-- can create DB instances only after the @RestoreDBClusterToPointInTime@
-- action has completed and the DB cluster is available.
--
-- For more information on Amazon Aurora DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide./
module Amazonka.RDS.RestoreDBClusterToPointInTime
  ( -- * Creating a Request
    RestoreDBClusterToPointInTime (..),
    newRestoreDBClusterToPointInTime,

    -- * Request Lenses
    restoreDBClusterToPointInTime_backtrackWindow,
    restoreDBClusterToPointInTime_copyTagsToSnapshot,
    restoreDBClusterToPointInTime_dbClusterInstanceClass,
    restoreDBClusterToPointInTime_dbClusterParameterGroupName,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_domain,
    restoreDBClusterToPointInTime_domainIAMRoleName,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication,
    restoreDBClusterToPointInTime_engineMode,
    restoreDBClusterToPointInTime_iops,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_networkType,
    restoreDBClusterToPointInTime_optionGroupName,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_publiclyAccessible,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_restoreType,
    restoreDBClusterToPointInTime_scalingConfiguration,
    restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration,
    restoreDBClusterToPointInTime_storageType,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,

    -- * Destructuring the Response
    RestoreDBClusterToPointInTimeResponse (..),
    newRestoreDBClusterToPointInTimeResponse,

    -- * Response Lenses
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,
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
-- /See:/ 'newRestoreDBClusterToPointInTime' smart constructor.
data RestoreDBClusterToPointInTime = RestoreDBClusterToPointInTime'
  { -- | The target backtrack window, in seconds. To disable backtracking, set
    -- this value to 0.
    --
    -- Default: 0
    --
    -- Constraints:
    --
    -- -   If specified, this value must be set to a number from 0 to 259,200
    --     (72 hours).
    --
    -- Valid for: Aurora MySQL DB clusters only
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The compute and memory capacity of the each DB instance in the Multi-AZ
    -- DB cluster, for example db.m6gd.xlarge. Not all DB instance classes are
    -- available in all Amazon Web Services Regions, or for all database
    -- engines.
    --
    -- For the full list of DB instance classes, and availability for your
    -- engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance class>
    -- in the /Amazon RDS User Guide./
    --
    -- Valid for: Multi-AZ DB clusters only
    dbClusterInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group to associate with this DB
    -- cluster. If this argument is omitted, the default DB cluster parameter
    -- group for the specified engine is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing DB cluster parameter
    --     group.
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The DB subnet group name to use for the new DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mydbsubnetgroup@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Specify the Active Directory directory ID to restore the DB cluster in.
    -- The domain must be created prior to this operation.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
    -- Authentication to authenticate users that connect to the DB cluster. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    --
    -- Valid for: Aurora DB clusters only
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used.
    --
    -- __RDS for MySQL__
    --
    -- Possible values are @error@, @general@, and @slowquery@.
    --
    -- __RDS for PostgreSQL__
    --
    -- Possible values are @postgresql@ and @upgrade@.
    --
    -- __Aurora MySQL__
    --
    -- Possible values are @audit@, @error@, @general@, and @slowquery@.
    --
    -- __Aurora PostgreSQL__
    --
    -- Possible value is @postgresql@.
    --
    -- For more information about exporting CloudWatch Logs for Amazon RDS, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about exporting CloudWatch Logs for Amazon Aurora,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping isn\'t enabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The engine mode of the new cluster. Specify @provisioned@ or
    -- @serverless@, depending on the type of the cluster you are creating. You
    -- can create an Aurora Serverless v1 clone from a provisioned cluster, or
    -- a provisioned clone from an Aurora Serverless v1 cluster. To create a
    -- clone that is an Aurora Serverless v1 cluster, the original cluster must
    -- be an Aurora Serverless v1 cluster or an encrypted provisioned cluster.
    --
    -- Valid for: Aurora DB clusters only
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for each DB instance in the Multi-AZ DB cluster.
    --
    -- For information about valid IOPS values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
    -- in the /Amazon RDS User Guide/.
    --
    -- Constraints: Must be a multiple between .5 and 50 of the storage amount
    -- for the DB instance.
    --
    -- Valid for: Multi-AZ DB clusters only
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services KMS key identifier to use when restoring an
    -- encrypted DB cluster from an encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- You can restore to a new DB cluster and encrypt the new DB cluster with
    -- a KMS key that is different from the KMS key used to encrypt the source
    -- DB cluster. The new DB cluster is encrypted with the KMS key identified
    -- by the @KmsKeyId@ parameter.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
    -- following occurs:
    --
    -- -   If the DB cluster is encrypted, then the restored DB cluster is
    --     encrypted using the KMS key that was used to encrypt the source DB
    --     cluster.
    --
    -- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
    --     isn\'t encrypted.
    --
    -- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
    -- then the restore request is rejected.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The network type of the DB cluster.
    --
    -- Valid values:
    --
    -- -   @IPV4@
    --
    -- -   @DUAL@
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for the
    -- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid for: Aurora DB clusters only
    networkType :: Prelude.Maybe Prelude.Text,
    -- | The name of the option group for the new DB cluster.
    --
    -- DB clusters are associated with a default option group that can\'t be
    -- modified.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the new DB cluster accepts connections.
    --
    -- Constraints: A value from @1150-65535@.
    --
    -- Default: The default port for the engine.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the DB cluster is publicly accessible.
    --
    -- When the DB cluster is publicly accessible, its Domain Name System (DNS)
    -- endpoint resolves to the private IP address from within the DB
    -- cluster\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB cluster\'s VPC. Access to the DB cluster
    -- is ultimately controlled by the security group it uses. That public
    -- access is not permitted if the security group assigned to the DB cluster
    -- doesn\'t permit it.
    --
    -- When the DB cluster isn\'t publicly accessible, it is an internal DB
    -- cluster with a DNS name that resolves to a private IP address.
    --
    -- Default: The default behavior varies depending on whether
    -- @DBSubnetGroupName@ is specified.
    --
    -- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the default VPC in the target Region doesn’t have an internet
    --     gateway attached to it, the DB cluster is private.
    --
    -- -   If the default VPC in the target Region has an internet gateway
    --     attached to it, the DB cluster is public.
    --
    -- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the subnets are part of a VPC that doesn’t have an internet
    --     gateway attached to it, the DB cluster is private.
    --
    -- -   If the subnets are part of a VPC that has an internet gateway
    --     attached to it, the DB cluster is public.
    --
    -- Valid for: Multi-AZ DB clusters only
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The date and time to restore the DB cluster to.
    --
    -- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
    -- format
    --
    -- Constraints:
    --
    -- -   Must be before the latest restorable time for the DB instance
    --
    -- -   Must be specified if @UseLatestRestorableTime@ parameter isn\'t
    --     provided
    --
    -- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
    --     enabled
    --
    -- -   Can\'t be specified if the @RestoreType@ parameter is
    --     @copy-on-write@
    --
    -- Example: @2015-03-07T23:45:00Z@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    restoreToTime :: Prelude.Maybe Data.ISO8601,
    -- | The type of restore to be performed. You can specify one of the
    -- following values:
    --
    -- -   @full-copy@ - The new DB cluster is restored as a full copy of the
    --     source DB cluster.
    --
    -- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
    --     source DB cluster.
    --
    -- Constraints: You can\'t specify @copy-on-write@ if the engine version of
    -- the source DB cluster is earlier than 1.11.
    --
    -- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
    -- restored as a full copy of the source DB cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    restoreType :: Prelude.Maybe Prelude.Text,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties
    -- of the DB cluster.
    --
    -- Valid for: Aurora DB clusters only
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | Specifies the storage type to be associated with the each DB instance in
    -- the Multi-AZ DB cluster.
    --
    -- Valid values: @io1@
    --
    -- When specified, a value for the @Iops@ parameter is required.
    --
    -- Default: @io1@
    --
    -- Valid for: Multi-AZ DB clusters only
    storageType :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | A value that indicates whether to restore the DB cluster to the latest
    -- restorable backup time. By default, the DB cluster isn\'t restored to
    -- the latest restorable backup time.
    --
    -- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
    -- provided.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | A list of VPC security groups that the new DB cluster belongs to.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the new DB cluster to be created.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterIdentifier :: Prelude.Text,
    -- | The identifier of the source DB cluster from which to restore.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBCluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    sourceDBClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterToPointInTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backtrackWindow', 'restoreDBClusterToPointInTime_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- Valid for: Aurora MySQL DB clusters only
--
-- 'copyTagsToSnapshot', 'restoreDBClusterToPointInTime_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterInstanceClass', 'restoreDBClusterToPointInTime_dbClusterInstanceClass' - The compute and memory capacity of the each DB instance in the Multi-AZ
-- DB cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes, and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance class>
-- in the /Amazon RDS User Guide./
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'dbClusterParameterGroupName', 'restoreDBClusterToPointInTime_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default DB cluster parameter
-- group for the specified engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbSubnetGroupName', 'restoreDBClusterToPointInTime_dbSubnetGroupName' - The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'deletionProtection', 'restoreDBClusterToPointInTime_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'domain', 'restoreDBClusterToPointInTime_domain' - Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'domainIAMRoleName', 'restoreDBClusterToPointInTime_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterToPointInTime_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used.
--
-- __RDS for MySQL__
--
-- Possible values are @error@, @general@, and @slowquery@.
--
-- __RDS for PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'engineMode', 'restoreDBClusterToPointInTime_engineMode' - The engine mode of the new cluster. Specify @provisioned@ or
-- @serverless@, depending on the type of the cluster you are creating. You
-- can create an Aurora Serverless v1 clone from a provisioned cluster, or
-- a provisioned clone from an Aurora Serverless v1 cluster. To create a
-- clone that is an Aurora Serverless v1 cluster, the original cluster must
-- be an Aurora Serverless v1 cluster or an encrypted provisioned cluster.
--
-- Valid for: Aurora DB clusters only
--
-- 'iops', 'restoreDBClusterToPointInTime_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB instance.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'kmsKeyId', 'restoreDBClusterToPointInTime_kmsKeyId' - The Amazon Web Services KMS key identifier to use when restoring an
-- encrypted DB cluster from an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a KMS key that is different from the KMS key used to encrypt the source
-- DB cluster. The new DB cluster is encrypted with the KMS key identified
-- by the @KmsKeyId@ parameter.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the KMS key that was used to encrypt the source DB
--     cluster.
--
-- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
--     isn\'t encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
-- then the restore request is rejected.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'networkType', 'restoreDBClusterToPointInTime_networkType' - The network type of the DB cluster.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters only
--
-- 'optionGroupName', 'restoreDBClusterToPointInTime_optionGroupName' - The name of the option group for the new DB cluster.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
--
-- 'port', 'restoreDBClusterToPointInTime_port' - The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@.
--
-- Default: The default port for the engine.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'publiclyAccessible', 'restoreDBClusterToPointInTime_publiclyAccessible' - A value that indicates whether the DB cluster is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access is not permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB cluster isn\'t publicly accessible, it is an internal DB
-- cluster with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB cluster is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB cluster is public.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'restoreToTime', 'restoreDBClusterToPointInTime_restoreToTime' - The date and time to restore the DB cluster to.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
-- format
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
--
-- -   Must be specified if @UseLatestRestorableTime@ parameter isn\'t
--     provided
--
-- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
--     enabled
--
-- -   Can\'t be specified if the @RestoreType@ parameter is
--     @copy-on-write@
--
-- Example: @2015-03-07T23:45:00Z@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'restoreType', 'restoreDBClusterToPointInTime_restoreType' - The type of restore to be performed. You can specify one of the
-- following values:
--
-- -   @full-copy@ - The new DB cluster is restored as a full copy of the
--     source DB cluster.
--
-- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
--     source DB cluster.
--
-- Constraints: You can\'t specify @copy-on-write@ if the engine version of
-- the source DB cluster is earlier than 1.11.
--
-- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
-- restored as a full copy of the source DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'scalingConfiguration', 'restoreDBClusterToPointInTime_scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- Valid for: Aurora DB clusters only
--
-- 'serverlessV2ScalingConfiguration', 'restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'storageType', 'restoreDBClusterToPointInTime_storageType' - Specifies the storage type to be associated with the each DB instance in
-- the Multi-AZ DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'tags', 'restoreDBClusterToPointInTime_tags' - Undocumented member.
--
-- 'useLatestRestorableTime', 'restoreDBClusterToPointInTime_useLatestRestorableTime' - A value that indicates whether to restore the DB cluster to the latest
-- restorable backup time. By default, the DB cluster isn\'t restored to
-- the latest restorable backup time.
--
-- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
-- provided.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterToPointInTime_vpcSecurityGroupIds' - A list of VPC security groups that the new DB cluster belongs to.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterIdentifier', 'restoreDBClusterToPointInTime_dbClusterIdentifier' - The name of the new DB cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'sourceDBClusterIdentifier', 'restoreDBClusterToPointInTime_sourceDBClusterIdentifier' - The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
newRestoreDBClusterToPointInTime ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'sourceDBClusterIdentifier'
  Prelude.Text ->
  RestoreDBClusterToPointInTime
newRestoreDBClusterToPointInTime
  pDBClusterIdentifier_
  pSourceDBClusterIdentifier_ =
    RestoreDBClusterToPointInTime'
      { backtrackWindow =
          Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        dbClusterInstanceClass = Prelude.Nothing,
        dbClusterParameterGroupName =
          Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        domain = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        enableCloudwatchLogsExports =
          Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        engineMode = Prelude.Nothing,
        iops = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        networkType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        port = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        restoreToTime = Prelude.Nothing,
        restoreType = Prelude.Nothing,
        scalingConfiguration = Prelude.Nothing,
        serverlessV2ScalingConfiguration =
          Prelude.Nothing,
        storageType = Prelude.Nothing,
        tags = Prelude.Nothing,
        useLatestRestorableTime = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        sourceDBClusterIdentifier =
          pSourceDBClusterIdentifier_
      }

-- | The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- Valid for: Aurora MySQL DB clusters only
restoreDBClusterToPointInTime_backtrackWindow :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Integer)
restoreDBClusterToPointInTime_backtrackWindow = Lens.lens (\RestoreDBClusterToPointInTime' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterToPointInTime' {} a -> s {backtrackWindow = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterToPointInTime' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterToPointInTime' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterToPointInTime)

-- | The compute and memory capacity of the each DB instance in the Multi-AZ
-- DB cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes, and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance class>
-- in the /Amazon RDS User Guide./
--
-- Valid for: Multi-AZ DB clusters only
restoreDBClusterToPointInTime_dbClusterInstanceClass :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbClusterInstanceClass = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterInstanceClass} -> dbClusterInstanceClass) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterInstanceClass = a} :: RestoreDBClusterToPointInTime)

-- | The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default DB cluster parameter
-- group for the specified engine is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The DB subnet group name to use for the new DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mydbsubnetgroup@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_dbSubnetGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_deletionProtection :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_deletionProtection = Lens.lens (\RestoreDBClusterToPointInTime' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterToPointInTime' {} a -> s {deletionProtection = a} :: RestoreDBClusterToPointInTime)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterToPointInTime_domain :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_domain = Lens.lens (\RestoreDBClusterToPointInTime' {domain} -> domain) (\s@RestoreDBClusterToPointInTime' {} a -> s {domain = a} :: RestoreDBClusterToPointInTime)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterToPointInTime_domainIAMRoleName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_domainIAMRoleName = Lens.lens (\RestoreDBClusterToPointInTime' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterToPointInTime' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterToPointInTime)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used.
--
-- __RDS for MySQL__
--
-- Possible values are @error@, @general@, and @slowquery@.
--
-- __RDS for PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterToPointInTime' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterToPointInTime' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterToPointInTime' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterToPointInTime)

-- | The engine mode of the new cluster. Specify @provisioned@ or
-- @serverless@, depending on the type of the cluster you are creating. You
-- can create an Aurora Serverless v1 clone from a provisioned cluster, or
-- a provisioned clone from an Aurora Serverless v1 cluster. To create a
-- clone that is an Aurora Serverless v1 cluster, the original cluster must
-- be an Aurora Serverless v1 cluster or an encrypted provisioned cluster.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterToPointInTime_engineMode :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_engineMode = Lens.lens (\RestoreDBClusterToPointInTime' {engineMode} -> engineMode) (\s@RestoreDBClusterToPointInTime' {} a -> s {engineMode = a} :: RestoreDBClusterToPointInTime)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB instance.
--
-- Valid for: Multi-AZ DB clusters only
restoreDBClusterToPointInTime_iops :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBClusterToPointInTime_iops = Lens.lens (\RestoreDBClusterToPointInTime' {iops} -> iops) (\s@RestoreDBClusterToPointInTime' {} a -> s {iops = a} :: RestoreDBClusterToPointInTime)

-- | The Amazon Web Services KMS key identifier to use when restoring an
-- encrypted DB cluster from an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- You can restore to a new DB cluster and encrypt the new DB cluster with
-- a KMS key that is different from the KMS key used to encrypt the source
-- DB cluster. The new DB cluster is encrypted with the KMS key identified
-- by the @KmsKeyId@ parameter.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then the
-- following occurs:
--
-- -   If the DB cluster is encrypted, then the restored DB cluster is
--     encrypted using the KMS key that was used to encrypt the source DB
--     cluster.
--
-- -   If the DB cluster isn\'t encrypted, then the restored DB cluster
--     isn\'t encrypted.
--
-- If @DBClusterIdentifier@ refers to a DB cluster that isn\'t encrypted,
-- then the restore request is rejected.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_kmsKeyId :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_kmsKeyId = Lens.lens (\RestoreDBClusterToPointInTime' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterToPointInTime' {} a -> s {kmsKeyId = a} :: RestoreDBClusterToPointInTime)

-- | The network type of the DB cluster.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters only
restoreDBClusterToPointInTime_networkType :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_networkType = Lens.lens (\RestoreDBClusterToPointInTime' {networkType} -> networkType) (\s@RestoreDBClusterToPointInTime' {} a -> s {networkType = a} :: RestoreDBClusterToPointInTime)

-- | The name of the option group for the new DB cluster.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
restoreDBClusterToPointInTime_optionGroupName :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_optionGroupName = Lens.lens (\RestoreDBClusterToPointInTime' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterToPointInTime' {} a -> s {optionGroupName = a} :: RestoreDBClusterToPointInTime)

-- | The port number on which the new DB cluster accepts connections.
--
-- Constraints: A value from @1150-65535@.
--
-- Default: The default port for the engine.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_port :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Int)
restoreDBClusterToPointInTime_port = Lens.lens (\RestoreDBClusterToPointInTime' {port} -> port) (\s@RestoreDBClusterToPointInTime' {} a -> s {port = a} :: RestoreDBClusterToPointInTime)

-- | A value that indicates whether the DB cluster is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access is not permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB cluster isn\'t publicly accessible, it is an internal DB
-- cluster with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB cluster is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB cluster is public.
--
-- Valid for: Multi-AZ DB clusters only
restoreDBClusterToPointInTime_publiclyAccessible :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_publiclyAccessible = Lens.lens (\RestoreDBClusterToPointInTime' {publiclyAccessible} -> publiclyAccessible) (\s@RestoreDBClusterToPointInTime' {} a -> s {publiclyAccessible = a} :: RestoreDBClusterToPointInTime)

-- | The date and time to restore the DB cluster to.
--
-- Valid Values: Value must be a time in Universal Coordinated Time (UTC)
-- format
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the DB instance
--
-- -   Must be specified if @UseLatestRestorableTime@ parameter isn\'t
--     provided
--
-- -   Can\'t be specified if the @UseLatestRestorableTime@ parameter is
--     enabled
--
-- -   Can\'t be specified if the @RestoreType@ parameter is
--     @copy-on-write@
--
-- Example: @2015-03-07T23:45:00Z@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_restoreToTime :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.UTCTime)
restoreDBClusterToPointInTime_restoreToTime = Lens.lens (\RestoreDBClusterToPointInTime' {restoreToTime} -> restoreToTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreToTime = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Data._Time

-- | The type of restore to be performed. You can specify one of the
-- following values:
--
-- -   @full-copy@ - The new DB cluster is restored as a full copy of the
--     source DB cluster.
--
-- -   @copy-on-write@ - The new DB cluster is restored as a clone of the
--     source DB cluster.
--
-- Constraints: You can\'t specify @copy-on-write@ if the engine version of
-- the source DB cluster is earlier than 1.11.
--
-- If you don\'t specify a @RestoreType@ value, then the new DB cluster is
-- restored as a full copy of the source DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_restoreType :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_restoreType = Lens.lens (\RestoreDBClusterToPointInTime' {restoreType} -> restoreType) (\s@RestoreDBClusterToPointInTime' {} a -> s {restoreType = a} :: RestoreDBClusterToPointInTime)

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- Valid for: Aurora DB clusters only
restoreDBClusterToPointInTime_scalingConfiguration :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe ScalingConfiguration)
restoreDBClusterToPointInTime_scalingConfiguration = Lens.lens (\RestoreDBClusterToPointInTime' {scalingConfiguration} -> scalingConfiguration) (\s@RestoreDBClusterToPointInTime' {} a -> s {scalingConfiguration = a} :: RestoreDBClusterToPointInTime)

-- | Undocumented member.
restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe ServerlessV2ScalingConfiguration)
restoreDBClusterToPointInTime_serverlessV2ScalingConfiguration = Lens.lens (\RestoreDBClusterToPointInTime' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@RestoreDBClusterToPointInTime' {} a -> s {serverlessV2ScalingConfiguration = a} :: RestoreDBClusterToPointInTime)

-- | Specifies the storage type to be associated with the each DB instance in
-- the Multi-AZ DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Multi-AZ DB clusters only
restoreDBClusterToPointInTime_storageType :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Text)
restoreDBClusterToPointInTime_storageType = Lens.lens (\RestoreDBClusterToPointInTime' {storageType} -> storageType) (\s@RestoreDBClusterToPointInTime' {} a -> s {storageType = a} :: RestoreDBClusterToPointInTime)

-- | Undocumented member.
restoreDBClusterToPointInTime_tags :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Tag])
restoreDBClusterToPointInTime_tags = Lens.lens (\RestoreDBClusterToPointInTime' {tags} -> tags) (\s@RestoreDBClusterToPointInTime' {} a -> s {tags = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to restore the DB cluster to the latest
-- restorable backup time. By default, the DB cluster isn\'t restored to
-- the latest restorable backup time.
--
-- Constraints: Can\'t be specified if @RestoreToTime@ parameter is
-- provided.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_useLatestRestorableTime :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe Prelude.Bool)
restoreDBClusterToPointInTime_useLatestRestorableTime = Lens.lens (\RestoreDBClusterToPointInTime' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@RestoreDBClusterToPointInTime' {} a -> s {useLatestRestorableTime = a} :: RestoreDBClusterToPointInTime)

-- | A list of VPC security groups that the new DB cluster belongs to.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterToPointInTime (Prelude.Maybe [Prelude.Text])
restoreDBClusterToPointInTime_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterToPointInTime' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterToPointInTime' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterToPointInTime) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new DB cluster to be created.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Prelude.Text
restoreDBClusterToPointInTime_dbClusterIdentifier = Lens.lens (\RestoreDBClusterToPointInTime' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterToPointInTime' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterToPointInTime)

-- | The identifier of the source DB cluster from which to restore.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
restoreDBClusterToPointInTime_sourceDBClusterIdentifier :: Lens.Lens' RestoreDBClusterToPointInTime Prelude.Text
restoreDBClusterToPointInTime_sourceDBClusterIdentifier = Lens.lens (\RestoreDBClusterToPointInTime' {sourceDBClusterIdentifier} -> sourceDBClusterIdentifier) (\s@RestoreDBClusterToPointInTime' {} a -> s {sourceDBClusterIdentifier = a} :: RestoreDBClusterToPointInTime)

instance
  Core.AWSRequest
    RestoreDBClusterToPointInTime
  where
  type
    AWSResponse RestoreDBClusterToPointInTime =
      RestoreDBClusterToPointInTimeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterToPointInTimeResult"
      ( \s h x ->
          RestoreDBClusterToPointInTimeResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreDBClusterToPointInTime
  where
  hashWithSalt _salt RestoreDBClusterToPointInTime' {..} =
    _salt
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterInstanceClass
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` engineMode
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` restoreToTime
      `Prelude.hashWithSalt` restoreType
      `Prelude.hashWithSalt` scalingConfiguration
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` useLatestRestorableTime
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` sourceDBClusterIdentifier

instance Prelude.NFData RestoreDBClusterToPointInTime where
  rnf RestoreDBClusterToPointInTime' {..} =
    Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterInstanceClass
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf engineMode
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf networkType
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf restoreToTime
      `Prelude.seq` Prelude.rnf restoreType
      `Prelude.seq` Prelude.rnf
        scalingConfiguration
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf
        useLatestRestorableTime
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier
      `Prelude.seq` Prelude.rnf
        sourceDBClusterIdentifier

instance Data.ToHeaders RestoreDBClusterToPointInTime where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreDBClusterToPointInTime where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreDBClusterToPointInTime where
  toQuery RestoreDBClusterToPointInTime' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RestoreDBClusterToPointInTime" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "BacktrackWindow" Data.=: backtrackWindow,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBClusterInstanceClass"
          Data.=: dbClusterInstanceClass,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EngineMode" Data.=: engineMode,
        "Iops" Data.=: iops,
        "KmsKeyId" Data.=: kmsKeyId,
        "NetworkType" Data.=: networkType,
        "OptionGroupName" Data.=: optionGroupName,
        "Port" Data.=: port,
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "RestoreToTime" Data.=: restoreToTime,
        "RestoreType" Data.=: restoreType,
        "ScalingConfiguration" Data.=: scalingConfiguration,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "StorageType" Data.=: storageType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "UseLatestRestorableTime"
          Data.=: useLatestRestorableTime,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "SourceDBClusterIdentifier"
          Data.=: sourceDBClusterIdentifier
      ]

-- | /See:/ 'newRestoreDBClusterToPointInTimeResponse' smart constructor.
data RestoreDBClusterToPointInTimeResponse = RestoreDBClusterToPointInTimeResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterToPointInTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'restoreDBClusterToPointInTimeResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'restoreDBClusterToPointInTimeResponse_httpStatus' - The response's http status code.
newRestoreDBClusterToPointInTimeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBClusterToPointInTimeResponse
newRestoreDBClusterToPointInTimeResponse pHttpStatus_ =
  RestoreDBClusterToPointInTimeResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBClusterToPointInTimeResponse_dbCluster :: Lens.Lens' RestoreDBClusterToPointInTimeResponse (Prelude.Maybe DBCluster)
restoreDBClusterToPointInTimeResponse_dbCluster = Lens.lens (\RestoreDBClusterToPointInTimeResponse' {dbCluster} -> dbCluster) (\s@RestoreDBClusterToPointInTimeResponse' {} a -> s {dbCluster = a} :: RestoreDBClusterToPointInTimeResponse)

-- | The response's http status code.
restoreDBClusterToPointInTimeResponse_httpStatus :: Lens.Lens' RestoreDBClusterToPointInTimeResponse Prelude.Int
restoreDBClusterToPointInTimeResponse_httpStatus = Lens.lens (\RestoreDBClusterToPointInTimeResponse' {httpStatus} -> httpStatus) (\s@RestoreDBClusterToPointInTimeResponse' {} a -> s {httpStatus = a} :: RestoreDBClusterToPointInTimeResponse)

instance
  Prelude.NFData
    RestoreDBClusterToPointInTimeResponse
  where
  rnf RestoreDBClusterToPointInTimeResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
