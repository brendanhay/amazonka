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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbClusterDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbClusterAssociatedRole
import Amazonka.SecurityHub.Types.AwsRdsDbClusterMember
import Amazonka.SecurityHub.Types.AwsRdsDbClusterOptionGroupMembership
import Amazonka.SecurityHub.Types.AwsRdsDbDomainMembership
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup

-- | Information about an Amazon RDS DB cluster.
--
-- /See:/ 'newAwsRdsDbClusterDetails' smart constructor.
data AwsRdsDbClusterDetails = AwsRdsDbClusterDetails'
  { -- | The port number on which the DB instances in the DB cluster accept
    -- connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the identifier that Amazon Route 53 assigns when you create a
    -- hosted zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | The range of time each day when automated backups are created, if
    -- automated backups are enabled.
    --
    -- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which automated backups are retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the master user for the DB cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Whether tags are copied from the DB cluster to snapshots of the DB
    -- cluster.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The status of the database activity stream. Valid values are as follows:
    --
    -- -   @started@
    --
    -- -   @starting@
    --
    -- -   @stopped@
    --
    -- -   @stopping@
    activityStreamStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group for the DB cluster.
    dbClusterParameterGroup :: Prelude.Maybe Prelude.Text,
    -- | The list of instances that make up the DB cluster.
    dbClusterMembers :: Prelude.Maybe [AwsRdsDbClusterMember],
    -- | The name of the database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The DB cluster identifier that the user assigned to the cluster. This
    -- identifier is the unique key that identifies a DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory domain membership records that are associated with
    -- the DB cluster.
    domainMemberships :: Prelude.Maybe [AwsRdsDbDomainMembership],
    -- | A list of Availability Zones (AZs) where instances in the DB cluster can
    -- be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Whether the DB cluster is a clone of a DB cluster owned by a different
    -- Amazon Web Services account.
    crossAccountClone :: Prelude.Maybe Prelude.Bool,
    -- | The list of option group memberships for this DB cluster.
    dbClusterOptionGroupMemberships :: Prelude.Maybe [AwsRdsDbClusterOptionGroupMembership],
    -- | The subnet group that is associated with the DB cluster, including the
    -- name, description, and subnets in the subnet group.
    dbSubnetGroup :: Prelude.Maybe Prelude.Text,
    -- | The current status of this DB cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | Whether the HTTP endpoint for an Aurora Serverless DB cluster is
    -- enabled.
    httpEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of custom endpoints for the DB cluster.
    customEndpoints :: Prelude.Maybe [Prelude.Text],
    -- | The database engine mode of the DB cluster.Valid values are as follows:
    --
    -- -   @global@
    --
    -- -   @multimaster@
    --
    -- -   @parallelquery@
    --
    -- -   @provisioned@
    --
    -- -   @serverless@
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the KMS master key that is used to encrypt the database
    -- instances in the DB cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine to use for this DB cluster. Valid values
    -- are as follows:
    --
    -- -   @aurora@
    --
    -- -   @aurora-mysql@
    --
    -- -   @aurora-postgresql@
    engine :: Prelude.Maybe Prelude.Text,
    -- | For all database engines except Aurora, specifies the allocated storage
    -- size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The reader endpoint for the DB cluster.
    readerEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB cluster has deletion protection enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Whether the mapping of IAM accounts to database accounts is enabled.
    iamDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Uses the format @\<day>:HH:MM-\<day>:HH:MM@.
    --
    -- For the day values, use @mon@|@tue@|@wed@|@thu@|@fri@|@sat@|@sun@.
    --
    -- For example, @sun:09:32-sun:10:02@.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The connection endpoint for the primary instance of the DB cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the DB cluster was created, in Universal Coordinated Time
    -- (UTC).
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    clusterCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the read replicas that are associated with this DB
    -- cluster.
    readReplicaIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | A list of log types that this DB cluster is configured to export to
    -- CloudWatch Logs.
    enabledCloudWatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the DB cluster. The identifier must be unique within
    -- each Amazon Web Services Region and is immutable.
    dbClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | A list of the IAM roles that are associated with the DB cluster.
    associatedRoles :: Prelude.Maybe [AwsRdsDbClusterAssociatedRole],
    -- | Whether the DB cluster has instances in multiple Availability Zones.
    multiAz :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to use.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC security groups that the DB cluster belongs to.
    vpcSecurityGroups :: Prelude.Maybe [AwsRdsDbInstanceVpcSecurityGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbClusterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'awsRdsDbClusterDetails_port' - The port number on which the DB instances in the DB cluster accept
-- connections.
--
-- 'hostedZoneId', 'awsRdsDbClusterDetails_hostedZoneId' - Specifies the identifier that Amazon Route 53 assigns when you create a
-- hosted zone.
--
-- 'preferredBackupWindow', 'awsRdsDbClusterDetails_preferredBackupWindow' - The range of time each day when automated backups are created, if
-- automated backups are enabled.
--
-- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
--
-- 'backupRetentionPeriod', 'awsRdsDbClusterDetails_backupRetentionPeriod' - The number of days for which automated backups are retained.
--
-- 'masterUsername', 'awsRdsDbClusterDetails_masterUsername' - The name of the master user for the DB cluster.
--
-- 'copyTagsToSnapshot', 'awsRdsDbClusterDetails_copyTagsToSnapshot' - Whether tags are copied from the DB cluster to snapshots of the DB
-- cluster.
--
-- 'activityStreamStatus', 'awsRdsDbClusterDetails_activityStreamStatus' - The status of the database activity stream. Valid values are as follows:
--
-- -   @started@
--
-- -   @starting@
--
-- -   @stopped@
--
-- -   @stopping@
--
-- 'dbClusterParameterGroup', 'awsRdsDbClusterDetails_dbClusterParameterGroup' - The name of the DB cluster parameter group for the DB cluster.
--
-- 'dbClusterMembers', 'awsRdsDbClusterDetails_dbClusterMembers' - The list of instances that make up the DB cluster.
--
-- 'databaseName', 'awsRdsDbClusterDetails_databaseName' - The name of the database.
--
-- 'dbClusterIdentifier', 'awsRdsDbClusterDetails_dbClusterIdentifier' - The DB cluster identifier that the user assigned to the cluster. This
-- identifier is the unique key that identifies a DB cluster.
--
-- 'domainMemberships', 'awsRdsDbClusterDetails_domainMemberships' - The Active Directory domain membership records that are associated with
-- the DB cluster.
--
-- 'availabilityZones', 'awsRdsDbClusterDetails_availabilityZones' - A list of Availability Zones (AZs) where instances in the DB cluster can
-- be created.
--
-- 'crossAccountClone', 'awsRdsDbClusterDetails_crossAccountClone' - Whether the DB cluster is a clone of a DB cluster owned by a different
-- Amazon Web Services account.
--
-- 'dbClusterOptionGroupMemberships', 'awsRdsDbClusterDetails_dbClusterOptionGroupMemberships' - The list of option group memberships for this DB cluster.
--
-- 'dbSubnetGroup', 'awsRdsDbClusterDetails_dbSubnetGroup' - The subnet group that is associated with the DB cluster, including the
-- name, description, and subnets in the subnet group.
--
-- 'status', 'awsRdsDbClusterDetails_status' - The current status of this DB cluster.
--
-- 'httpEndpointEnabled', 'awsRdsDbClusterDetails_httpEndpointEnabled' - Whether the HTTP endpoint for an Aurora Serverless DB cluster is
-- enabled.
--
-- 'customEndpoints', 'awsRdsDbClusterDetails_customEndpoints' - A list of custom endpoints for the DB cluster.
--
-- 'engineMode', 'awsRdsDbClusterDetails_engineMode' - The database engine mode of the DB cluster.Valid values are as follows:
--
-- -   @global@
--
-- -   @multimaster@
--
-- -   @parallelquery@
--
-- -   @provisioned@
--
-- -   @serverless@
--
-- 'storageEncrypted', 'awsRdsDbClusterDetails_storageEncrypted' - Whether the DB cluster is encrypted.
--
-- 'kmsKeyId', 'awsRdsDbClusterDetails_kmsKeyId' - The ARN of the KMS master key that is used to encrypt the database
-- instances in the DB cluster.
--
-- 'engine', 'awsRdsDbClusterDetails_engine' - The name of the database engine to use for this DB cluster. Valid values
-- are as follows:
--
-- -   @aurora@
--
-- -   @aurora-mysql@
--
-- -   @aurora-postgresql@
--
-- 'allocatedStorage', 'awsRdsDbClusterDetails_allocatedStorage' - For all database engines except Aurora, specifies the allocated storage
-- size in gibibytes (GiB).
--
-- 'readerEndpoint', 'awsRdsDbClusterDetails_readerEndpoint' - The reader endpoint for the DB cluster.
--
-- 'deletionProtection', 'awsRdsDbClusterDetails_deletionProtection' - Whether the DB cluster has deletion protection enabled.
--
-- 'iamDatabaseAuthenticationEnabled', 'awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled' - Whether the mapping of IAM accounts to database accounts is enabled.
--
-- 'preferredMaintenanceWindow', 'awsRdsDbClusterDetails_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Uses the format @\<day>:HH:MM-\<day>:HH:MM@.
--
-- For the day values, use @mon@|@tue@|@wed@|@thu@|@fri@|@sat@|@sun@.
--
-- For example, @sun:09:32-sun:10:02@.
--
-- 'endpoint', 'awsRdsDbClusterDetails_endpoint' - The connection endpoint for the primary instance of the DB cluster.
--
-- 'clusterCreateTime', 'awsRdsDbClusterDetails_clusterCreateTime' - Indicates when the DB cluster was created, in Universal Coordinated Time
-- (UTC).
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'readReplicaIdentifiers', 'awsRdsDbClusterDetails_readReplicaIdentifiers' - The identifiers of the read replicas that are associated with this DB
-- cluster.
--
-- 'enabledCloudWatchLogsExports', 'awsRdsDbClusterDetails_enabledCloudWatchLogsExports' - A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- 'dbClusterResourceId', 'awsRdsDbClusterDetails_dbClusterResourceId' - The identifier of the DB cluster. The identifier must be unique within
-- each Amazon Web Services Region and is immutable.
--
-- 'associatedRoles', 'awsRdsDbClusterDetails_associatedRoles' - A list of the IAM roles that are associated with the DB cluster.
--
-- 'multiAz', 'awsRdsDbClusterDetails_multiAz' - Whether the DB cluster has instances in multiple Availability Zones.
--
-- 'engineVersion', 'awsRdsDbClusterDetails_engineVersion' - The version number of the database engine to use.
--
-- 'vpcSecurityGroups', 'awsRdsDbClusterDetails_vpcSecurityGroups' - A list of VPC security groups that the DB cluster belongs to.
newAwsRdsDbClusterDetails ::
  AwsRdsDbClusterDetails
newAwsRdsDbClusterDetails =
  AwsRdsDbClusterDetails'
    { port = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      activityStreamStatus = Prelude.Nothing,
      dbClusterParameterGroup = Prelude.Nothing,
      dbClusterMembers = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      crossAccountClone = Prelude.Nothing,
      dbClusterOptionGroupMemberships = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      status = Prelude.Nothing,
      httpEndpointEnabled = Prelude.Nothing,
      customEndpoints = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      engine = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      readerEndpoint = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      iamDatabaseAuthenticationEnabled = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      readReplicaIdentifiers = Prelude.Nothing,
      enabledCloudWatchLogsExports = Prelude.Nothing,
      dbClusterResourceId = Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      multiAz = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | The port number on which the DB instances in the DB cluster accept
-- connections.
awsRdsDbClusterDetails_port :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterDetails_port = Lens.lens (\AwsRdsDbClusterDetails' {port} -> port) (\s@AwsRdsDbClusterDetails' {} a -> s {port = a} :: AwsRdsDbClusterDetails)

-- | Specifies the identifier that Amazon Route 53 assigns when you create a
-- hosted zone.
awsRdsDbClusterDetails_hostedZoneId :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_hostedZoneId = Lens.lens (\AwsRdsDbClusterDetails' {hostedZoneId} -> hostedZoneId) (\s@AwsRdsDbClusterDetails' {} a -> s {hostedZoneId = a} :: AwsRdsDbClusterDetails)

-- | The range of time each day when automated backups are created, if
-- automated backups are enabled.
--
-- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
awsRdsDbClusterDetails_preferredBackupWindow :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_preferredBackupWindow = Lens.lens (\AwsRdsDbClusterDetails' {preferredBackupWindow} -> preferredBackupWindow) (\s@AwsRdsDbClusterDetails' {} a -> s {preferredBackupWindow = a} :: AwsRdsDbClusterDetails)

-- | The number of days for which automated backups are retained.
awsRdsDbClusterDetails_backupRetentionPeriod :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterDetails_backupRetentionPeriod = Lens.lens (\AwsRdsDbClusterDetails' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@AwsRdsDbClusterDetails' {} a -> s {backupRetentionPeriod = a} :: AwsRdsDbClusterDetails)

-- | The name of the master user for the DB cluster.
awsRdsDbClusterDetails_masterUsername :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_masterUsername = Lens.lens (\AwsRdsDbClusterDetails' {masterUsername} -> masterUsername) (\s@AwsRdsDbClusterDetails' {} a -> s {masterUsername = a} :: AwsRdsDbClusterDetails)

-- | Whether tags are copied from the DB cluster to snapshots of the DB
-- cluster.
awsRdsDbClusterDetails_copyTagsToSnapshot :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_copyTagsToSnapshot = Lens.lens (\AwsRdsDbClusterDetails' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@AwsRdsDbClusterDetails' {} a -> s {copyTagsToSnapshot = a} :: AwsRdsDbClusterDetails)

-- | The status of the database activity stream. Valid values are as follows:
--
-- -   @started@
--
-- -   @starting@
--
-- -   @stopped@
--
-- -   @stopping@
awsRdsDbClusterDetails_activityStreamStatus :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_activityStreamStatus = Lens.lens (\AwsRdsDbClusterDetails' {activityStreamStatus} -> activityStreamStatus) (\s@AwsRdsDbClusterDetails' {} a -> s {activityStreamStatus = a} :: AwsRdsDbClusterDetails)

-- | The name of the DB cluster parameter group for the DB cluster.
awsRdsDbClusterDetails_dbClusterParameterGroup :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbClusterParameterGroup = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterParameterGroup = a} :: AwsRdsDbClusterDetails)

-- | The list of instances that make up the DB cluster.
awsRdsDbClusterDetails_dbClusterMembers :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbClusterMember])
awsRdsDbClusterDetails_dbClusterMembers = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterMembers} -> dbClusterMembers) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterMembers = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the database.
awsRdsDbClusterDetails_databaseName :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_databaseName = Lens.lens (\AwsRdsDbClusterDetails' {databaseName} -> databaseName) (\s@AwsRdsDbClusterDetails' {} a -> s {databaseName = a} :: AwsRdsDbClusterDetails)

-- | The DB cluster identifier that the user assigned to the cluster. This
-- identifier is the unique key that identifies a DB cluster.
awsRdsDbClusterDetails_dbClusterIdentifier :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbClusterIdentifier = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterIdentifier = a} :: AwsRdsDbClusterDetails)

-- | The Active Directory domain membership records that are associated with
-- the DB cluster.
awsRdsDbClusterDetails_domainMemberships :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbDomainMembership])
awsRdsDbClusterDetails_domainMemberships = Lens.lens (\AwsRdsDbClusterDetails' {domainMemberships} -> domainMemberships) (\s@AwsRdsDbClusterDetails' {} a -> s {domainMemberships = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of Availability Zones (AZs) where instances in the DB cluster can
-- be created.
awsRdsDbClusterDetails_availabilityZones :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_availabilityZones = Lens.lens (\AwsRdsDbClusterDetails' {availabilityZones} -> availabilityZones) (\s@AwsRdsDbClusterDetails' {} a -> s {availabilityZones = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether the DB cluster is a clone of a DB cluster owned by a different
-- Amazon Web Services account.
awsRdsDbClusterDetails_crossAccountClone :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_crossAccountClone = Lens.lens (\AwsRdsDbClusterDetails' {crossAccountClone} -> crossAccountClone) (\s@AwsRdsDbClusterDetails' {} a -> s {crossAccountClone = a} :: AwsRdsDbClusterDetails)

-- | The list of option group memberships for this DB cluster.
awsRdsDbClusterDetails_dbClusterOptionGroupMemberships :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbClusterOptionGroupMembership])
awsRdsDbClusterDetails_dbClusterOptionGroupMemberships = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterOptionGroupMemberships} -> dbClusterOptionGroupMemberships) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterOptionGroupMemberships = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The subnet group that is associated with the DB cluster, including the
-- name, description, and subnets in the subnet group.
awsRdsDbClusterDetails_dbSubnetGroup :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbSubnetGroup = Lens.lens (\AwsRdsDbClusterDetails' {dbSubnetGroup} -> dbSubnetGroup) (\s@AwsRdsDbClusterDetails' {} a -> s {dbSubnetGroup = a} :: AwsRdsDbClusterDetails)

-- | The current status of this DB cluster.
awsRdsDbClusterDetails_status :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_status = Lens.lens (\AwsRdsDbClusterDetails' {status} -> status) (\s@AwsRdsDbClusterDetails' {} a -> s {status = a} :: AwsRdsDbClusterDetails)

-- | Whether the HTTP endpoint for an Aurora Serverless DB cluster is
-- enabled.
awsRdsDbClusterDetails_httpEndpointEnabled :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_httpEndpointEnabled = Lens.lens (\AwsRdsDbClusterDetails' {httpEndpointEnabled} -> httpEndpointEnabled) (\s@AwsRdsDbClusterDetails' {} a -> s {httpEndpointEnabled = a} :: AwsRdsDbClusterDetails)

-- | A list of custom endpoints for the DB cluster.
awsRdsDbClusterDetails_customEndpoints :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_customEndpoints = Lens.lens (\AwsRdsDbClusterDetails' {customEndpoints} -> customEndpoints) (\s@AwsRdsDbClusterDetails' {} a -> s {customEndpoints = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The database engine mode of the DB cluster.Valid values are as follows:
--
-- -   @global@
--
-- -   @multimaster@
--
-- -   @parallelquery@
--
-- -   @provisioned@
--
-- -   @serverless@
awsRdsDbClusterDetails_engineMode :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_engineMode = Lens.lens (\AwsRdsDbClusterDetails' {engineMode} -> engineMode) (\s@AwsRdsDbClusterDetails' {} a -> s {engineMode = a} :: AwsRdsDbClusterDetails)

-- | Whether the DB cluster is encrypted.
awsRdsDbClusterDetails_storageEncrypted :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_storageEncrypted = Lens.lens (\AwsRdsDbClusterDetails' {storageEncrypted} -> storageEncrypted) (\s@AwsRdsDbClusterDetails' {} a -> s {storageEncrypted = a} :: AwsRdsDbClusterDetails)

-- | The ARN of the KMS master key that is used to encrypt the database
-- instances in the DB cluster.
awsRdsDbClusterDetails_kmsKeyId :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_kmsKeyId = Lens.lens (\AwsRdsDbClusterDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRdsDbClusterDetails' {} a -> s {kmsKeyId = a} :: AwsRdsDbClusterDetails)

-- | The name of the database engine to use for this DB cluster. Valid values
-- are as follows:
--
-- -   @aurora@
--
-- -   @aurora-mysql@
--
-- -   @aurora-postgresql@
awsRdsDbClusterDetails_engine :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_engine = Lens.lens (\AwsRdsDbClusterDetails' {engine} -> engine) (\s@AwsRdsDbClusterDetails' {} a -> s {engine = a} :: AwsRdsDbClusterDetails)

-- | For all database engines except Aurora, specifies the allocated storage
-- size in gibibytes (GiB).
awsRdsDbClusterDetails_allocatedStorage :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterDetails_allocatedStorage = Lens.lens (\AwsRdsDbClusterDetails' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbClusterDetails' {} a -> s {allocatedStorage = a} :: AwsRdsDbClusterDetails)

-- | The reader endpoint for the DB cluster.
awsRdsDbClusterDetails_readerEndpoint :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_readerEndpoint = Lens.lens (\AwsRdsDbClusterDetails' {readerEndpoint} -> readerEndpoint) (\s@AwsRdsDbClusterDetails' {} a -> s {readerEndpoint = a} :: AwsRdsDbClusterDetails)

-- | Whether the DB cluster has deletion protection enabled.
awsRdsDbClusterDetails_deletionProtection :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_deletionProtection = Lens.lens (\AwsRdsDbClusterDetails' {deletionProtection} -> deletionProtection) (\s@AwsRdsDbClusterDetails' {} a -> s {deletionProtection = a} :: AwsRdsDbClusterDetails)

-- | Whether the mapping of IAM accounts to database accounts is enabled.
awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled = Lens.lens (\AwsRdsDbClusterDetails' {iamDatabaseAuthenticationEnabled} -> iamDatabaseAuthenticationEnabled) (\s@AwsRdsDbClusterDetails' {} a -> s {iamDatabaseAuthenticationEnabled = a} :: AwsRdsDbClusterDetails)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Uses the format @\<day>:HH:MM-\<day>:HH:MM@.
--
-- For the day values, use @mon@|@tue@|@wed@|@thu@|@fri@|@sat@|@sun@.
--
-- For example, @sun:09:32-sun:10:02@.
awsRdsDbClusterDetails_preferredMaintenanceWindow :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_preferredMaintenanceWindow = Lens.lens (\AwsRdsDbClusterDetails' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@AwsRdsDbClusterDetails' {} a -> s {preferredMaintenanceWindow = a} :: AwsRdsDbClusterDetails)

-- | The connection endpoint for the primary instance of the DB cluster.
awsRdsDbClusterDetails_endpoint :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_endpoint = Lens.lens (\AwsRdsDbClusterDetails' {endpoint} -> endpoint) (\s@AwsRdsDbClusterDetails' {} a -> s {endpoint = a} :: AwsRdsDbClusterDetails)

-- | Indicates when the DB cluster was created, in Universal Coordinated Time
-- (UTC).
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRdsDbClusterDetails_clusterCreateTime :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_clusterCreateTime = Lens.lens (\AwsRdsDbClusterDetails' {clusterCreateTime} -> clusterCreateTime) (\s@AwsRdsDbClusterDetails' {} a -> s {clusterCreateTime = a} :: AwsRdsDbClusterDetails)

-- | The identifiers of the read replicas that are associated with this DB
-- cluster.
awsRdsDbClusterDetails_readReplicaIdentifiers :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_readReplicaIdentifiers = Lens.lens (\AwsRdsDbClusterDetails' {readReplicaIdentifiers} -> readReplicaIdentifiers) (\s@AwsRdsDbClusterDetails' {} a -> s {readReplicaIdentifiers = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
awsRdsDbClusterDetails_enabledCloudWatchLogsExports :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_enabledCloudWatchLogsExports = Lens.lens (\AwsRdsDbClusterDetails' {enabledCloudWatchLogsExports} -> enabledCloudWatchLogsExports) (\s@AwsRdsDbClusterDetails' {} a -> s {enabledCloudWatchLogsExports = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the DB cluster. The identifier must be unique within
-- each Amazon Web Services Region and is immutable.
awsRdsDbClusterDetails_dbClusterResourceId :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbClusterResourceId = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterResourceId} -> dbClusterResourceId) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterResourceId = a} :: AwsRdsDbClusterDetails)

-- | A list of the IAM roles that are associated with the DB cluster.
awsRdsDbClusterDetails_associatedRoles :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbClusterAssociatedRole])
awsRdsDbClusterDetails_associatedRoles = Lens.lens (\AwsRdsDbClusterDetails' {associatedRoles} -> associatedRoles) (\s@AwsRdsDbClusterDetails' {} a -> s {associatedRoles = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether the DB cluster has instances in multiple Availability Zones.
awsRdsDbClusterDetails_multiAz :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_multiAz = Lens.lens (\AwsRdsDbClusterDetails' {multiAz} -> multiAz) (\s@AwsRdsDbClusterDetails' {} a -> s {multiAz = a} :: AwsRdsDbClusterDetails)

-- | The version number of the database engine to use.
awsRdsDbClusterDetails_engineVersion :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_engineVersion = Lens.lens (\AwsRdsDbClusterDetails' {engineVersion} -> engineVersion) (\s@AwsRdsDbClusterDetails' {} a -> s {engineVersion = a} :: AwsRdsDbClusterDetails)

-- | A list of VPC security groups that the DB cluster belongs to.
awsRdsDbClusterDetails_vpcSecurityGroups :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbInstanceVpcSecurityGroup])
awsRdsDbClusterDetails_vpcSecurityGroups = Lens.lens (\AwsRdsDbClusterDetails' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@AwsRdsDbClusterDetails' {} a -> s {vpcSecurityGroups = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsRdsDbClusterDetails where
  parseJSON =
    Data.withObject
      "AwsRdsDbClusterDetails"
      ( \x ->
          AwsRdsDbClusterDetails'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "HostedZoneId")
            Prelude.<*> (x Data..:? "PreferredBackupWindow")
            Prelude.<*> (x Data..:? "BackupRetentionPeriod")
            Prelude.<*> (x Data..:? "MasterUsername")
            Prelude.<*> (x Data..:? "CopyTagsToSnapshot")
            Prelude.<*> (x Data..:? "ActivityStreamStatus")
            Prelude.<*> (x Data..:? "DbClusterParameterGroup")
            Prelude.<*> ( x Data..:? "DbClusterMembers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "DbClusterIdentifier")
            Prelude.<*> ( x Data..:? "DomainMemberships"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CrossAccountClone")
            Prelude.<*> ( x Data..:? "DbClusterOptionGroupMemberships"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DbSubnetGroup")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "HttpEndpointEnabled")
            Prelude.<*> ( x Data..:? "CustomEndpoints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EngineMode")
            Prelude.<*> (x Data..:? "StorageEncrypted")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "Engine")
            Prelude.<*> (x Data..:? "AllocatedStorage")
            Prelude.<*> (x Data..:? "ReaderEndpoint")
            Prelude.<*> (x Data..:? "DeletionProtection")
            Prelude.<*> (x Data..:? "IamDatabaseAuthenticationEnabled")
            Prelude.<*> (x Data..:? "PreferredMaintenanceWindow")
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "ClusterCreateTime")
            Prelude.<*> ( x Data..:? "ReadReplicaIdentifiers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "EnabledCloudWatchLogsExports"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DbClusterResourceId")
            Prelude.<*> ( x Data..:? "AssociatedRoles"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MultiAz")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> ( x Data..:? "VpcSecurityGroups"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsRdsDbClusterDetails where
  hashWithSalt _salt AwsRdsDbClusterDetails' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` activityStreamStatus
      `Prelude.hashWithSalt` dbClusterParameterGroup
      `Prelude.hashWithSalt` dbClusterMembers
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` domainMemberships
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` crossAccountClone
      `Prelude.hashWithSalt` dbClusterOptionGroupMemberships
      `Prelude.hashWithSalt` dbSubnetGroup
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` httpEndpointEnabled
      `Prelude.hashWithSalt` customEndpoints
      `Prelude.hashWithSalt` engineMode
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` readerEndpoint
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` iamDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` readReplicaIdentifiers
      `Prelude.hashWithSalt` enabledCloudWatchLogsExports
      `Prelude.hashWithSalt` dbClusterResourceId
      `Prelude.hashWithSalt` associatedRoles
      `Prelude.hashWithSalt` multiAz
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData AwsRdsDbClusterDetails where
  rnf AwsRdsDbClusterDetails' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf activityStreamStatus
      `Prelude.seq` Prelude.rnf dbClusterParameterGroup
      `Prelude.seq` Prelude.rnf dbClusterMembers
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf domainMemberships
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf crossAccountClone
      `Prelude.seq` Prelude.rnf
        dbClusterOptionGroupMemberships
      `Prelude.seq` Prelude.rnf dbSubnetGroup
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpEndpointEnabled
      `Prelude.seq` Prelude.rnf customEndpoints
      `Prelude.seq` Prelude.rnf engineMode
      `Prelude.seq` Prelude.rnf storageEncrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf
        allocatedStorage
      `Prelude.seq` Prelude.rnf
        readerEndpoint
      `Prelude.seq` Prelude.rnf
        deletionProtection
      `Prelude.seq` Prelude.rnf
        iamDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        endpoint
      `Prelude.seq` Prelude.rnf
        clusterCreateTime
      `Prelude.seq` Prelude.rnf
        readReplicaIdentifiers
      `Prelude.seq` Prelude.rnf
        enabledCloudWatchLogsExports
      `Prelude.seq` Prelude.rnf
        dbClusterResourceId
      `Prelude.seq` Prelude.rnf
        associatedRoles
      `Prelude.seq` Prelude.rnf
        multiAz
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups

instance Data.ToJSON AwsRdsDbClusterDetails where
  toJSON AwsRdsDbClusterDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Port" Data..=) Prelude.<$> port,
            ("HostedZoneId" Data..=) Prelude.<$> hostedZoneId,
            ("PreferredBackupWindow" Data..=)
              Prelude.<$> preferredBackupWindow,
            ("BackupRetentionPeriod" Data..=)
              Prelude.<$> backupRetentionPeriod,
            ("MasterUsername" Data..=)
              Prelude.<$> masterUsername,
            ("CopyTagsToSnapshot" Data..=)
              Prelude.<$> copyTagsToSnapshot,
            ("ActivityStreamStatus" Data..=)
              Prelude.<$> activityStreamStatus,
            ("DbClusterParameterGroup" Data..=)
              Prelude.<$> dbClusterParameterGroup,
            ("DbClusterMembers" Data..=)
              Prelude.<$> dbClusterMembers,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("DbClusterIdentifier" Data..=)
              Prelude.<$> dbClusterIdentifier,
            ("DomainMemberships" Data..=)
              Prelude.<$> domainMemberships,
            ("AvailabilityZones" Data..=)
              Prelude.<$> availabilityZones,
            ("CrossAccountClone" Data..=)
              Prelude.<$> crossAccountClone,
            ("DbClusterOptionGroupMemberships" Data..=)
              Prelude.<$> dbClusterOptionGroupMemberships,
            ("DbSubnetGroup" Data..=) Prelude.<$> dbSubnetGroup,
            ("Status" Data..=) Prelude.<$> status,
            ("HttpEndpointEnabled" Data..=)
              Prelude.<$> httpEndpointEnabled,
            ("CustomEndpoints" Data..=)
              Prelude.<$> customEndpoints,
            ("EngineMode" Data..=) Prelude.<$> engineMode,
            ("StorageEncrypted" Data..=)
              Prelude.<$> storageEncrypted,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Engine" Data..=) Prelude.<$> engine,
            ("AllocatedStorage" Data..=)
              Prelude.<$> allocatedStorage,
            ("ReaderEndpoint" Data..=)
              Prelude.<$> readerEndpoint,
            ("DeletionProtection" Data..=)
              Prelude.<$> deletionProtection,
            ("IamDatabaseAuthenticationEnabled" Data..=)
              Prelude.<$> iamDatabaseAuthenticationEnabled,
            ("PreferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("Endpoint" Data..=) Prelude.<$> endpoint,
            ("ClusterCreateTime" Data..=)
              Prelude.<$> clusterCreateTime,
            ("ReadReplicaIdentifiers" Data..=)
              Prelude.<$> readReplicaIdentifiers,
            ("EnabledCloudWatchLogsExports" Data..=)
              Prelude.<$> enabledCloudWatchLogsExports,
            ("DbClusterResourceId" Data..=)
              Prelude.<$> dbClusterResourceId,
            ("AssociatedRoles" Data..=)
              Prelude.<$> associatedRoles,
            ("MultiAz" Data..=) Prelude.<$> multiAz,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            ("VpcSecurityGroups" Data..=)
              Prelude.<$> vpcSecurityGroups
          ]
      )
