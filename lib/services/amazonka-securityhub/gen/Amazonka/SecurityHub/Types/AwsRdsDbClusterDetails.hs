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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | The version number of the database engine to use.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The current status of this DB cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | The list of instances that make up the DB cluster.
    dbClusterMembers :: Prelude.Maybe [AwsRdsDbClusterMember],
    -- | Whether the DB cluster has deletion protection enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The DB cluster identifier that the user assigned to the cluster. This
    -- identifier is the unique key that identifies a DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the read replicas that are associated with this DB
    -- cluster.
    readReplicaIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DB cluster parameter group for the DB cluster.
    dbClusterParameterGroup :: Prelude.Maybe Prelude.Text,
    -- | Specifies the identifier that Amazon Route 53 assigns when you create a
    -- hosted zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | The name of the master user for the DB cluster.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | Whether the mapping of IAM accounts to database accounts is enabled.
    iamDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the DB cluster. The identifier must be unique within
    -- each Amazon Web Services Region and is immutable.
    dbClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | A list of custom endpoints for the DB cluster.
    customEndpoints :: Prelude.Maybe [Prelude.Text],
    -- | The name of the database engine to use for this DB cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Whether the HTTP endpoint for an Aurora Serverless DB cluster is
    -- enabled.
    httpEndpointEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the DB cluster is a clone of a DB cluster owned by a different
    -- Amazon Web Services account.
    crossAccountClone :: Prelude.Maybe Prelude.Bool,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Uses the format @\<day>:HH:MM-\<day>:HH:MM@.
    --
    -- For the day values, use @mon@|@tue@|@wed@|@thu@|@fri@|@sat@|@sun@.
    --
    -- For example, @sun:09:32-sun:10:02@.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones (AZs) where instances in the DB cluster can
    -- be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the KMS master key that is used to encrypt the database
    -- instances in the DB cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The range of time each day when automated backups are created, if
    -- automated backups are enabled.
    --
    -- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | A list of the IAM roles that are associated with the DB cluster.
    associatedRoles :: Prelude.Maybe [AwsRdsDbClusterAssociatedRole],
    -- | A list of VPC security groups that the DB cluster belongs to.
    vpcSecurityGroups :: Prelude.Maybe [AwsRdsDbInstanceVpcSecurityGroup],
    -- | The number of days for which automated backups are retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The subnet group that is associated with the DB cluster, including the
    -- name, description, and subnets in the subnet group.
    dbSubnetGroup :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB cluster has instances in multiple Availability Zones.
    multiAz :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The database engine mode of the DB cluster.
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | A list of log types that this DB cluster is configured to export to
    -- CloudWatch Logs.
    enabledCloudWatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The status of the database activity stream.
    activityStreamStatus :: Prelude.Maybe Prelude.Text,
    -- | For all database engines except Aurora, specifies the allocated storage
    -- size in gibibytes (GiB).
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Whether tags are copied from the DB cluster to snapshots of the DB
    -- cluster.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the DB cluster was created, in Universal Coordinated Time
    -- (UTC).
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    clusterCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The connection endpoint for the primary instance of the DB cluster.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The reader endpoint for the DB cluster.
    readerEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The list of option group memberships for this DB cluster.
    dbClusterOptionGroupMemberships :: Prelude.Maybe [AwsRdsDbClusterOptionGroupMembership],
    -- | The port number on which the DB instances in the DB cluster accept
    -- connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | The Active Directory domain membership records that are associated with
    -- the DB cluster.
    domainMemberships :: Prelude.Maybe [AwsRdsDbDomainMembership]
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
-- 'engineVersion', 'awsRdsDbClusterDetails_engineVersion' - The version number of the database engine to use.
--
-- 'status', 'awsRdsDbClusterDetails_status' - The current status of this DB cluster.
--
-- 'dbClusterMembers', 'awsRdsDbClusterDetails_dbClusterMembers' - The list of instances that make up the DB cluster.
--
-- 'deletionProtection', 'awsRdsDbClusterDetails_deletionProtection' - Whether the DB cluster has deletion protection enabled.
--
-- 'storageEncrypted', 'awsRdsDbClusterDetails_storageEncrypted' - Whether the DB cluster is encrypted.
--
-- 'dbClusterIdentifier', 'awsRdsDbClusterDetails_dbClusterIdentifier' - The DB cluster identifier that the user assigned to the cluster. This
-- identifier is the unique key that identifies a DB cluster.
--
-- 'readReplicaIdentifiers', 'awsRdsDbClusterDetails_readReplicaIdentifiers' - The identifiers of the read replicas that are associated with this DB
-- cluster.
--
-- 'dbClusterParameterGroup', 'awsRdsDbClusterDetails_dbClusterParameterGroup' - The name of the DB cluster parameter group for the DB cluster.
--
-- 'hostedZoneId', 'awsRdsDbClusterDetails_hostedZoneId' - Specifies the identifier that Amazon Route 53 assigns when you create a
-- hosted zone.
--
-- 'masterUsername', 'awsRdsDbClusterDetails_masterUsername' - The name of the master user for the DB cluster.
--
-- 'iamDatabaseAuthenticationEnabled', 'awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled' - Whether the mapping of IAM accounts to database accounts is enabled.
--
-- 'dbClusterResourceId', 'awsRdsDbClusterDetails_dbClusterResourceId' - The identifier of the DB cluster. The identifier must be unique within
-- each Amazon Web Services Region and is immutable.
--
-- 'customEndpoints', 'awsRdsDbClusterDetails_customEndpoints' - A list of custom endpoints for the DB cluster.
--
-- 'engine', 'awsRdsDbClusterDetails_engine' - The name of the database engine to use for this DB cluster.
--
-- 'httpEndpointEnabled', 'awsRdsDbClusterDetails_httpEndpointEnabled' - Whether the HTTP endpoint for an Aurora Serverless DB cluster is
-- enabled.
--
-- 'crossAccountClone', 'awsRdsDbClusterDetails_crossAccountClone' - Whether the DB cluster is a clone of a DB cluster owned by a different
-- Amazon Web Services account.
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
-- 'availabilityZones', 'awsRdsDbClusterDetails_availabilityZones' - A list of Availability Zones (AZs) where instances in the DB cluster can
-- be created.
--
-- 'kmsKeyId', 'awsRdsDbClusterDetails_kmsKeyId' - The ARN of the KMS master key that is used to encrypt the database
-- instances in the DB cluster.
--
-- 'preferredBackupWindow', 'awsRdsDbClusterDetails_preferredBackupWindow' - The range of time each day when automated backups are created, if
-- automated backups are enabled.
--
-- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
--
-- 'associatedRoles', 'awsRdsDbClusterDetails_associatedRoles' - A list of the IAM roles that are associated with the DB cluster.
--
-- 'vpcSecurityGroups', 'awsRdsDbClusterDetails_vpcSecurityGroups' - A list of VPC security groups that the DB cluster belongs to.
--
-- 'backupRetentionPeriod', 'awsRdsDbClusterDetails_backupRetentionPeriod' - The number of days for which automated backups are retained.
--
-- 'dbSubnetGroup', 'awsRdsDbClusterDetails_dbSubnetGroup' - The subnet group that is associated with the DB cluster, including the
-- name, description, and subnets in the subnet group.
--
-- 'multiAz', 'awsRdsDbClusterDetails_multiAz' - Whether the DB cluster has instances in multiple Availability Zones.
--
-- 'databaseName', 'awsRdsDbClusterDetails_databaseName' - The name of the database.
--
-- 'engineMode', 'awsRdsDbClusterDetails_engineMode' - The database engine mode of the DB cluster.
--
-- 'enabledCloudWatchLogsExports', 'awsRdsDbClusterDetails_enabledCloudWatchLogsExports' - A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
--
-- 'activityStreamStatus', 'awsRdsDbClusterDetails_activityStreamStatus' - The status of the database activity stream.
--
-- 'allocatedStorage', 'awsRdsDbClusterDetails_allocatedStorage' - For all database engines except Aurora, specifies the allocated storage
-- size in gibibytes (GiB).
--
-- 'copyTagsToSnapshot', 'awsRdsDbClusterDetails_copyTagsToSnapshot' - Whether tags are copied from the DB cluster to snapshots of the DB
-- cluster.
--
-- 'clusterCreateTime', 'awsRdsDbClusterDetails_clusterCreateTime' - Indicates when the DB cluster was created, in Universal Coordinated Time
-- (UTC).
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'endpoint', 'awsRdsDbClusterDetails_endpoint' - The connection endpoint for the primary instance of the DB cluster.
--
-- 'readerEndpoint', 'awsRdsDbClusterDetails_readerEndpoint' - The reader endpoint for the DB cluster.
--
-- 'dbClusterOptionGroupMemberships', 'awsRdsDbClusterDetails_dbClusterOptionGroupMemberships' - The list of option group memberships for this DB cluster.
--
-- 'port', 'awsRdsDbClusterDetails_port' - The port number on which the DB instances in the DB cluster accept
-- connections.
--
-- 'domainMemberships', 'awsRdsDbClusterDetails_domainMemberships' - The Active Directory domain membership records that are associated with
-- the DB cluster.
newAwsRdsDbClusterDetails ::
  AwsRdsDbClusterDetails
newAwsRdsDbClusterDetails =
  AwsRdsDbClusterDetails'
    { engineVersion =
        Prelude.Nothing,
      status = Prelude.Nothing,
      dbClusterMembers = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      readReplicaIdentifiers = Prelude.Nothing,
      dbClusterParameterGroup = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      iamDatabaseAuthenticationEnabled = Prelude.Nothing,
      dbClusterResourceId = Prelude.Nothing,
      customEndpoints = Prelude.Nothing,
      engine = Prelude.Nothing,
      httpEndpointEnabled = Prelude.Nothing,
      crossAccountClone = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      multiAz = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      enabledCloudWatchLogsExports = Prelude.Nothing,
      activityStreamStatus = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      readerEndpoint = Prelude.Nothing,
      dbClusterOptionGroupMemberships = Prelude.Nothing,
      port = Prelude.Nothing,
      domainMemberships = Prelude.Nothing
    }

-- | The version number of the database engine to use.
awsRdsDbClusterDetails_engineVersion :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_engineVersion = Lens.lens (\AwsRdsDbClusterDetails' {engineVersion} -> engineVersion) (\s@AwsRdsDbClusterDetails' {} a -> s {engineVersion = a} :: AwsRdsDbClusterDetails)

-- | The current status of this DB cluster.
awsRdsDbClusterDetails_status :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_status = Lens.lens (\AwsRdsDbClusterDetails' {status} -> status) (\s@AwsRdsDbClusterDetails' {} a -> s {status = a} :: AwsRdsDbClusterDetails)

-- | The list of instances that make up the DB cluster.
awsRdsDbClusterDetails_dbClusterMembers :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbClusterMember])
awsRdsDbClusterDetails_dbClusterMembers = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterMembers} -> dbClusterMembers) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterMembers = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether the DB cluster has deletion protection enabled.
awsRdsDbClusterDetails_deletionProtection :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_deletionProtection = Lens.lens (\AwsRdsDbClusterDetails' {deletionProtection} -> deletionProtection) (\s@AwsRdsDbClusterDetails' {} a -> s {deletionProtection = a} :: AwsRdsDbClusterDetails)

-- | Whether the DB cluster is encrypted.
awsRdsDbClusterDetails_storageEncrypted :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_storageEncrypted = Lens.lens (\AwsRdsDbClusterDetails' {storageEncrypted} -> storageEncrypted) (\s@AwsRdsDbClusterDetails' {} a -> s {storageEncrypted = a} :: AwsRdsDbClusterDetails)

-- | The DB cluster identifier that the user assigned to the cluster. This
-- identifier is the unique key that identifies a DB cluster.
awsRdsDbClusterDetails_dbClusterIdentifier :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbClusterIdentifier = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterIdentifier = a} :: AwsRdsDbClusterDetails)

-- | The identifiers of the read replicas that are associated with this DB
-- cluster.
awsRdsDbClusterDetails_readReplicaIdentifiers :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_readReplicaIdentifiers = Lens.lens (\AwsRdsDbClusterDetails' {readReplicaIdentifiers} -> readReplicaIdentifiers) (\s@AwsRdsDbClusterDetails' {} a -> s {readReplicaIdentifiers = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB cluster parameter group for the DB cluster.
awsRdsDbClusterDetails_dbClusterParameterGroup :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbClusterParameterGroup = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterParameterGroup = a} :: AwsRdsDbClusterDetails)

-- | Specifies the identifier that Amazon Route 53 assigns when you create a
-- hosted zone.
awsRdsDbClusterDetails_hostedZoneId :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_hostedZoneId = Lens.lens (\AwsRdsDbClusterDetails' {hostedZoneId} -> hostedZoneId) (\s@AwsRdsDbClusterDetails' {} a -> s {hostedZoneId = a} :: AwsRdsDbClusterDetails)

-- | The name of the master user for the DB cluster.
awsRdsDbClusterDetails_masterUsername :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_masterUsername = Lens.lens (\AwsRdsDbClusterDetails' {masterUsername} -> masterUsername) (\s@AwsRdsDbClusterDetails' {} a -> s {masterUsername = a} :: AwsRdsDbClusterDetails)

-- | Whether the mapping of IAM accounts to database accounts is enabled.
awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_iamDatabaseAuthenticationEnabled = Lens.lens (\AwsRdsDbClusterDetails' {iamDatabaseAuthenticationEnabled} -> iamDatabaseAuthenticationEnabled) (\s@AwsRdsDbClusterDetails' {} a -> s {iamDatabaseAuthenticationEnabled = a} :: AwsRdsDbClusterDetails)

-- | The identifier of the DB cluster. The identifier must be unique within
-- each Amazon Web Services Region and is immutable.
awsRdsDbClusterDetails_dbClusterResourceId :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbClusterResourceId = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterResourceId} -> dbClusterResourceId) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterResourceId = a} :: AwsRdsDbClusterDetails)

-- | A list of custom endpoints for the DB cluster.
awsRdsDbClusterDetails_customEndpoints :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_customEndpoints = Lens.lens (\AwsRdsDbClusterDetails' {customEndpoints} -> customEndpoints) (\s@AwsRdsDbClusterDetails' {} a -> s {customEndpoints = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the database engine to use for this DB cluster.
awsRdsDbClusterDetails_engine :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_engine = Lens.lens (\AwsRdsDbClusterDetails' {engine} -> engine) (\s@AwsRdsDbClusterDetails' {} a -> s {engine = a} :: AwsRdsDbClusterDetails)

-- | Whether the HTTP endpoint for an Aurora Serverless DB cluster is
-- enabled.
awsRdsDbClusterDetails_httpEndpointEnabled :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_httpEndpointEnabled = Lens.lens (\AwsRdsDbClusterDetails' {httpEndpointEnabled} -> httpEndpointEnabled) (\s@AwsRdsDbClusterDetails' {} a -> s {httpEndpointEnabled = a} :: AwsRdsDbClusterDetails)

-- | Whether the DB cluster is a clone of a DB cluster owned by a different
-- Amazon Web Services account.
awsRdsDbClusterDetails_crossAccountClone :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_crossAccountClone = Lens.lens (\AwsRdsDbClusterDetails' {crossAccountClone} -> crossAccountClone) (\s@AwsRdsDbClusterDetails' {} a -> s {crossAccountClone = a} :: AwsRdsDbClusterDetails)

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

-- | A list of Availability Zones (AZs) where instances in the DB cluster can
-- be created.
awsRdsDbClusterDetails_availabilityZones :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_availabilityZones = Lens.lens (\AwsRdsDbClusterDetails' {availabilityZones} -> availabilityZones) (\s@AwsRdsDbClusterDetails' {} a -> s {availabilityZones = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the KMS master key that is used to encrypt the database
-- instances in the DB cluster.
awsRdsDbClusterDetails_kmsKeyId :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_kmsKeyId = Lens.lens (\AwsRdsDbClusterDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRdsDbClusterDetails' {} a -> s {kmsKeyId = a} :: AwsRdsDbClusterDetails)

-- | The range of time each day when automated backups are created, if
-- automated backups are enabled.
--
-- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
awsRdsDbClusterDetails_preferredBackupWindow :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_preferredBackupWindow = Lens.lens (\AwsRdsDbClusterDetails' {preferredBackupWindow} -> preferredBackupWindow) (\s@AwsRdsDbClusterDetails' {} a -> s {preferredBackupWindow = a} :: AwsRdsDbClusterDetails)

-- | A list of the IAM roles that are associated with the DB cluster.
awsRdsDbClusterDetails_associatedRoles :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbClusterAssociatedRole])
awsRdsDbClusterDetails_associatedRoles = Lens.lens (\AwsRdsDbClusterDetails' {associatedRoles} -> associatedRoles) (\s@AwsRdsDbClusterDetails' {} a -> s {associatedRoles = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of VPC security groups that the DB cluster belongs to.
awsRdsDbClusterDetails_vpcSecurityGroups :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbInstanceVpcSecurityGroup])
awsRdsDbClusterDetails_vpcSecurityGroups = Lens.lens (\AwsRdsDbClusterDetails' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@AwsRdsDbClusterDetails' {} a -> s {vpcSecurityGroups = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which automated backups are retained.
awsRdsDbClusterDetails_backupRetentionPeriod :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterDetails_backupRetentionPeriod = Lens.lens (\AwsRdsDbClusterDetails' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@AwsRdsDbClusterDetails' {} a -> s {backupRetentionPeriod = a} :: AwsRdsDbClusterDetails)

-- | The subnet group that is associated with the DB cluster, including the
-- name, description, and subnets in the subnet group.
awsRdsDbClusterDetails_dbSubnetGroup :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_dbSubnetGroup = Lens.lens (\AwsRdsDbClusterDetails' {dbSubnetGroup} -> dbSubnetGroup) (\s@AwsRdsDbClusterDetails' {} a -> s {dbSubnetGroup = a} :: AwsRdsDbClusterDetails)

-- | Whether the DB cluster has instances in multiple Availability Zones.
awsRdsDbClusterDetails_multiAz :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_multiAz = Lens.lens (\AwsRdsDbClusterDetails' {multiAz} -> multiAz) (\s@AwsRdsDbClusterDetails' {} a -> s {multiAz = a} :: AwsRdsDbClusterDetails)

-- | The name of the database.
awsRdsDbClusterDetails_databaseName :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_databaseName = Lens.lens (\AwsRdsDbClusterDetails' {databaseName} -> databaseName) (\s@AwsRdsDbClusterDetails' {} a -> s {databaseName = a} :: AwsRdsDbClusterDetails)

-- | The database engine mode of the DB cluster.
awsRdsDbClusterDetails_engineMode :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_engineMode = Lens.lens (\AwsRdsDbClusterDetails' {engineMode} -> engineMode) (\s@AwsRdsDbClusterDetails' {} a -> s {engineMode = a} :: AwsRdsDbClusterDetails)

-- | A list of log types that this DB cluster is configured to export to
-- CloudWatch Logs.
awsRdsDbClusterDetails_enabledCloudWatchLogsExports :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbClusterDetails_enabledCloudWatchLogsExports = Lens.lens (\AwsRdsDbClusterDetails' {enabledCloudWatchLogsExports} -> enabledCloudWatchLogsExports) (\s@AwsRdsDbClusterDetails' {} a -> s {enabledCloudWatchLogsExports = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The status of the database activity stream.
awsRdsDbClusterDetails_activityStreamStatus :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_activityStreamStatus = Lens.lens (\AwsRdsDbClusterDetails' {activityStreamStatus} -> activityStreamStatus) (\s@AwsRdsDbClusterDetails' {} a -> s {activityStreamStatus = a} :: AwsRdsDbClusterDetails)

-- | For all database engines except Aurora, specifies the allocated storage
-- size in gibibytes (GiB).
awsRdsDbClusterDetails_allocatedStorage :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterDetails_allocatedStorage = Lens.lens (\AwsRdsDbClusterDetails' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbClusterDetails' {} a -> s {allocatedStorage = a} :: AwsRdsDbClusterDetails)

-- | Whether tags are copied from the DB cluster to snapshots of the DB
-- cluster.
awsRdsDbClusterDetails_copyTagsToSnapshot :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbClusterDetails_copyTagsToSnapshot = Lens.lens (\AwsRdsDbClusterDetails' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@AwsRdsDbClusterDetails' {} a -> s {copyTagsToSnapshot = a} :: AwsRdsDbClusterDetails)

-- | Indicates when the DB cluster was created, in Universal Coordinated Time
-- (UTC).
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRdsDbClusterDetails_clusterCreateTime :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_clusterCreateTime = Lens.lens (\AwsRdsDbClusterDetails' {clusterCreateTime} -> clusterCreateTime) (\s@AwsRdsDbClusterDetails' {} a -> s {clusterCreateTime = a} :: AwsRdsDbClusterDetails)

-- | The connection endpoint for the primary instance of the DB cluster.
awsRdsDbClusterDetails_endpoint :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_endpoint = Lens.lens (\AwsRdsDbClusterDetails' {endpoint} -> endpoint) (\s@AwsRdsDbClusterDetails' {} a -> s {endpoint = a} :: AwsRdsDbClusterDetails)

-- | The reader endpoint for the DB cluster.
awsRdsDbClusterDetails_readerEndpoint :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Text)
awsRdsDbClusterDetails_readerEndpoint = Lens.lens (\AwsRdsDbClusterDetails' {readerEndpoint} -> readerEndpoint) (\s@AwsRdsDbClusterDetails' {} a -> s {readerEndpoint = a} :: AwsRdsDbClusterDetails)

-- | The list of option group memberships for this DB cluster.
awsRdsDbClusterDetails_dbClusterOptionGroupMemberships :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbClusterOptionGroupMembership])
awsRdsDbClusterDetails_dbClusterOptionGroupMemberships = Lens.lens (\AwsRdsDbClusterDetails' {dbClusterOptionGroupMemberships} -> dbClusterOptionGroupMemberships) (\s@AwsRdsDbClusterDetails' {} a -> s {dbClusterOptionGroupMemberships = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the DB instances in the DB cluster accept
-- connections.
awsRdsDbClusterDetails_port :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe Prelude.Int)
awsRdsDbClusterDetails_port = Lens.lens (\AwsRdsDbClusterDetails' {port} -> port) (\s@AwsRdsDbClusterDetails' {} a -> s {port = a} :: AwsRdsDbClusterDetails)

-- | The Active Directory domain membership records that are associated with
-- the DB cluster.
awsRdsDbClusterDetails_domainMemberships :: Lens.Lens' AwsRdsDbClusterDetails (Prelude.Maybe [AwsRdsDbDomainMembership])
awsRdsDbClusterDetails_domainMemberships = Lens.lens (\AwsRdsDbClusterDetails' {domainMemberships} -> domainMemberships) (\s@AwsRdsDbClusterDetails' {} a -> s {domainMemberships = a} :: AwsRdsDbClusterDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsRdsDbClusterDetails where
  parseJSON =
    Core.withObject
      "AwsRdsDbClusterDetails"
      ( \x ->
          AwsRdsDbClusterDetails'
            Prelude.<$> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> ( x Core..:? "DbClusterMembers"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DeletionProtection")
            Prelude.<*> (x Core..:? "StorageEncrypted")
            Prelude.<*> (x Core..:? "DbClusterIdentifier")
            Prelude.<*> ( x Core..:? "ReadReplicaIdentifiers"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DbClusterParameterGroup")
            Prelude.<*> (x Core..:? "HostedZoneId")
            Prelude.<*> (x Core..:? "MasterUsername")
            Prelude.<*> (x Core..:? "IamDatabaseAuthenticationEnabled")
            Prelude.<*> (x Core..:? "DbClusterResourceId")
            Prelude.<*> ( x Core..:? "CustomEndpoints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Engine")
            Prelude.<*> (x Core..:? "HttpEndpointEnabled")
            Prelude.<*> (x Core..:? "CrossAccountClone")
            Prelude.<*> (x Core..:? "PreferredMaintenanceWindow")
            Prelude.<*> ( x Core..:? "AvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "PreferredBackupWindow")
            Prelude.<*> ( x Core..:? "AssociatedRoles"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "VpcSecurityGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "BackupRetentionPeriod")
            Prelude.<*> (x Core..:? "DbSubnetGroup")
            Prelude.<*> (x Core..:? "MultiAz")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "EngineMode")
            Prelude.<*> ( x Core..:? "EnabledCloudWatchLogsExports"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ActivityStreamStatus")
            Prelude.<*> (x Core..:? "AllocatedStorage")
            Prelude.<*> (x Core..:? "CopyTagsToSnapshot")
            Prelude.<*> (x Core..:? "ClusterCreateTime")
            Prelude.<*> (x Core..:? "Endpoint")
            Prelude.<*> (x Core..:? "ReaderEndpoint")
            Prelude.<*> ( x Core..:? "DbClusterOptionGroupMemberships"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Port")
            Prelude.<*> ( x Core..:? "DomainMemberships"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsRdsDbClusterDetails

instance Prelude.NFData AwsRdsDbClusterDetails

instance Core.ToJSON AwsRdsDbClusterDetails where
  toJSON AwsRdsDbClusterDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EngineVersion" Core..=) Prelude.<$> engineVersion,
            ("Status" Core..=) Prelude.<$> status,
            ("DbClusterMembers" Core..=)
              Prelude.<$> dbClusterMembers,
            ("DeletionProtection" Core..=)
              Prelude.<$> deletionProtection,
            ("StorageEncrypted" Core..=)
              Prelude.<$> storageEncrypted,
            ("DbClusterIdentifier" Core..=)
              Prelude.<$> dbClusterIdentifier,
            ("ReadReplicaIdentifiers" Core..=)
              Prelude.<$> readReplicaIdentifiers,
            ("DbClusterParameterGroup" Core..=)
              Prelude.<$> dbClusterParameterGroup,
            ("HostedZoneId" Core..=) Prelude.<$> hostedZoneId,
            ("MasterUsername" Core..=)
              Prelude.<$> masterUsername,
            ("IamDatabaseAuthenticationEnabled" Core..=)
              Prelude.<$> iamDatabaseAuthenticationEnabled,
            ("DbClusterResourceId" Core..=)
              Prelude.<$> dbClusterResourceId,
            ("CustomEndpoints" Core..=)
              Prelude.<$> customEndpoints,
            ("Engine" Core..=) Prelude.<$> engine,
            ("HttpEndpointEnabled" Core..=)
              Prelude.<$> httpEndpointEnabled,
            ("CrossAccountClone" Core..=)
              Prelude.<$> crossAccountClone,
            ("PreferredMaintenanceWindow" Core..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("AvailabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("PreferredBackupWindow" Core..=)
              Prelude.<$> preferredBackupWindow,
            ("AssociatedRoles" Core..=)
              Prelude.<$> associatedRoles,
            ("VpcSecurityGroups" Core..=)
              Prelude.<$> vpcSecurityGroups,
            ("BackupRetentionPeriod" Core..=)
              Prelude.<$> backupRetentionPeriod,
            ("DbSubnetGroup" Core..=) Prelude.<$> dbSubnetGroup,
            ("MultiAz" Core..=) Prelude.<$> multiAz,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("EngineMode" Core..=) Prelude.<$> engineMode,
            ("EnabledCloudWatchLogsExports" Core..=)
              Prelude.<$> enabledCloudWatchLogsExports,
            ("ActivityStreamStatus" Core..=)
              Prelude.<$> activityStreamStatus,
            ("AllocatedStorage" Core..=)
              Prelude.<$> allocatedStorage,
            ("CopyTagsToSnapshot" Core..=)
              Prelude.<$> copyTagsToSnapshot,
            ("ClusterCreateTime" Core..=)
              Prelude.<$> clusterCreateTime,
            ("Endpoint" Core..=) Prelude.<$> endpoint,
            ("ReaderEndpoint" Core..=)
              Prelude.<$> readerEndpoint,
            ("DbClusterOptionGroupMemberships" Core..=)
              Prelude.<$> dbClusterOptionGroupMemberships,
            ("Port" Core..=) Prelude.<$> port,
            ("DomainMemberships" Core..=)
              Prelude.<$> domainMemberships
          ]
      )
