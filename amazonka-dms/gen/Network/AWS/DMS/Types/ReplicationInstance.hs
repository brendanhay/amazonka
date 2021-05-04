{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DMS.Types.ReplicationInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationInstance where

import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
import Network.AWS.DMS.Types.ReplicationSubnetGroup
import Network.AWS.DMS.Types.VpcSecurityGroupMembership
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines a replication instance.
--
-- /See:/ 'newReplicationInstance' smart constructor.
data ReplicationInstance = ReplicationInstance'
  { -- | The private IP address of the replication instance.
    replicationInstancePrivateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The VPC security group for the instance.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | The expiration date of the free replication instance that is part of the
    -- Free DMS program.
    freeUntil :: Prelude.Maybe Prelude.POSIX,
    -- | The subnet group for the replication instance.
    replicationSubnetGroup :: Prelude.Maybe ReplicationSubnetGroup,
    -- | The time the replication instance was created.
    instanceCreateTime :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You
    -- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
    -- set to @true@.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the accessibility options for the replication instance. A
    -- value of @true@ represents an instance with a public IP address. A value
    -- of @false@ represents an instance with a private IP address. The default
    -- value is @true@.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | An AWS KMS key identifier that is used to encrypt the data on the
    -- replication instance.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
    -- uses your default encryption key.
    --
    -- AWS KMS creates the default encryption key for your AWS account. Your
    -- AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone for the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance
    -- is created, the default is the latest engine version available.
    --
    -- When modifying a major engine version of an instance, also set
    -- @AllowMajorVersionUpgrade@ to @true@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The maintenance window times for the replication instance. Any pending
    -- upgrades to the replication instance are performed during this time.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | One or more private IP addresses for the replication instance.
    replicationInstancePrivateIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The status of the replication instance. The possible return values
    -- include:
    --
    -- -   @\"available\"@
    --
    -- -   @\"creating\"@
    --
    -- -   @\"deleted\"@
    --
    -- -   @\"deleting\"@
    --
    -- -   @\"failed\"@
    --
    -- -   @\"modifying\"@
    --
    -- -   @\"upgrading\"@
    --
    -- -   @\"rebooting\"@
    --
    -- -   @\"resetting-master-credentials\"@
    --
    -- -   @\"storage-full\"@
    --
    -- -   @\"incompatible-credentials\"@
    --
    -- -   @\"incompatible-network\"@
    --
    -- -   @\"maintenance\"@
    replicationInstanceStatus :: Prelude.Maybe Prelude.Text,
    -- | The replication instance identifier is a required parameter. This
    -- parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain 1-63 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @myrepinstance@
    replicationInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The pending modification values.
    pendingModifiedValues :: Prelude.Maybe ReplicationPendingModifiedValues,
    -- | The public IP address of the replication instance.
    replicationInstancePublicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The compute and memory capacity of the replication instance as defined
    -- for the specified replication instance class. It is a required
    -- parameter, although a defualt value is pre-selected in the DMS console.
    --
    -- For more information on the settings and capacities for the available
    -- replication instance classes, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
    replicationInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The DNS name servers supported for the replication instance to access
    -- your on-premise source or target database.
    dnsNameServers :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) that is allocated for the
    -- replication instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | One or more public IP addresses for the replication instance.
    replicationInstancePublicIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The Availability Zone of the standby replication instance in a Multi-AZ
    -- deployment.
    secondaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Boolean value indicating if minor version upgrades will be automatically
    -- applied to the instance.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstancePrivateIpAddress', 'replicationInstance_replicationInstancePrivateIpAddress' - The private IP address of the replication instance.
--
-- 'vpcSecurityGroups', 'replicationInstance_vpcSecurityGroups' - The VPC security group for the instance.
--
-- 'freeUntil', 'replicationInstance_freeUntil' - The expiration date of the free replication instance that is part of the
-- Free DMS program.
--
-- 'replicationSubnetGroup', 'replicationInstance_replicationSubnetGroup' - The subnet group for the replication instance.
--
-- 'instanceCreateTime', 'replicationInstance_instanceCreateTime' - The time the replication instance was created.
--
-- 'multiAZ', 'replicationInstance_multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
--
-- 'publiclyAccessible', 'replicationInstance_publiclyAccessible' - Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
--
-- 'kmsKeyId', 'replicationInstance_kmsKeyId' - An AWS KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
--
-- 'availabilityZone', 'replicationInstance_availabilityZone' - The Availability Zone for the instance.
--
-- 'engineVersion', 'replicationInstance_engineVersion' - The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
--
-- When modifying a major engine version of an instance, also set
-- @AllowMajorVersionUpgrade@ to @true@.
--
-- 'preferredMaintenanceWindow', 'replicationInstance_preferredMaintenanceWindow' - The maintenance window times for the replication instance. Any pending
-- upgrades to the replication instance are performed during this time.
--
-- 'replicationInstancePrivateIpAddresses', 'replicationInstance_replicationInstancePrivateIpAddresses' - One or more private IP addresses for the replication instance.
--
-- 'replicationInstanceStatus', 'replicationInstance_replicationInstanceStatus' - The status of the replication instance. The possible return values
-- include:
--
-- -   @\"available\"@
--
-- -   @\"creating\"@
--
-- -   @\"deleted\"@
--
-- -   @\"deleting\"@
--
-- -   @\"failed\"@
--
-- -   @\"modifying\"@
--
-- -   @\"upgrading\"@
--
-- -   @\"rebooting\"@
--
-- -   @\"resetting-master-credentials\"@
--
-- -   @\"storage-full\"@
--
-- -   @\"incompatible-credentials\"@
--
-- -   @\"incompatible-network\"@
--
-- -   @\"maintenance\"@
--
-- 'replicationInstanceIdentifier', 'replicationInstance_replicationInstanceIdentifier' - The replication instance identifier is a required parameter. This
-- parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain 1-63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @myrepinstance@
--
-- 'pendingModifiedValues', 'replicationInstance_pendingModifiedValues' - The pending modification values.
--
-- 'replicationInstancePublicIpAddress', 'replicationInstance_replicationInstancePublicIpAddress' - The public IP address of the replication instance.
--
-- 'replicationInstanceClass', 'replicationInstance_replicationInstanceClass' - The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. It is a required
-- parameter, although a defualt value is pre-selected in the DMS console.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
--
-- 'replicationInstanceArn', 'replicationInstance_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
--
-- 'dnsNameServers', 'replicationInstance_dnsNameServers' - The DNS name servers supported for the replication instance to access
-- your on-premise source or target database.
--
-- 'allocatedStorage', 'replicationInstance_allocatedStorage' - The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
--
-- 'replicationInstancePublicIpAddresses', 'replicationInstance_replicationInstancePublicIpAddresses' - One or more public IP addresses for the replication instance.
--
-- 'secondaryAvailabilityZone', 'replicationInstance_secondaryAvailabilityZone' - The Availability Zone of the standby replication instance in a Multi-AZ
-- deployment.
--
-- 'autoMinorVersionUpgrade', 'replicationInstance_autoMinorVersionUpgrade' - Boolean value indicating if minor version upgrades will be automatically
-- applied to the instance.
newReplicationInstance ::
  ReplicationInstance
newReplicationInstance =
  ReplicationInstance'
    { replicationInstancePrivateIpAddress =
        Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      freeUntil = Prelude.Nothing,
      replicationSubnetGroup = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      replicationInstancePrivateIpAddresses =
        Prelude.Nothing,
      replicationInstanceStatus = Prelude.Nothing,
      replicationInstanceIdentifier = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      replicationInstancePublicIpAddress = Prelude.Nothing,
      replicationInstanceClass = Prelude.Nothing,
      replicationInstanceArn = Prelude.Nothing,
      dnsNameServers = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      replicationInstancePublicIpAddresses =
        Prelude.Nothing,
      secondaryAvailabilityZone = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing
    }

-- | The private IP address of the replication instance.
replicationInstance_replicationInstancePrivateIpAddress :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstancePrivateIpAddress = Lens.lens (\ReplicationInstance' {replicationInstancePrivateIpAddress} -> replicationInstancePrivateIpAddress) (\s@ReplicationInstance' {} a -> s {replicationInstancePrivateIpAddress = a} :: ReplicationInstance)

-- | The VPC security group for the instance.
replicationInstance_vpcSecurityGroups :: Lens.Lens' ReplicationInstance (Prelude.Maybe [VpcSecurityGroupMembership])
replicationInstance_vpcSecurityGroups = Lens.lens (\ReplicationInstance' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@ReplicationInstance' {} a -> s {vpcSecurityGroups = a} :: ReplicationInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | The expiration date of the free replication instance that is part of the
-- Free DMS program.
replicationInstance_freeUntil :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.UTCTime)
replicationInstance_freeUntil = Lens.lens (\ReplicationInstance' {freeUntil} -> freeUntil) (\s@ReplicationInstance' {} a -> s {freeUntil = a} :: ReplicationInstance) Prelude.. Lens.mapping Prelude._Time

-- | The subnet group for the replication instance.
replicationInstance_replicationSubnetGroup :: Lens.Lens' ReplicationInstance (Prelude.Maybe ReplicationSubnetGroup)
replicationInstance_replicationSubnetGroup = Lens.lens (\ReplicationInstance' {replicationSubnetGroup} -> replicationSubnetGroup) (\s@ReplicationInstance' {} a -> s {replicationSubnetGroup = a} :: ReplicationInstance)

-- | The time the replication instance was created.
replicationInstance_instanceCreateTime :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.UTCTime)
replicationInstance_instanceCreateTime = Lens.lens (\ReplicationInstance' {instanceCreateTime} -> instanceCreateTime) (\s@ReplicationInstance' {} a -> s {instanceCreateTime = a} :: ReplicationInstance) Prelude.. Lens.mapping Prelude._Time

-- | Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
replicationInstance_multiAZ :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Bool)
replicationInstance_multiAZ = Lens.lens (\ReplicationInstance' {multiAZ} -> multiAZ) (\s@ReplicationInstance' {} a -> s {multiAZ = a} :: ReplicationInstance)

-- | Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
replicationInstance_publiclyAccessible :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Bool)
replicationInstance_publiclyAccessible = Lens.lens (\ReplicationInstance' {publiclyAccessible} -> publiclyAccessible) (\s@ReplicationInstance' {} a -> s {publiclyAccessible = a} :: ReplicationInstance)

-- | An AWS KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then AWS DMS
-- uses your default encryption key.
--
-- AWS KMS creates the default encryption key for your AWS account. Your
-- AWS account has a different default encryption key for each AWS Region.
replicationInstance_kmsKeyId :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_kmsKeyId = Lens.lens (\ReplicationInstance' {kmsKeyId} -> kmsKeyId) (\s@ReplicationInstance' {} a -> s {kmsKeyId = a} :: ReplicationInstance)

-- | The Availability Zone for the instance.
replicationInstance_availabilityZone :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_availabilityZone = Lens.lens (\ReplicationInstance' {availabilityZone} -> availabilityZone) (\s@ReplicationInstance' {} a -> s {availabilityZone = a} :: ReplicationInstance)

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
--
-- When modifying a major engine version of an instance, also set
-- @AllowMajorVersionUpgrade@ to @true@.
replicationInstance_engineVersion :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_engineVersion = Lens.lens (\ReplicationInstance' {engineVersion} -> engineVersion) (\s@ReplicationInstance' {} a -> s {engineVersion = a} :: ReplicationInstance)

-- | The maintenance window times for the replication instance. Any pending
-- upgrades to the replication instance are performed during this time.
replicationInstance_preferredMaintenanceWindow :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_preferredMaintenanceWindow = Lens.lens (\ReplicationInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ReplicationInstance' {} a -> s {preferredMaintenanceWindow = a} :: ReplicationInstance)

-- | One or more private IP addresses for the replication instance.
replicationInstance_replicationInstancePrivateIpAddresses :: Lens.Lens' ReplicationInstance (Prelude.Maybe [Prelude.Text])
replicationInstance_replicationInstancePrivateIpAddresses = Lens.lens (\ReplicationInstance' {replicationInstancePrivateIpAddresses} -> replicationInstancePrivateIpAddresses) (\s@ReplicationInstance' {} a -> s {replicationInstancePrivateIpAddresses = a} :: ReplicationInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | The status of the replication instance. The possible return values
-- include:
--
-- -   @\"available\"@
--
-- -   @\"creating\"@
--
-- -   @\"deleted\"@
--
-- -   @\"deleting\"@
--
-- -   @\"failed\"@
--
-- -   @\"modifying\"@
--
-- -   @\"upgrading\"@
--
-- -   @\"rebooting\"@
--
-- -   @\"resetting-master-credentials\"@
--
-- -   @\"storage-full\"@
--
-- -   @\"incompatible-credentials\"@
--
-- -   @\"incompatible-network\"@
--
-- -   @\"maintenance\"@
replicationInstance_replicationInstanceStatus :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstanceStatus = Lens.lens (\ReplicationInstance' {replicationInstanceStatus} -> replicationInstanceStatus) (\s@ReplicationInstance' {} a -> s {replicationInstanceStatus = a} :: ReplicationInstance)

-- | The replication instance identifier is a required parameter. This
-- parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain 1-63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @myrepinstance@
replicationInstance_replicationInstanceIdentifier :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstanceIdentifier = Lens.lens (\ReplicationInstance' {replicationInstanceIdentifier} -> replicationInstanceIdentifier) (\s@ReplicationInstance' {} a -> s {replicationInstanceIdentifier = a} :: ReplicationInstance)

-- | The pending modification values.
replicationInstance_pendingModifiedValues :: Lens.Lens' ReplicationInstance (Prelude.Maybe ReplicationPendingModifiedValues)
replicationInstance_pendingModifiedValues = Lens.lens (\ReplicationInstance' {pendingModifiedValues} -> pendingModifiedValues) (\s@ReplicationInstance' {} a -> s {pendingModifiedValues = a} :: ReplicationInstance)

-- | The public IP address of the replication instance.
replicationInstance_replicationInstancePublicIpAddress :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstancePublicIpAddress = Lens.lens (\ReplicationInstance' {replicationInstancePublicIpAddress} -> replicationInstancePublicIpAddress) (\s@ReplicationInstance' {} a -> s {replicationInstancePublicIpAddress = a} :: ReplicationInstance)

-- | The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. It is a required
-- parameter, although a defualt value is pre-selected in the DMS console.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration>.
replicationInstance_replicationInstanceClass :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstanceClass = Lens.lens (\ReplicationInstance' {replicationInstanceClass} -> replicationInstanceClass) (\s@ReplicationInstance' {} a -> s {replicationInstanceClass = a} :: ReplicationInstance)

-- | The Amazon Resource Name (ARN) of the replication instance.
replicationInstance_replicationInstanceArn :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstanceArn = Lens.lens (\ReplicationInstance' {replicationInstanceArn} -> replicationInstanceArn) (\s@ReplicationInstance' {} a -> s {replicationInstanceArn = a} :: ReplicationInstance)

-- | The DNS name servers supported for the replication instance to access
-- your on-premise source or target database.
replicationInstance_dnsNameServers :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_dnsNameServers = Lens.lens (\ReplicationInstance' {dnsNameServers} -> dnsNameServers) (\s@ReplicationInstance' {} a -> s {dnsNameServers = a} :: ReplicationInstance)

-- | The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
replicationInstance_allocatedStorage :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Int)
replicationInstance_allocatedStorage = Lens.lens (\ReplicationInstance' {allocatedStorage} -> allocatedStorage) (\s@ReplicationInstance' {} a -> s {allocatedStorage = a} :: ReplicationInstance)

-- | One or more public IP addresses for the replication instance.
replicationInstance_replicationInstancePublicIpAddresses :: Lens.Lens' ReplicationInstance (Prelude.Maybe [Prelude.Text])
replicationInstance_replicationInstancePublicIpAddresses = Lens.lens (\ReplicationInstance' {replicationInstancePublicIpAddresses} -> replicationInstancePublicIpAddresses) (\s@ReplicationInstance' {} a -> s {replicationInstancePublicIpAddresses = a} :: ReplicationInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zone of the standby replication instance in a Multi-AZ
-- deployment.
replicationInstance_secondaryAvailabilityZone :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_secondaryAvailabilityZone = Lens.lens (\ReplicationInstance' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@ReplicationInstance' {} a -> s {secondaryAvailabilityZone = a} :: ReplicationInstance)

-- | Boolean value indicating if minor version upgrades will be automatically
-- applied to the instance.
replicationInstance_autoMinorVersionUpgrade :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Bool)
replicationInstance_autoMinorVersionUpgrade = Lens.lens (\ReplicationInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ReplicationInstance' {} a -> s {autoMinorVersionUpgrade = a} :: ReplicationInstance)

instance Prelude.FromJSON ReplicationInstance where
  parseJSON =
    Prelude.withObject
      "ReplicationInstance"
      ( \x ->
          ReplicationInstance'
            Prelude.<$> (x Prelude..:? "ReplicationInstancePrivateIpAddress")
            Prelude.<*> ( x Prelude..:? "VpcSecurityGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "FreeUntil")
            Prelude.<*> (x Prelude..:? "ReplicationSubnetGroup")
            Prelude.<*> (x Prelude..:? "InstanceCreateTime")
            Prelude.<*> (x Prelude..:? "MultiAZ")
            Prelude.<*> (x Prelude..:? "PubliclyAccessible")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "AvailabilityZone")
            Prelude.<*> (x Prelude..:? "EngineVersion")
            Prelude.<*> (x Prelude..:? "PreferredMaintenanceWindow")
            Prelude.<*> ( x
                            Prelude..:? "ReplicationInstancePrivateIpAddresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ReplicationInstanceStatus")
            Prelude.<*> (x Prelude..:? "ReplicationInstanceIdentifier")
            Prelude.<*> (x Prelude..:? "PendingModifiedValues")
            Prelude.<*> (x Prelude..:? "ReplicationInstancePublicIpAddress")
            Prelude.<*> (x Prelude..:? "ReplicationInstanceClass")
            Prelude.<*> (x Prelude..:? "ReplicationInstanceArn")
            Prelude.<*> (x Prelude..:? "DnsNameServers")
            Prelude.<*> (x Prelude..:? "AllocatedStorage")
            Prelude.<*> ( x Prelude..:? "ReplicationInstancePublicIpAddresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "SecondaryAvailabilityZone")
            Prelude.<*> (x Prelude..:? "AutoMinorVersionUpgrade")
      )

instance Prelude.Hashable ReplicationInstance

instance Prelude.NFData ReplicationInstance
