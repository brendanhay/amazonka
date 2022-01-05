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
-- Module      : Amazonka.DMS.Types.ReplicationInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationInstance where

import qualified Amazonka.Core as Core
import Amazonka.DMS.Types.ReplicationPendingModifiedValues
import Amazonka.DMS.Types.ReplicationSubnetGroup
import Amazonka.DMS.Types.VpcSecurityGroupMembership
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines a replication instance.
--
-- /See:/ 'newReplicationInstance' smart constructor.
data ReplicationInstance = ReplicationInstance'
  { -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance
    -- is created, the default is the latest engine version available.
    --
    -- When modifying a major engine version of an instance, also set
    -- @AllowMajorVersionUpgrade@ to @true@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the accessibility options for the replication instance. A
    -- value of @true@ represents an instance with a public IP address. A value
    -- of @false@ represents an instance with a private IP address. The default
    -- value is @true@.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Boolean value indicating if minor version upgrades will be automatically
    -- applied to the instance.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | One or more public IP addresses for the replication instance.
    replicationInstancePublicIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The subnet group for the replication instance.
    replicationSubnetGroup :: Prelude.Maybe ReplicationSubnetGroup,
    -- | The time the replication instance was created.
    instanceCreateTime :: Prelude.Maybe Core.POSIX,
    -- | The expiration date of the free replication instance that is part of the
    -- Free DMS program.
    freeUntil :: Prelude.Maybe Core.POSIX,
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
    -- | One or more private IP addresses for the replication instance.
    replicationInstancePrivateIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The maintenance window times for the replication instance. Any pending
    -- upgrades to the replication instance are performed during this time.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of the replication instance.
    replicationInstancePrivateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | An KMS key identifier that is used to encrypt the data on the
    -- replication instance.
    --
    -- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
    -- uses your default encryption key.
    --
    -- KMS creates the default encryption key for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default
    -- encryption key for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone for the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The VPC security group for the instance.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You
    -- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
    -- set to @true@.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone of the standby replication instance in a Multi-AZ
    -- deployment.
    secondaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage (in gigabytes) that is allocated for the
    -- replication instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The DNS name servers supported for the replication instance to access
    -- your on-premise source or target database.
    dnsNameServers :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of the replication instance.
    replicationInstancePublicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The compute and memory capacity of the replication instance as defined
    -- for the specified replication instance class. It is a required
    -- parameter, although a default value is pre-selected in the DMS console.
    --
    -- For more information on the settings and capacities for the available
    -- replication instance classes, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
    replicationInstanceClass :: Prelude.Maybe Prelude.Text,
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
    pendingModifiedValues :: Prelude.Maybe ReplicationPendingModifiedValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'replicationInstance_engineVersion' - The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
--
-- When modifying a major engine version of an instance, also set
-- @AllowMajorVersionUpgrade@ to @true@.
--
-- 'publiclyAccessible', 'replicationInstance_publiclyAccessible' - Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
--
-- 'autoMinorVersionUpgrade', 'replicationInstance_autoMinorVersionUpgrade' - Boolean value indicating if minor version upgrades will be automatically
-- applied to the instance.
--
-- 'replicationInstancePublicIpAddresses', 'replicationInstance_replicationInstancePublicIpAddresses' - One or more public IP addresses for the replication instance.
--
-- 'replicationSubnetGroup', 'replicationInstance_replicationSubnetGroup' - The subnet group for the replication instance.
--
-- 'instanceCreateTime', 'replicationInstance_instanceCreateTime' - The time the replication instance was created.
--
-- 'freeUntil', 'replicationInstance_freeUntil' - The expiration date of the free replication instance that is part of the
-- Free DMS program.
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
-- 'replicationInstancePrivateIpAddresses', 'replicationInstance_replicationInstancePrivateIpAddresses' - One or more private IP addresses for the replication instance.
--
-- 'preferredMaintenanceWindow', 'replicationInstance_preferredMaintenanceWindow' - The maintenance window times for the replication instance. Any pending
-- upgrades to the replication instance are performed during this time.
--
-- 'replicationInstancePrivateIpAddress', 'replicationInstance_replicationInstancePrivateIpAddress' - The private IP address of the replication instance.
--
-- 'kmsKeyId', 'replicationInstance_kmsKeyId' - An KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
--
-- 'availabilityZone', 'replicationInstance_availabilityZone' - The Availability Zone for the instance.
--
-- 'vpcSecurityGroups', 'replicationInstance_vpcSecurityGroups' - The VPC security group for the instance.
--
-- 'multiAZ', 'replicationInstance_multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
--
-- 'secondaryAvailabilityZone', 'replicationInstance_secondaryAvailabilityZone' - The Availability Zone of the standby replication instance in a Multi-AZ
-- deployment.
--
-- 'replicationInstanceArn', 'replicationInstance_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
--
-- 'allocatedStorage', 'replicationInstance_allocatedStorage' - The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
--
-- 'dnsNameServers', 'replicationInstance_dnsNameServers' - The DNS name servers supported for the replication instance to access
-- your on-premise source or target database.
--
-- 'replicationInstancePublicIpAddress', 'replicationInstance_replicationInstancePublicIpAddress' - The public IP address of the replication instance.
--
-- 'replicationInstanceClass', 'replicationInstance_replicationInstanceClass' - The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. It is a required
-- parameter, although a default value is pre-selected in the DMS console.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
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
newReplicationInstance ::
  ReplicationInstance
newReplicationInstance =
  ReplicationInstance'
    { engineVersion =
        Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      replicationInstancePublicIpAddresses =
        Prelude.Nothing,
      replicationSubnetGroup = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      freeUntil = Prelude.Nothing,
      replicationInstanceStatus = Prelude.Nothing,
      replicationInstancePrivateIpAddresses =
        Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      replicationInstancePrivateIpAddress =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      secondaryAvailabilityZone = Prelude.Nothing,
      replicationInstanceArn = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      dnsNameServers = Prelude.Nothing,
      replicationInstancePublicIpAddress = Prelude.Nothing,
      replicationInstanceClass = Prelude.Nothing,
      replicationInstanceIdentifier = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing
    }

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance
-- is created, the default is the latest engine version available.
--
-- When modifying a major engine version of an instance, also set
-- @AllowMajorVersionUpgrade@ to @true@.
replicationInstance_engineVersion :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_engineVersion = Lens.lens (\ReplicationInstance' {engineVersion} -> engineVersion) (\s@ReplicationInstance' {} a -> s {engineVersion = a} :: ReplicationInstance)

-- | Specifies the accessibility options for the replication instance. A
-- value of @true@ represents an instance with a public IP address. A value
-- of @false@ represents an instance with a private IP address. The default
-- value is @true@.
replicationInstance_publiclyAccessible :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Bool)
replicationInstance_publiclyAccessible = Lens.lens (\ReplicationInstance' {publiclyAccessible} -> publiclyAccessible) (\s@ReplicationInstance' {} a -> s {publiclyAccessible = a} :: ReplicationInstance)

-- | Boolean value indicating if minor version upgrades will be automatically
-- applied to the instance.
replicationInstance_autoMinorVersionUpgrade :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Bool)
replicationInstance_autoMinorVersionUpgrade = Lens.lens (\ReplicationInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ReplicationInstance' {} a -> s {autoMinorVersionUpgrade = a} :: ReplicationInstance)

-- | One or more public IP addresses for the replication instance.
replicationInstance_replicationInstancePublicIpAddresses :: Lens.Lens' ReplicationInstance (Prelude.Maybe [Prelude.Text])
replicationInstance_replicationInstancePublicIpAddresses = Lens.lens (\ReplicationInstance' {replicationInstancePublicIpAddresses} -> replicationInstancePublicIpAddresses) (\s@ReplicationInstance' {} a -> s {replicationInstancePublicIpAddresses = a} :: ReplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | The subnet group for the replication instance.
replicationInstance_replicationSubnetGroup :: Lens.Lens' ReplicationInstance (Prelude.Maybe ReplicationSubnetGroup)
replicationInstance_replicationSubnetGroup = Lens.lens (\ReplicationInstance' {replicationSubnetGroup} -> replicationSubnetGroup) (\s@ReplicationInstance' {} a -> s {replicationSubnetGroup = a} :: ReplicationInstance)

-- | The time the replication instance was created.
replicationInstance_instanceCreateTime :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.UTCTime)
replicationInstance_instanceCreateTime = Lens.lens (\ReplicationInstance' {instanceCreateTime} -> instanceCreateTime) (\s@ReplicationInstance' {} a -> s {instanceCreateTime = a} :: ReplicationInstance) Prelude.. Lens.mapping Core._Time

-- | The expiration date of the free replication instance that is part of the
-- Free DMS program.
replicationInstance_freeUntil :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.UTCTime)
replicationInstance_freeUntil = Lens.lens (\ReplicationInstance' {freeUntil} -> freeUntil) (\s@ReplicationInstance' {} a -> s {freeUntil = a} :: ReplicationInstance) Prelude.. Lens.mapping Core._Time

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

-- | One or more private IP addresses for the replication instance.
replicationInstance_replicationInstancePrivateIpAddresses :: Lens.Lens' ReplicationInstance (Prelude.Maybe [Prelude.Text])
replicationInstance_replicationInstancePrivateIpAddresses = Lens.lens (\ReplicationInstance' {replicationInstancePrivateIpAddresses} -> replicationInstancePrivateIpAddresses) (\s@ReplicationInstance' {} a -> s {replicationInstancePrivateIpAddresses = a} :: ReplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | The maintenance window times for the replication instance. Any pending
-- upgrades to the replication instance are performed during this time.
replicationInstance_preferredMaintenanceWindow :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_preferredMaintenanceWindow = Lens.lens (\ReplicationInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ReplicationInstance' {} a -> s {preferredMaintenanceWindow = a} :: ReplicationInstance)

-- | The private IP address of the replication instance.
replicationInstance_replicationInstancePrivateIpAddress :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstancePrivateIpAddress = Lens.lens (\ReplicationInstance' {replicationInstancePrivateIpAddress} -> replicationInstancePrivateIpAddress) (\s@ReplicationInstance' {} a -> s {replicationInstancePrivateIpAddress = a} :: ReplicationInstance)

-- | An KMS key identifier that is used to encrypt the data on the
-- replication instance.
--
-- If you don\'t specify a value for the @KmsKeyId@ parameter, then DMS
-- uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Region.
replicationInstance_kmsKeyId :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_kmsKeyId = Lens.lens (\ReplicationInstance' {kmsKeyId} -> kmsKeyId) (\s@ReplicationInstance' {} a -> s {kmsKeyId = a} :: ReplicationInstance)

-- | The Availability Zone for the instance.
replicationInstance_availabilityZone :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_availabilityZone = Lens.lens (\ReplicationInstance' {availabilityZone} -> availabilityZone) (\s@ReplicationInstance' {} a -> s {availabilityZone = a} :: ReplicationInstance)

-- | The VPC security group for the instance.
replicationInstance_vpcSecurityGroups :: Lens.Lens' ReplicationInstance (Prelude.Maybe [VpcSecurityGroupMembership])
replicationInstance_vpcSecurityGroups = Lens.lens (\ReplicationInstance' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@ReplicationInstance' {} a -> s {vpcSecurityGroups = a} :: ReplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the replication instance is a Multi-AZ deployment. You
-- can\'t set the @AvailabilityZone@ parameter if the Multi-AZ parameter is
-- set to @true@.
replicationInstance_multiAZ :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Bool)
replicationInstance_multiAZ = Lens.lens (\ReplicationInstance' {multiAZ} -> multiAZ) (\s@ReplicationInstance' {} a -> s {multiAZ = a} :: ReplicationInstance)

-- | The Availability Zone of the standby replication instance in a Multi-AZ
-- deployment.
replicationInstance_secondaryAvailabilityZone :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_secondaryAvailabilityZone = Lens.lens (\ReplicationInstance' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@ReplicationInstance' {} a -> s {secondaryAvailabilityZone = a} :: ReplicationInstance)

-- | The Amazon Resource Name (ARN) of the replication instance.
replicationInstance_replicationInstanceArn :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstanceArn = Lens.lens (\ReplicationInstance' {replicationInstanceArn} -> replicationInstanceArn) (\s@ReplicationInstance' {} a -> s {replicationInstanceArn = a} :: ReplicationInstance)

-- | The amount of storage (in gigabytes) that is allocated for the
-- replication instance.
replicationInstance_allocatedStorage :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Int)
replicationInstance_allocatedStorage = Lens.lens (\ReplicationInstance' {allocatedStorage} -> allocatedStorage) (\s@ReplicationInstance' {} a -> s {allocatedStorage = a} :: ReplicationInstance)

-- | The DNS name servers supported for the replication instance to access
-- your on-premise source or target database.
replicationInstance_dnsNameServers :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_dnsNameServers = Lens.lens (\ReplicationInstance' {dnsNameServers} -> dnsNameServers) (\s@ReplicationInstance' {} a -> s {dnsNameServers = a} :: ReplicationInstance)

-- | The public IP address of the replication instance.
replicationInstance_replicationInstancePublicIpAddress :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstancePublicIpAddress = Lens.lens (\ReplicationInstance' {replicationInstancePublicIpAddress} -> replicationInstancePublicIpAddress) (\s@ReplicationInstance' {} a -> s {replicationInstancePublicIpAddress = a} :: ReplicationInstance)

-- | The compute and memory capacity of the replication instance as defined
-- for the specified replication instance class. It is a required
-- parameter, although a default value is pre-selected in the DMS console.
--
-- For more information on the settings and capacities for the available
-- replication instance classes, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right DMS replication instance for your migration>.
replicationInstance_replicationInstanceClass :: Lens.Lens' ReplicationInstance (Prelude.Maybe Prelude.Text)
replicationInstance_replicationInstanceClass = Lens.lens (\ReplicationInstance' {replicationInstanceClass} -> replicationInstanceClass) (\s@ReplicationInstance' {} a -> s {replicationInstanceClass = a} :: ReplicationInstance)

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

instance Core.FromJSON ReplicationInstance where
  parseJSON =
    Core.withObject
      "ReplicationInstance"
      ( \x ->
          ReplicationInstance'
            Prelude.<$> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "PubliclyAccessible")
            Prelude.<*> (x Core..:? "AutoMinorVersionUpgrade")
            Prelude.<*> ( x Core..:? "ReplicationInstancePublicIpAddresses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ReplicationSubnetGroup")
            Prelude.<*> (x Core..:? "InstanceCreateTime")
            Prelude.<*> (x Core..:? "FreeUntil")
            Prelude.<*> (x Core..:? "ReplicationInstanceStatus")
            Prelude.<*> ( x Core..:? "ReplicationInstancePrivateIpAddresses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PreferredMaintenanceWindow")
            Prelude.<*> (x Core..:? "ReplicationInstancePrivateIpAddress")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> ( x Core..:? "VpcSecurityGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "MultiAZ")
            Prelude.<*> (x Core..:? "SecondaryAvailabilityZone")
            Prelude.<*> (x Core..:? "ReplicationInstanceArn")
            Prelude.<*> (x Core..:? "AllocatedStorage")
            Prelude.<*> (x Core..:? "DnsNameServers")
            Prelude.<*> (x Core..:? "ReplicationInstancePublicIpAddress")
            Prelude.<*> (x Core..:? "ReplicationInstanceClass")
            Prelude.<*> (x Core..:? "ReplicationInstanceIdentifier")
            Prelude.<*> (x Core..:? "PendingModifiedValues")
      )

instance Prelude.Hashable ReplicationInstance where
  hashWithSalt _salt ReplicationInstance' {..} =
    _salt `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` replicationInstancePublicIpAddresses
      `Prelude.hashWithSalt` replicationSubnetGroup
      `Prelude.hashWithSalt` instanceCreateTime
      `Prelude.hashWithSalt` freeUntil
      `Prelude.hashWithSalt` replicationInstanceStatus
      `Prelude.hashWithSalt` replicationInstancePrivateIpAddresses
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` replicationInstancePrivateIpAddress
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` vpcSecurityGroups
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` secondaryAvailabilityZone
      `Prelude.hashWithSalt` replicationInstanceArn
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` dnsNameServers
      `Prelude.hashWithSalt` replicationInstancePublicIpAddress
      `Prelude.hashWithSalt` replicationInstanceClass
      `Prelude.hashWithSalt` replicationInstanceIdentifier
      `Prelude.hashWithSalt` pendingModifiedValues

instance Prelude.NFData ReplicationInstance where
  rnf ReplicationInstance' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf replicationInstancePublicIpAddresses
      `Prelude.seq` Prelude.rnf replicationSubnetGroup
      `Prelude.seq` Prelude.rnf instanceCreateTime
      `Prelude.seq` Prelude.rnf freeUntil
      `Prelude.seq` Prelude.rnf replicationInstanceStatus
      `Prelude.seq` Prelude.rnf replicationInstancePrivateIpAddresses
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf replicationInstancePrivateIpAddress
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf vpcSecurityGroups
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf secondaryAvailabilityZone
      `Prelude.seq` Prelude.rnf replicationInstanceArn
      `Prelude.seq` Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf dnsNameServers
      `Prelude.seq` Prelude.rnf
        replicationInstancePublicIpAddress
      `Prelude.seq` Prelude.rnf
        replicationInstanceClass
      `Prelude.seq` Prelude.rnf
        replicationInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
