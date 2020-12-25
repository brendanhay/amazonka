{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationInstance
  ( ReplicationInstance (..),

    -- * Smart constructor
    mkReplicationInstance,

    -- * Lenses
    riAllocatedStorage,
    riAutoMinorVersionUpgrade,
    riAvailabilityZone,
    riDnsNameServers,
    riEngineVersion,
    riFreeUntil,
    riInstanceCreateTime,
    riKmsKeyId,
    riMultiAZ,
    riPendingModifiedValues,
    riPreferredMaintenanceWindow,
    riPubliclyAccessible,
    riReplicationInstanceArn,
    riReplicationInstanceClass,
    riReplicationInstanceIdentifier,
    riReplicationInstancePrivateIpAddress,
    riReplicationInstancePrivateIpAddresses,
    riReplicationInstancePublicIpAddress,
    riReplicationInstancePublicIpAddresses,
    riReplicationInstanceStatus,
    riReplicationSubnetGroup,
    riSecondaryAvailabilityZone,
    riVpcSecurityGroups,
  )
where

import qualified Network.AWS.DMS.Types.ReplicationPendingModifiedValues as Types
import qualified Network.AWS.DMS.Types.ReplicationSubnetGroup as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.DMS.Types.VpcSecurityGroupMembership as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines a replication instance.
--
-- /See:/ 'mkReplicationInstance' smart constructor.
data ReplicationInstance = ReplicationInstance'
  { -- | The amount of storage (in gigabytes) that is allocated for the replication instance.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | Boolean value indicating if minor version upgrades will be automatically applied to the instance.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The Availability Zone for the instance.
    availabilityZone :: Core.Maybe Types.String,
    -- | The DNS name servers supported for the replication instance to access your on-premise source or target database.
    dnsNameServers :: Core.Maybe Types.String,
    -- | The engine version number of the replication instance.
    --
    -- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
    -- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
    engineVersion :: Core.Maybe Types.String,
    -- | The expiration date of the free replication instance that is part of the Free DMS program.
    freeUntil :: Core.Maybe Core.NominalDiffTime,
    -- | The time the replication instance was created.
    instanceCreateTime :: Core.Maybe Core.NominalDiffTime,
    -- | An AWS KMS key identifier that is used to encrypt the data on the replication instance.
    --
    -- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
    -- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    kmsKeyId :: Core.Maybe Types.String,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
    multiAZ :: Core.Maybe Core.Bool,
    -- | The pending modification values.
    pendingModifiedValues :: Core.Maybe Types.ReplicationPendingModifiedValues,
    -- | The maintenance window times for the replication instance. Any pending upgrades to the replication instance are performed during this time.
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Maybe Types.String,
    -- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. It is a required parameter, although a defualt value is pre-selected in the DMS console.
    --
    -- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
    replicationInstanceClass :: Core.Maybe Types.String,
    -- | The replication instance identifier is a required parameter. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain 1-63 alphanumeric characters or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens.
    --
    --
    -- Example: @myrepinstance@
    replicationInstanceIdentifier :: Core.Maybe Types.String,
    -- | The private IP address of the replication instance.
    replicationInstancePrivateIpAddress :: Core.Maybe Types.String,
    -- | One or more private IP addresses for the replication instance.
    replicationInstancePrivateIpAddresses :: Core.Maybe [Types.String],
    -- | The public IP address of the replication instance.
    replicationInstancePublicIpAddress :: Core.Maybe Types.String,
    -- | One or more public IP addresses for the replication instance.
    replicationInstancePublicIpAddresses :: Core.Maybe [Types.String],
    -- | The status of the replication instance. The possible return values include:
    --
    --
    --     * @"available"@
    --
    --
    --     * @"creating"@
    --
    --
    --     * @"deleted"@
    --
    --
    --     * @"deleting"@
    --
    --
    --     * @"failed"@
    --
    --
    --     * @"modifying"@
    --
    --
    --     * @"upgrading"@
    --
    --
    --     * @"rebooting"@
    --
    --
    --     * @"resetting-master-credentials"@
    --
    --
    --     * @"storage-full"@
    --
    --
    --     * @"incompatible-credentials"@
    --
    --
    --     * @"incompatible-network"@
    --
    --
    --     * @"maintenance"@
    replicationInstanceStatus :: Core.Maybe Types.String,
    -- | The subnet group for the replication instance.
    replicationSubnetGroup :: Core.Maybe Types.ReplicationSubnetGroup,
    -- | The Availability Zone of the standby replication instance in a Multi-AZ deployment.
    secondaryAvailabilityZone :: Core.Maybe Types.String,
    -- | The VPC security group for the instance.
    vpcSecurityGroups :: Core.Maybe [Types.VpcSecurityGroupMembership]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReplicationInstance' value with any optional fields omitted.
mkReplicationInstance ::
  ReplicationInstance
mkReplicationInstance =
  ReplicationInstance'
    { allocatedStorage = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      availabilityZone = Core.Nothing,
      dnsNameServers = Core.Nothing,
      engineVersion = Core.Nothing,
      freeUntil = Core.Nothing,
      instanceCreateTime = Core.Nothing,
      kmsKeyId = Core.Nothing,
      multiAZ = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      replicationInstanceArn = Core.Nothing,
      replicationInstanceClass = Core.Nothing,
      replicationInstanceIdentifier = Core.Nothing,
      replicationInstancePrivateIpAddress = Core.Nothing,
      replicationInstancePrivateIpAddresses = Core.Nothing,
      replicationInstancePublicIpAddress = Core.Nothing,
      replicationInstancePublicIpAddresses = Core.Nothing,
      replicationInstanceStatus = Core.Nothing,
      replicationSubnetGroup = Core.Nothing,
      secondaryAvailabilityZone = Core.Nothing,
      vpcSecurityGroups = Core.Nothing
    }

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAllocatedStorage :: Lens.Lens' ReplicationInstance (Core.Maybe Core.Int)
riAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED riAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Boolean value indicating if minor version upgrades will be automatically applied to the instance.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAutoMinorVersionUpgrade :: Lens.Lens' ReplicationInstance (Core.Maybe Core.Bool)
riAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED riAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The Availability Zone for the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAvailabilityZone :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED riAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The DNS name servers supported for the replication instance to access your on-premise source or target database.
--
-- /Note:/ Consider using 'dnsNameServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDnsNameServers :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riDnsNameServers = Lens.field @"dnsNameServers"
{-# DEPRECATED riDnsNameServers "Use generic-lens or generic-optics with 'dnsNameServers' instead." #-}

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEngineVersion :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED riEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The expiration date of the free replication instance that is part of the Free DMS program.
--
-- /Note:/ Consider using 'freeUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riFreeUntil :: Lens.Lens' ReplicationInstance (Core.Maybe Core.NominalDiffTime)
riFreeUntil = Lens.field @"freeUntil"
{-# DEPRECATED riFreeUntil "Use generic-lens or generic-optics with 'freeUntil' instead." #-}

-- | The time the replication instance was created.
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceCreateTime :: Lens.Lens' ReplicationInstance (Core.Maybe Core.NominalDiffTime)
riInstanceCreateTime = Lens.field @"instanceCreateTime"
{-# DEPRECATED riInstanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead." #-}

-- | An AWS KMS key identifier that is used to encrypt the data on the replication instance.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riKmsKeyId :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED riKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riMultiAZ :: Lens.Lens' ReplicationInstance (Core.Maybe Core.Bool)
riMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED riMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The pending modification values.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPendingModifiedValues :: Lens.Lens' ReplicationInstance (Core.Maybe Types.ReplicationPendingModifiedValues)
riPendingModifiedValues = Lens.field @"pendingModifiedValues"
{-# DEPRECATED riPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The maintenance window times for the replication instance. Any pending upgrades to the replication instance are performed during this time.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPreferredMaintenanceWindow :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED riPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPubliclyAccessible :: Lens.Lens' ReplicationInstance (Core.Maybe Core.Bool)
riPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# DEPRECATED riPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstanceArn :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED riReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. It is a required parameter, although a defualt value is pre-selected in the DMS console.
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstanceClass :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riReplicationInstanceClass = Lens.field @"replicationInstanceClass"
{-# DEPRECATED riReplicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead." #-}

-- | The replication instance identifier is a required parameter. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain 1-63 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @myrepinstance@
--
-- /Note:/ Consider using 'replicationInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstanceIdentifier :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riReplicationInstanceIdentifier = Lens.field @"replicationInstanceIdentifier"
{-# DEPRECATED riReplicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead." #-}

-- | The private IP address of the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePrivateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePrivateIpAddress :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riReplicationInstancePrivateIpAddress = Lens.field @"replicationInstancePrivateIpAddress"
{-# DEPRECATED riReplicationInstancePrivateIpAddress "Use generic-lens or generic-optics with 'replicationInstancePrivateIpAddress' instead." #-}

-- | One or more private IP addresses for the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePrivateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePrivateIpAddresses :: Lens.Lens' ReplicationInstance (Core.Maybe [Types.String])
riReplicationInstancePrivateIpAddresses = Lens.field @"replicationInstancePrivateIpAddresses"
{-# DEPRECATED riReplicationInstancePrivateIpAddresses "Use generic-lens or generic-optics with 'replicationInstancePrivateIpAddresses' instead." #-}

-- | The public IP address of the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePublicIpAddress :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riReplicationInstancePublicIpAddress = Lens.field @"replicationInstancePublicIpAddress"
{-# DEPRECATED riReplicationInstancePublicIpAddress "Use generic-lens or generic-optics with 'replicationInstancePublicIpAddress' instead." #-}

-- | One or more public IP addresses for the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePublicIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePublicIpAddresses :: Lens.Lens' ReplicationInstance (Core.Maybe [Types.String])
riReplicationInstancePublicIpAddresses = Lens.field @"replicationInstancePublicIpAddresses"
{-# DEPRECATED riReplicationInstancePublicIpAddresses "Use generic-lens or generic-optics with 'replicationInstancePublicIpAddresses' instead." #-}

-- | The status of the replication instance. The possible return values include:
--
--
--     * @"available"@
--
--
--     * @"creating"@
--
--
--     * @"deleted"@
--
--
--     * @"deleting"@
--
--
--     * @"failed"@
--
--
--     * @"modifying"@
--
--
--     * @"upgrading"@
--
--
--     * @"rebooting"@
--
--
--     * @"resetting-master-credentials"@
--
--
--     * @"storage-full"@
--
--
--     * @"incompatible-credentials"@
--
--
--     * @"incompatible-network"@
--
--
--     * @"maintenance"@
--
--
--
-- /Note:/ Consider using 'replicationInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstanceStatus :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riReplicationInstanceStatus = Lens.field @"replicationInstanceStatus"
{-# DEPRECATED riReplicationInstanceStatus "Use generic-lens or generic-optics with 'replicationInstanceStatus' instead." #-}

-- | The subnet group for the replication instance.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationSubnetGroup :: Lens.Lens' ReplicationInstance (Core.Maybe Types.ReplicationSubnetGroup)
riReplicationSubnetGroup = Lens.field @"replicationSubnetGroup"
{-# DEPRECATED riReplicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead." #-}

-- | The Availability Zone of the standby replication instance in a Multi-AZ deployment.
--
-- /Note:/ Consider using 'secondaryAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riSecondaryAvailabilityZone :: Lens.Lens' ReplicationInstance (Core.Maybe Types.String)
riSecondaryAvailabilityZone = Lens.field @"secondaryAvailabilityZone"
{-# DEPRECATED riSecondaryAvailabilityZone "Use generic-lens or generic-optics with 'secondaryAvailabilityZone' instead." #-}

-- | The VPC security group for the instance.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riVpcSecurityGroups :: Lens.Lens' ReplicationInstance (Core.Maybe [Types.VpcSecurityGroupMembership])
riVpcSecurityGroups = Lens.field @"vpcSecurityGroups"
{-# DEPRECATED riVpcSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead." #-}

instance Core.FromJSON ReplicationInstance where
  parseJSON =
    Core.withObject "ReplicationInstance" Core.$
      \x ->
        ReplicationInstance'
          Core.<$> (x Core..:? "AllocatedStorage")
          Core.<*> (x Core..:? "AutoMinorVersionUpgrade")
          Core.<*> (x Core..:? "AvailabilityZone")
          Core.<*> (x Core..:? "DnsNameServers")
          Core.<*> (x Core..:? "EngineVersion")
          Core.<*> (x Core..:? "FreeUntil")
          Core.<*> (x Core..:? "InstanceCreateTime")
          Core.<*> (x Core..:? "KmsKeyId")
          Core.<*> (x Core..:? "MultiAZ")
          Core.<*> (x Core..:? "PendingModifiedValues")
          Core.<*> (x Core..:? "PreferredMaintenanceWindow")
          Core.<*> (x Core..:? "PubliclyAccessible")
          Core.<*> (x Core..:? "ReplicationInstanceArn")
          Core.<*> (x Core..:? "ReplicationInstanceClass")
          Core.<*> (x Core..:? "ReplicationInstanceIdentifier")
          Core.<*> (x Core..:? "ReplicationInstancePrivateIpAddress")
          Core.<*> (x Core..:? "ReplicationInstancePrivateIpAddresses")
          Core.<*> (x Core..:? "ReplicationInstancePublicIpAddress")
          Core.<*> (x Core..:? "ReplicationInstancePublicIpAddresses")
          Core.<*> (x Core..:? "ReplicationInstanceStatus")
          Core.<*> (x Core..:? "ReplicationSubnetGroup")
          Core.<*> (x Core..:? "SecondaryAvailabilityZone")
          Core.<*> (x Core..:? "VpcSecurityGroups")
