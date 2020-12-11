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
    riEngineVersion,
    riPubliclyAccessible,
    riAutoMinorVersionUpgrade,
    riReplicationInstancePublicIPAddresses,
    riReplicationSubnetGroup,
    riInstanceCreateTime,
    riFreeUntil,
    riReplicationInstanceStatus,
    riReplicationInstancePrivateIPAddresses,
    riPreferredMaintenanceWindow,
    riReplicationInstancePrivateIPAddress,
    riKMSKeyId,
    riAvailabilityZone,
    riVPCSecurityGroups,
    riMultiAZ,
    riSecondaryAvailabilityZone,
    riReplicationInstanceARN,
    riAllocatedStorage,
    riDNSNameServers,
    riReplicationInstancePublicIPAddress,
    riReplicationInstanceClass,
    riReplicationInstanceIdentifier,
    riPendingModifiedValues,
  )
where

import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
import Network.AWS.DMS.Types.ReplicationSubnetGroup
import Network.AWS.DMS.Types.VPCSecurityGroupMembership
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines a replication instance.
--
-- /See:/ 'mkReplicationInstance' smart constructor.
data ReplicationInstance = ReplicationInstance'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    replicationInstancePublicIPAddresses ::
      Lude.Maybe [Lude.Text],
    replicationSubnetGroup ::
      Lude.Maybe ReplicationSubnetGroup,
    instanceCreateTime :: Lude.Maybe Lude.Timestamp,
    freeUntil :: Lude.Maybe Lude.Timestamp,
    replicationInstanceStatus :: Lude.Maybe Lude.Text,
    replicationInstancePrivateIPAddresses ::
      Lude.Maybe [Lude.Text],
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    replicationInstancePrivateIPAddress ::
      Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    vpcSecurityGroups ::
      Lude.Maybe [VPCSecurityGroupMembership],
    multiAZ :: Lude.Maybe Lude.Bool,
    secondaryAvailabilityZone :: Lude.Maybe Lude.Text,
    replicationInstanceARN :: Lude.Maybe Lude.Text,
    allocatedStorage :: Lude.Maybe Lude.Int,
    dnsNameServers :: Lude.Maybe Lude.Text,
    replicationInstancePublicIPAddress ::
      Lude.Maybe Lude.Text,
    replicationInstanceClass :: Lude.Maybe Lude.Text,
    replicationInstanceIdentifier ::
      Lude.Maybe Lude.Text,
    pendingModifiedValues ::
      Lude.Maybe ReplicationPendingModifiedValues
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationInstance' with the minimum fields required to make a request.
--
-- * 'allocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
-- * 'autoMinorVersionUpgrade' - Boolean value indicating if minor version upgrades will be automatically applied to the instance.
-- * 'availabilityZone' - The Availability Zone for the instance.
-- * 'dnsNameServers' - The DNS name servers supported for the replication instance to access your on-premise source or target database.
-- * 'engineVersion' - The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
-- * 'freeUntil' - The expiration date of the free replication instance that is part of the Free DMS program.
-- * 'instanceCreateTime' - The time the replication instance was created.
-- * 'kmsKeyId' - An AWS KMS key identifier that is used to encrypt the data on the replication instance.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
-- * 'pendingModifiedValues' - The pending modification values.
-- * 'preferredMaintenanceWindow' - The maintenance window times for the replication instance. Any pending upgrades to the replication instance are performed during this time.
-- * 'publiclyAccessible' - Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
-- * 'replicationInstanceClass' - The compute and memory capacity of the replication instance as defined for the specified replication instance class. It is a required parameter, although a defualt value is pre-selected in the DMS console.
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
-- * 'replicationInstanceIdentifier' - The replication instance identifier is a required parameter. This parameter is stored as a lowercase string.
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
-- * 'replicationInstancePrivateIPAddress' - The private IP address of the replication instance.
-- * 'replicationInstancePrivateIPAddresses' - One or more private IP addresses for the replication instance.
-- * 'replicationInstancePublicIPAddress' - The public IP address of the replication instance.
-- * 'replicationInstancePublicIPAddresses' - One or more public IP addresses for the replication instance.
-- * 'replicationInstanceStatus' - The status of the replication instance. The possible return values include:
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
-- * 'replicationSubnetGroup' - The subnet group for the replication instance.
-- * 'secondaryAvailabilityZone' - The Availability Zone of the standby replication instance in a Multi-AZ deployment.
-- * 'vpcSecurityGroups' - The VPC security group for the instance.
mkReplicationInstance ::
  ReplicationInstance
mkReplicationInstance =
  ReplicationInstance'
    { engineVersion = Lude.Nothing,
      publiclyAccessible = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      replicationInstancePublicIPAddresses = Lude.Nothing,
      replicationSubnetGroup = Lude.Nothing,
      instanceCreateTime = Lude.Nothing,
      freeUntil = Lude.Nothing,
      replicationInstanceStatus = Lude.Nothing,
      replicationInstancePrivateIPAddresses = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      replicationInstancePrivateIPAddress = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      vpcSecurityGroups = Lude.Nothing,
      multiAZ = Lude.Nothing,
      secondaryAvailabilityZone = Lude.Nothing,
      replicationInstanceARN = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      dnsNameServers = Lude.Nothing,
      replicationInstancePublicIPAddress = Lude.Nothing,
      replicationInstanceClass = Lude.Nothing,
      replicationInstanceIdentifier = Lude.Nothing,
      pendingModifiedValues = Lude.Nothing
    }

-- | The engine version number of the replication instance.
--
-- If an engine version number is not specified when a replication instance is created, the default is the latest engine version available.
-- When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEngineVersion :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riEngineVersion = Lens.lens (engineVersion :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ReplicationInstance)
{-# DEPRECATED riEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPubliclyAccessible :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Bool)
riPubliclyAccessible = Lens.lens (publiclyAccessible :: ReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: ReplicationInstance)
{-# DEPRECATED riPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | Boolean value indicating if minor version upgrades will be automatically applied to the instance.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAutoMinorVersionUpgrade :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Bool)
riAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: ReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: ReplicationInstance)
{-# DEPRECATED riAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | One or more public IP addresses for the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePublicIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePublicIPAddresses :: Lens.Lens' ReplicationInstance (Lude.Maybe [Lude.Text])
riReplicationInstancePublicIPAddresses = Lens.lens (replicationInstancePublicIPAddresses :: ReplicationInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {replicationInstancePublicIPAddresses = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationInstancePublicIPAddresses "Use generic-lens or generic-optics with 'replicationInstancePublicIPAddresses' instead." #-}

-- | The subnet group for the replication instance.
--
-- /Note:/ Consider using 'replicationSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationSubnetGroup :: Lens.Lens' ReplicationInstance (Lude.Maybe ReplicationSubnetGroup)
riReplicationSubnetGroup = Lens.lens (replicationSubnetGroup :: ReplicationInstance -> Lude.Maybe ReplicationSubnetGroup) (\s a -> s {replicationSubnetGroup = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationSubnetGroup "Use generic-lens or generic-optics with 'replicationSubnetGroup' instead." #-}

-- | The time the replication instance was created.
--
-- /Note:/ Consider using 'instanceCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceCreateTime :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Timestamp)
riInstanceCreateTime = Lens.lens (instanceCreateTime :: ReplicationInstance -> Lude.Maybe Lude.Timestamp) (\s a -> s {instanceCreateTime = a} :: ReplicationInstance)
{-# DEPRECATED riInstanceCreateTime "Use generic-lens or generic-optics with 'instanceCreateTime' instead." #-}

-- | The expiration date of the free replication instance that is part of the Free DMS program.
--
-- /Note:/ Consider using 'freeUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riFreeUntil :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Timestamp)
riFreeUntil = Lens.lens (freeUntil :: ReplicationInstance -> Lude.Maybe Lude.Timestamp) (\s a -> s {freeUntil = a} :: ReplicationInstance)
{-# DEPRECATED riFreeUntil "Use generic-lens or generic-optics with 'freeUntil' instead." #-}

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
riReplicationInstanceStatus :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riReplicationInstanceStatus = Lens.lens (replicationInstanceStatus :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceStatus = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationInstanceStatus "Use generic-lens or generic-optics with 'replicationInstanceStatus' instead." #-}

-- | One or more private IP addresses for the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePrivateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePrivateIPAddresses :: Lens.Lens' ReplicationInstance (Lude.Maybe [Lude.Text])
riReplicationInstancePrivateIPAddresses = Lens.lens (replicationInstancePrivateIPAddresses :: ReplicationInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {replicationInstancePrivateIPAddresses = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationInstancePrivateIPAddresses "Use generic-lens or generic-optics with 'replicationInstancePrivateIPAddresses' instead." #-}

-- | The maintenance window times for the replication instance. Any pending upgrades to the replication instance are performed during this time.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPreferredMaintenanceWindow :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: ReplicationInstance)
{-# DEPRECATED riPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The private IP address of the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePrivateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePrivateIPAddress :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riReplicationInstancePrivateIPAddress = Lens.lens (replicationInstancePrivateIPAddress :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstancePrivateIPAddress = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationInstancePrivateIPAddress "Use generic-lens or generic-optics with 'replicationInstancePrivateIPAddress' instead." #-}

-- | An AWS KMS key identifier that is used to encrypt the data on the replication instance.
--
-- If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key.
-- AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riKMSKeyId :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riKMSKeyId = Lens.lens (kmsKeyId :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ReplicationInstance)
{-# DEPRECATED riKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Availability Zone for the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAvailabilityZone :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riAvailabilityZone = Lens.lens (availabilityZone :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ReplicationInstance)
{-# DEPRECATED riAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The VPC security group for the instance.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riVPCSecurityGroups :: Lens.Lens' ReplicationInstance (Lude.Maybe [VPCSecurityGroupMembership])
riVPCSecurityGroups = Lens.lens (vpcSecurityGroups :: ReplicationInstance -> Lude.Maybe [VPCSecurityGroupMembership]) (\s a -> s {vpcSecurityGroups = a} :: ReplicationInstance)
{-# DEPRECATED riVPCSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead." #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riMultiAZ :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Bool)
riMultiAZ = Lens.lens (multiAZ :: ReplicationInstance -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: ReplicationInstance)
{-# DEPRECATED riMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The Availability Zone of the standby replication instance in a Multi-AZ deployment.
--
-- /Note:/ Consider using 'secondaryAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riSecondaryAvailabilityZone :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riSecondaryAvailabilityZone = Lens.lens (secondaryAvailabilityZone :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {secondaryAvailabilityZone = a} :: ReplicationInstance)
{-# DEPRECATED riSecondaryAvailabilityZone "Use generic-lens or generic-optics with 'secondaryAvailabilityZone' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstanceARN :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riReplicationInstanceARN = Lens.lens (replicationInstanceARN :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAllocatedStorage :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Int)
riAllocatedStorage = Lens.lens (allocatedStorage :: ReplicationInstance -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: ReplicationInstance)
{-# DEPRECATED riAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | The DNS name servers supported for the replication instance to access your on-premise source or target database.
--
-- /Note:/ Consider using 'dnsNameServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDNSNameServers :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riDNSNameServers = Lens.lens (dnsNameServers :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {dnsNameServers = a} :: ReplicationInstance)
{-# DEPRECATED riDNSNameServers "Use generic-lens or generic-optics with 'dnsNameServers' instead." #-}

-- | The public IP address of the replication instance.
--
-- /Note:/ Consider using 'replicationInstancePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstancePublicIPAddress :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riReplicationInstancePublicIPAddress = Lens.lens (replicationInstancePublicIPAddress :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstancePublicIPAddress = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationInstancePublicIPAddress "Use generic-lens or generic-optics with 'replicationInstancePublicIPAddress' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. It is a required parameter, although a defualt value is pre-selected in the DMS console.
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReplicationInstanceClass :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riReplicationInstanceClass = Lens.lens (replicationInstanceClass :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceClass = a} :: ReplicationInstance)
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
riReplicationInstanceIdentifier :: Lens.Lens' ReplicationInstance (Lude.Maybe Lude.Text)
riReplicationInstanceIdentifier = Lens.lens (replicationInstanceIdentifier :: ReplicationInstance -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceIdentifier = a} :: ReplicationInstance)
{-# DEPRECATED riReplicationInstanceIdentifier "Use generic-lens or generic-optics with 'replicationInstanceIdentifier' instead." #-}

-- | The pending modification values.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riPendingModifiedValues :: Lens.Lens' ReplicationInstance (Lude.Maybe ReplicationPendingModifiedValues)
riPendingModifiedValues = Lens.lens (pendingModifiedValues :: ReplicationInstance -> Lude.Maybe ReplicationPendingModifiedValues) (\s a -> s {pendingModifiedValues = a} :: ReplicationInstance)
{-# DEPRECATED riPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

instance Lude.FromJSON ReplicationInstance where
  parseJSON =
    Lude.withObject
      "ReplicationInstance"
      ( \x ->
          ReplicationInstance'
            Lude.<$> (x Lude..:? "EngineVersion")
            Lude.<*> (x Lude..:? "PubliclyAccessible")
            Lude.<*> (x Lude..:? "AutoMinorVersionUpgrade")
            Lude.<*> ( x Lude..:? "ReplicationInstancePublicIpAddresses"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "ReplicationSubnetGroup")
            Lude.<*> (x Lude..:? "InstanceCreateTime")
            Lude.<*> (x Lude..:? "FreeUntil")
            Lude.<*> (x Lude..:? "ReplicationInstanceStatus")
            Lude.<*> ( x Lude..:? "ReplicationInstancePrivateIpAddresses"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "PreferredMaintenanceWindow")
            Lude.<*> (x Lude..:? "ReplicationInstancePrivateIpAddress")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "VpcSecurityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MultiAZ")
            Lude.<*> (x Lude..:? "SecondaryAvailabilityZone")
            Lude.<*> (x Lude..:? "ReplicationInstanceArn")
            Lude.<*> (x Lude..:? "AllocatedStorage")
            Lude.<*> (x Lude..:? "DnsNameServers")
            Lude.<*> (x Lude..:? "ReplicationInstancePublicIpAddress")
            Lude.<*> (x Lude..:? "ReplicationInstanceClass")
            Lude.<*> (x Lude..:? "ReplicationInstanceIdentifier")
            Lude.<*> (x Lude..:? "PendingModifiedValues")
      )
