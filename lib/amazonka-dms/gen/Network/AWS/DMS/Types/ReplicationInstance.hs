{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationInstance where

import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
import Network.AWS.DMS.Types.ReplicationSubnetGroup
import Network.AWS.DMS.Types.VPCSecurityGroupMembership
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines a replication instance.
--
--
--
-- /See:/ 'replicationInstance' smart constructor.
data ReplicationInstance = ReplicationInstance'
  { _riEngineVersion ::
      !(Maybe Text),
    _riPubliclyAccessible :: !(Maybe Bool),
    _riAutoMinorVersionUpgrade :: !(Maybe Bool),
    _riReplicationInstancePublicIPAddresses ::
      !(Maybe [Text]),
    _riReplicationSubnetGroup ::
      !(Maybe ReplicationSubnetGroup),
    _riInstanceCreateTime :: !(Maybe POSIX),
    _riFreeUntil :: !(Maybe POSIX),
    _riReplicationInstanceStatus :: !(Maybe Text),
    _riReplicationInstancePrivateIPAddresses ::
      !(Maybe [Text]),
    _riPreferredMaintenanceWindow :: !(Maybe Text),
    _riReplicationInstancePrivateIPAddress ::
      !(Maybe Text),
    _riKMSKeyId :: !(Maybe Text),
    _riAvailabilityZone :: !(Maybe Text),
    _riVPCSecurityGroups ::
      !(Maybe [VPCSecurityGroupMembership]),
    _riMultiAZ :: !(Maybe Bool),
    _riSecondaryAvailabilityZone :: !(Maybe Text),
    _riReplicationInstanceARN :: !(Maybe Text),
    _riAllocatedStorage :: !(Maybe Int),
    _riDNSNameServers :: !(Maybe Text),
    _riReplicationInstancePublicIPAddress ::
      !(Maybe Text),
    _riReplicationInstanceClass :: !(Maybe Text),
    _riReplicationInstanceIdentifier :: !(Maybe Text),
    _riPendingModifiedValues ::
      !(Maybe ReplicationPendingModifiedValues)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riEngineVersion' - The engine version number of the replication instance. If an engine version number is not specified when a replication instance is created, the default is the latest engine version available. When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
--
-- * 'riPubliclyAccessible' - Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
--
-- * 'riAutoMinorVersionUpgrade' - Boolean value indicating if minor version upgrades will be automatically applied to the instance.
--
-- * 'riReplicationInstancePublicIPAddresses' - One or more public IP addresses for the replication instance.
--
-- * 'riReplicationSubnetGroup' - The subnet group for the replication instance.
--
-- * 'riInstanceCreateTime' - The time the replication instance was created.
--
-- * 'riFreeUntil' - The expiration date of the free replication instance that is part of the Free DMS program.
--
-- * 'riReplicationInstanceStatus' - The status of the replication instance. The possible return values include:     * @"available"@      * @"creating"@      * @"deleted"@      * @"deleting"@      * @"failed"@      * @"modifying"@      * @"upgrading"@      * @"rebooting"@      * @"resetting-master-credentials"@      * @"storage-full"@      * @"incompatible-credentials"@      * @"incompatible-network"@      * @"maintenance"@
--
-- * 'riReplicationInstancePrivateIPAddresses' - One or more private IP addresses for the replication instance.
--
-- * 'riPreferredMaintenanceWindow' - The maintenance window times for the replication instance. Any pending upgrades to the replication instance are performed during this time.
--
-- * 'riReplicationInstancePrivateIPAddress' - The private IP address of the replication instance.
--
-- * 'riKMSKeyId' - An AWS KMS key identifier that is used to encrypt the data on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'riAvailabilityZone' - The Availability Zone for the instance.
--
-- * 'riVPCSecurityGroups' - The VPC security group for the instance.
--
-- * 'riMultiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- * 'riSecondaryAvailabilityZone' - The Availability Zone of the standby replication instance in a Multi-AZ deployment.
--
-- * 'riReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'riAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'riDNSNameServers' - The DNS name servers supported for the replication instance to access your on-premise source or target database.
--
-- * 'riReplicationInstancePublicIPAddress' - The public IP address of the replication instance.
--
-- * 'riReplicationInstanceClass' - The compute and memory capacity of the replication instance as defined for the specified replication instance class. It is a required parameter, although a defualt value is pre-selected in the DMS console. For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- * 'riReplicationInstanceIdentifier' - The replication instance identifier is a required parameter. This parameter is stored as a lowercase string. Constraints:     * Must contain 1-63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
--
-- * 'riPendingModifiedValues' - The pending modification values.
replicationInstance ::
  ReplicationInstance
replicationInstance =
  ReplicationInstance'
    { _riEngineVersion = Nothing,
      _riPubliclyAccessible = Nothing,
      _riAutoMinorVersionUpgrade = Nothing,
      _riReplicationInstancePublicIPAddresses = Nothing,
      _riReplicationSubnetGroup = Nothing,
      _riInstanceCreateTime = Nothing,
      _riFreeUntil = Nothing,
      _riReplicationInstanceStatus = Nothing,
      _riReplicationInstancePrivateIPAddresses = Nothing,
      _riPreferredMaintenanceWindow = Nothing,
      _riReplicationInstancePrivateIPAddress = Nothing,
      _riKMSKeyId = Nothing,
      _riAvailabilityZone = Nothing,
      _riVPCSecurityGroups = Nothing,
      _riMultiAZ = Nothing,
      _riSecondaryAvailabilityZone = Nothing,
      _riReplicationInstanceARN = Nothing,
      _riAllocatedStorage = Nothing,
      _riDNSNameServers = Nothing,
      _riReplicationInstancePublicIPAddress = Nothing,
      _riReplicationInstanceClass = Nothing,
      _riReplicationInstanceIdentifier = Nothing,
      _riPendingModifiedValues = Nothing
    }

-- | The engine version number of the replication instance. If an engine version number is not specified when a replication instance is created, the default is the latest engine version available. When modifying a major engine version of an instance, also set @AllowMajorVersionUpgrade@ to @true@ .
riEngineVersion :: Lens' ReplicationInstance (Maybe Text)
riEngineVersion = lens _riEngineVersion (\s a -> s {_riEngineVersion = a})

-- | Specifies the accessibility options for the replication instance. A value of @true@ represents an instance with a public IP address. A value of @false@ represents an instance with a private IP address. The default value is @true@ .
riPubliclyAccessible :: Lens' ReplicationInstance (Maybe Bool)
riPubliclyAccessible = lens _riPubliclyAccessible (\s a -> s {_riPubliclyAccessible = a})

-- | Boolean value indicating if minor version upgrades will be automatically applied to the instance.
riAutoMinorVersionUpgrade :: Lens' ReplicationInstance (Maybe Bool)
riAutoMinorVersionUpgrade = lens _riAutoMinorVersionUpgrade (\s a -> s {_riAutoMinorVersionUpgrade = a})

-- | One or more public IP addresses for the replication instance.
riReplicationInstancePublicIPAddresses :: Lens' ReplicationInstance [Text]
riReplicationInstancePublicIPAddresses = lens _riReplicationInstancePublicIPAddresses (\s a -> s {_riReplicationInstancePublicIPAddresses = a}) . _Default . _Coerce

-- | The subnet group for the replication instance.
riReplicationSubnetGroup :: Lens' ReplicationInstance (Maybe ReplicationSubnetGroup)
riReplicationSubnetGroup = lens _riReplicationSubnetGroup (\s a -> s {_riReplicationSubnetGroup = a})

-- | The time the replication instance was created.
riInstanceCreateTime :: Lens' ReplicationInstance (Maybe UTCTime)
riInstanceCreateTime = lens _riInstanceCreateTime (\s a -> s {_riInstanceCreateTime = a}) . mapping _Time

-- | The expiration date of the free replication instance that is part of the Free DMS program.
riFreeUntil :: Lens' ReplicationInstance (Maybe UTCTime)
riFreeUntil = lens _riFreeUntil (\s a -> s {_riFreeUntil = a}) . mapping _Time

-- | The status of the replication instance. The possible return values include:     * @"available"@      * @"creating"@      * @"deleted"@      * @"deleting"@      * @"failed"@      * @"modifying"@      * @"upgrading"@      * @"rebooting"@      * @"resetting-master-credentials"@      * @"storage-full"@      * @"incompatible-credentials"@      * @"incompatible-network"@      * @"maintenance"@
riReplicationInstanceStatus :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceStatus = lens _riReplicationInstanceStatus (\s a -> s {_riReplicationInstanceStatus = a})

-- | One or more private IP addresses for the replication instance.
riReplicationInstancePrivateIPAddresses :: Lens' ReplicationInstance [Text]
riReplicationInstancePrivateIPAddresses = lens _riReplicationInstancePrivateIPAddresses (\s a -> s {_riReplicationInstancePrivateIPAddresses = a}) . _Default . _Coerce

-- | The maintenance window times for the replication instance. Any pending upgrades to the replication instance are performed during this time.
riPreferredMaintenanceWindow :: Lens' ReplicationInstance (Maybe Text)
riPreferredMaintenanceWindow = lens _riPreferredMaintenanceWindow (\s a -> s {_riPreferredMaintenanceWindow = a})

-- | The private IP address of the replication instance.
riReplicationInstancePrivateIPAddress :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstancePrivateIPAddress = lens _riReplicationInstancePrivateIPAddress (\s a -> s {_riReplicationInstancePrivateIPAddress = a})

-- | An AWS KMS key identifier that is used to encrypt the data on the replication instance. If you don't specify a value for the @KmsKeyId@ parameter, then AWS DMS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
riKMSKeyId :: Lens' ReplicationInstance (Maybe Text)
riKMSKeyId = lens _riKMSKeyId (\s a -> s {_riKMSKeyId = a})

-- | The Availability Zone for the instance.
riAvailabilityZone :: Lens' ReplicationInstance (Maybe Text)
riAvailabilityZone = lens _riAvailabilityZone (\s a -> s {_riAvailabilityZone = a})

-- | The VPC security group for the instance.
riVPCSecurityGroups :: Lens' ReplicationInstance [VPCSecurityGroupMembership]
riVPCSecurityGroups = lens _riVPCSecurityGroups (\s a -> s {_riVPCSecurityGroups = a}) . _Default . _Coerce

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
riMultiAZ :: Lens' ReplicationInstance (Maybe Bool)
riMultiAZ = lens _riMultiAZ (\s a -> s {_riMultiAZ = a})

-- | The Availability Zone of the standby replication instance in a Multi-AZ deployment.
riSecondaryAvailabilityZone :: Lens' ReplicationInstance (Maybe Text)
riSecondaryAvailabilityZone = lens _riSecondaryAvailabilityZone (\s a -> s {_riSecondaryAvailabilityZone = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
riReplicationInstanceARN :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceARN = lens _riReplicationInstanceARN (\s a -> s {_riReplicationInstanceARN = a})

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
riAllocatedStorage :: Lens' ReplicationInstance (Maybe Int)
riAllocatedStorage = lens _riAllocatedStorage (\s a -> s {_riAllocatedStorage = a})

-- | The DNS name servers supported for the replication instance to access your on-premise source or target database.
riDNSNameServers :: Lens' ReplicationInstance (Maybe Text)
riDNSNameServers = lens _riDNSNameServers (\s a -> s {_riDNSNameServers = a})

-- | The public IP address of the replication instance.
riReplicationInstancePublicIPAddress :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstancePublicIPAddress = lens _riReplicationInstancePublicIPAddress (\s a -> s {_riReplicationInstancePublicIPAddress = a})

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. It is a required parameter, although a defualt value is pre-selected in the DMS console. For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
riReplicationInstanceClass :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceClass = lens _riReplicationInstanceClass (\s a -> s {_riReplicationInstanceClass = a})

-- | The replication instance identifier is a required parameter. This parameter is stored as a lowercase string. Constraints:     * Must contain 1-63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @myrepinstance@
riReplicationInstanceIdentifier :: Lens' ReplicationInstance (Maybe Text)
riReplicationInstanceIdentifier = lens _riReplicationInstanceIdentifier (\s a -> s {_riReplicationInstanceIdentifier = a})

-- | The pending modification values.
riPendingModifiedValues :: Lens' ReplicationInstance (Maybe ReplicationPendingModifiedValues)
riPendingModifiedValues = lens _riPendingModifiedValues (\s a -> s {_riPendingModifiedValues = a})

instance FromJSON ReplicationInstance where
  parseJSON =
    withObject
      "ReplicationInstance"
      ( \x ->
          ReplicationInstance'
            <$> (x .:? "EngineVersion")
            <*> (x .:? "PubliclyAccessible")
            <*> (x .:? "AutoMinorVersionUpgrade")
            <*> (x .:? "ReplicationInstancePublicIpAddresses" .!= mempty)
            <*> (x .:? "ReplicationSubnetGroup")
            <*> (x .:? "InstanceCreateTime")
            <*> (x .:? "FreeUntil")
            <*> (x .:? "ReplicationInstanceStatus")
            <*> (x .:? "ReplicationInstancePrivateIpAddresses" .!= mempty)
            <*> (x .:? "PreferredMaintenanceWindow")
            <*> (x .:? "ReplicationInstancePrivateIpAddress")
            <*> (x .:? "KmsKeyId")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "VpcSecurityGroups" .!= mempty)
            <*> (x .:? "MultiAZ")
            <*> (x .:? "SecondaryAvailabilityZone")
            <*> (x .:? "ReplicationInstanceArn")
            <*> (x .:? "AllocatedStorage")
            <*> (x .:? "DnsNameServers")
            <*> (x .:? "ReplicationInstancePublicIpAddress")
            <*> (x .:? "ReplicationInstanceClass")
            <*> (x .:? "ReplicationInstanceIdentifier")
            <*> (x .:? "PendingModifiedValues")
      )

instance Hashable ReplicationInstance

instance NFData ReplicationInstance
