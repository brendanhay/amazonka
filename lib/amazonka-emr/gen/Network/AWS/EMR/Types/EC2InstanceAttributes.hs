{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EC2InstanceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.EC2InstanceAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
--
--
-- /See:/ 'ec2InstanceAttributes' smart constructor.
data EC2InstanceAttributes = EC2InstanceAttributes'
  { _eiaEC2KeyName ::
      !(Maybe Text),
    _eiaEmrManagedSlaveSecurityGroup ::
      !(Maybe Text),
    _eiaAdditionalSlaveSecurityGroups ::
      !(Maybe [Text]),
    _eiaRequestedEC2SubnetIds :: !(Maybe [Text]),
    _eiaAdditionalMasterSecurityGroups ::
      !(Maybe [Text]),
    _eiaIAMInstanceProfile :: !(Maybe Text),
    _eiaEmrManagedMasterSecurityGroup ::
      !(Maybe Text),
    _eiaEC2SubnetId :: !(Maybe Text),
    _eiaRequestedEC2AvailabilityZones ::
      !(Maybe [Text]),
    _eiaServiceAccessSecurityGroup :: !(Maybe Text),
    _eiaEC2AvailabilityZone :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2InstanceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiaEC2KeyName' - The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
--
-- * 'eiaEmrManagedSlaveSecurityGroup' - The identifier of the Amazon EC2 security group for the core and task nodes.
--
-- * 'eiaAdditionalSlaveSecurityGroups' - A list of additional Amazon EC2 security group IDs for the core and task nodes.
--
-- * 'eiaRequestedEC2SubnetIds' - Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and Region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- * 'eiaAdditionalMasterSecurityGroups' - A list of additional Amazon EC2 security group IDs for the master node.
--
-- * 'eiaIAMInstanceProfile' - The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
--
-- * 'eiaEmrManagedMasterSecurityGroup' - The identifier of the Amazon EC2 security group for the master node.
--
-- * 'eiaEC2SubnetId' - Set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, and your account supports EC2-Classic, the cluster launches in EC2-Classic.
--
-- * 'eiaRequestedEC2AvailabilityZones' - Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
--
-- * 'eiaServiceAccessSecurityGroup' - The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
--
-- * 'eiaEC2AvailabilityZone' - The Availability Zone in which the cluster will run.
ec2InstanceAttributes ::
  EC2InstanceAttributes
ec2InstanceAttributes =
  EC2InstanceAttributes'
    { _eiaEC2KeyName = Nothing,
      _eiaEmrManagedSlaveSecurityGroup = Nothing,
      _eiaAdditionalSlaveSecurityGroups = Nothing,
      _eiaRequestedEC2SubnetIds = Nothing,
      _eiaAdditionalMasterSecurityGroups = Nothing,
      _eiaIAMInstanceProfile = Nothing,
      _eiaEmrManagedMasterSecurityGroup = Nothing,
      _eiaEC2SubnetId = Nothing,
      _eiaRequestedEC2AvailabilityZones = Nothing,
      _eiaServiceAccessSecurityGroup = Nothing,
      _eiaEC2AvailabilityZone = Nothing
    }

-- | The name of the Amazon EC2 key pair to use when connecting with SSH into the master node as a user named "hadoop".
eiaEC2KeyName :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2KeyName = lens _eiaEC2KeyName (\s a -> s {_eiaEC2KeyName = a})

-- | The identifier of the Amazon EC2 security group for the core and task nodes.
eiaEmrManagedSlaveSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedSlaveSecurityGroup = lens _eiaEmrManagedSlaveSecurityGroup (\s a -> s {_eiaEmrManagedSlaveSecurityGroup = a})

-- | A list of additional Amazon EC2 security group IDs for the core and task nodes.
eiaAdditionalSlaveSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalSlaveSecurityGroups = lens _eiaAdditionalSlaveSecurityGroups (\s a -> s {_eiaAdditionalSlaveSecurityGroups = a}) . _Default . _Coerce

-- | Applies to clusters configured with the instance fleets option. Specifies the unique identifier of one or more Amazon EC2 subnets in which to launch EC2 cluster instances. Subnets must exist within the same VPC. Amazon EMR chooses the EC2 subnet with the best fit from among the list of @RequestedEc2SubnetIds@ , and then launches all cluster instances within that Subnet. If this value is not specified, and the account and Region support EC2-Classic networks, the cluster launches instances in the EC2-Classic network and uses @RequestedEc2AvailabilityZones@ instead of this setting. If EC2-Classic is not supported, and no Subnet is specified, Amazon EMR chooses the subnet for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
eiaRequestedEC2SubnetIds :: Lens' EC2InstanceAttributes [Text]
eiaRequestedEC2SubnetIds = lens _eiaRequestedEC2SubnetIds (\s a -> s {_eiaRequestedEC2SubnetIds = a}) . _Default . _Coerce

-- | A list of additional Amazon EC2 security group IDs for the master node.
eiaAdditionalMasterSecurityGroups :: Lens' EC2InstanceAttributes [Text]
eiaAdditionalMasterSecurityGroups = lens _eiaAdditionalMasterSecurityGroups (\s a -> s {_eiaAdditionalMasterSecurityGroups = a}) . _Default . _Coerce

-- | The IAM role that was specified when the cluster was launched. The EC2 instances of the cluster assume this role.
eiaIAMInstanceProfile :: Lens' EC2InstanceAttributes (Maybe Text)
eiaIAMInstanceProfile = lens _eiaIAMInstanceProfile (\s a -> s {_eiaIAMInstanceProfile = a})

-- | The identifier of the Amazon EC2 security group for the master node.
eiaEmrManagedMasterSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEmrManagedMasterSecurityGroup = lens _eiaEmrManagedMasterSecurityGroup (\s a -> s {_eiaEmrManagedMasterSecurityGroup = a})

-- | Set this parameter to the identifier of the Amazon VPC subnet where you want the cluster to launch. If you do not specify this value, and your account supports EC2-Classic, the cluster launches in EC2-Classic.
eiaEC2SubnetId :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2SubnetId = lens _eiaEC2SubnetId (\s a -> s {_eiaEC2SubnetId = a})

-- | Applies to clusters configured with the instance fleets option. Specifies one or more Availability Zones in which to launch EC2 cluster instances when the EC2-Classic network configuration is supported. Amazon EMR chooses the Availability Zone with the best fit from among the list of @RequestedEc2AvailabilityZones@ , and then launches all cluster instances within that Availability Zone. If you do not specify this value, Amazon EMR chooses the Availability Zone for you. @RequestedEc2SubnetIDs@ and @RequestedEc2AvailabilityZones@ cannot be specified together.
eiaRequestedEC2AvailabilityZones :: Lens' EC2InstanceAttributes [Text]
eiaRequestedEC2AvailabilityZones = lens _eiaRequestedEC2AvailabilityZones (\s a -> s {_eiaRequestedEC2AvailabilityZones = a}) . _Default . _Coerce

-- | The identifier of the Amazon EC2 security group for the Amazon EMR service to access clusters in VPC private subnets.
eiaServiceAccessSecurityGroup :: Lens' EC2InstanceAttributes (Maybe Text)
eiaServiceAccessSecurityGroup = lens _eiaServiceAccessSecurityGroup (\s a -> s {_eiaServiceAccessSecurityGroup = a})

-- | The Availability Zone in which the cluster will run.
eiaEC2AvailabilityZone :: Lens' EC2InstanceAttributes (Maybe Text)
eiaEC2AvailabilityZone = lens _eiaEC2AvailabilityZone (\s a -> s {_eiaEC2AvailabilityZone = a})

instance FromJSON EC2InstanceAttributes where
  parseJSON =
    withObject
      "EC2InstanceAttributes"
      ( \x ->
          EC2InstanceAttributes'
            <$> (x .:? "Ec2KeyName")
            <*> (x .:? "EmrManagedSlaveSecurityGroup")
            <*> (x .:? "AdditionalSlaveSecurityGroups" .!= mempty)
            <*> (x .:? "RequestedEc2SubnetIds" .!= mempty)
            <*> (x .:? "AdditionalMasterSecurityGroups" .!= mempty)
            <*> (x .:? "IamInstanceProfile")
            <*> (x .:? "EmrManagedMasterSecurityGroup")
            <*> (x .:? "Ec2SubnetId")
            <*> (x .:? "RequestedEc2AvailabilityZones" .!= mempty)
            <*> (x .:? "ServiceAccessSecurityGroup")
            <*> (x .:? "Ec2AvailabilityZone")
      )

instance Hashable EC2InstanceAttributes

instance NFData EC2InstanceAttributes
