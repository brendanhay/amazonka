{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Instance where

import Network.AWS.EMR.Types.EBSVolume
import Network.AWS.EMR.Types.InstanceStatus
import Network.AWS.EMR.Types.MarketType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an EC2 instance provisioned as part of cluster.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iStatus :: !(Maybe InstanceStatus),
    _iPublicDNSName :: !(Maybe Text),
    _iEBSVolumes :: !(Maybe [EBSVolume]),
    _iEC2InstanceId :: !(Maybe Text),
    _iInstanceType :: !(Maybe Text),
    _iMarket :: !(Maybe MarketType),
    _iPrivateIPAddress :: !(Maybe Text),
    _iInstanceFleetId :: !(Maybe Text),
    _iId :: !(Maybe Text),
    _iInstanceGroupId :: !(Maybe Text),
    _iPrivateDNSName :: !(Maybe Text),
    _iPublicIPAddress :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iStatus' - The current status of the instance.
--
-- * 'iPublicDNSName' - The public DNS name of the instance.
--
-- * 'iEBSVolumes' - The list of EBS volumes that are attached to this instance.
--
-- * 'iEC2InstanceId' - The unique identifier of the instance in Amazon EC2.
--
-- * 'iInstanceType' - The EC2 instance type, for example @m3.xlarge@ .
--
-- * 'iMarket' - The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
--
-- * 'iPrivateIPAddress' - The private IP address of the instance.
--
-- * 'iInstanceFleetId' - The unique identifier of the instance fleet to which an EC2 instance belongs.
--
-- * 'iId' - The unique identifier for the instance in Amazon EMR.
--
-- * 'iInstanceGroupId' - The identifier of the instance group to which this instance belongs.
--
-- * 'iPrivateDNSName' - The private DNS name of the instance.
--
-- * 'iPublicIPAddress' - The public IP address of the instance.
instance' ::
  Instance
instance' =
  Instance'
    { _iStatus = Nothing,
      _iPublicDNSName = Nothing,
      _iEBSVolumes = Nothing,
      _iEC2InstanceId = Nothing,
      _iInstanceType = Nothing,
      _iMarket = Nothing,
      _iPrivateIPAddress = Nothing,
      _iInstanceFleetId = Nothing,
      _iId = Nothing,
      _iInstanceGroupId = Nothing,
      _iPrivateDNSName = Nothing,
      _iPublicIPAddress = Nothing
    }

-- | The current status of the instance.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\s a -> s {_iStatus = a})

-- | The public DNS name of the instance.
iPublicDNSName :: Lens' Instance (Maybe Text)
iPublicDNSName = lens _iPublicDNSName (\s a -> s {_iPublicDNSName = a})

-- | The list of EBS volumes that are attached to this instance.
iEBSVolumes :: Lens' Instance [EBSVolume]
iEBSVolumes = lens _iEBSVolumes (\s a -> s {_iEBSVolumes = a}) . _Default . _Coerce

-- | The unique identifier of the instance in Amazon EC2.
iEC2InstanceId :: Lens' Instance (Maybe Text)
iEC2InstanceId = lens _iEC2InstanceId (\s a -> s {_iEC2InstanceId = a})

-- | The EC2 instance type, for example @m3.xlarge@ .
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\s a -> s {_iInstanceType = a})

-- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
iMarket :: Lens' Instance (Maybe MarketType)
iMarket = lens _iMarket (\s a -> s {_iMarket = a})

-- | The private IP address of the instance.
iPrivateIPAddress :: Lens' Instance (Maybe Text)
iPrivateIPAddress = lens _iPrivateIPAddress (\s a -> s {_iPrivateIPAddress = a})

-- | The unique identifier of the instance fleet to which an EC2 instance belongs.
iInstanceFleetId :: Lens' Instance (Maybe Text)
iInstanceFleetId = lens _iInstanceFleetId (\s a -> s {_iInstanceFleetId = a})

-- | The unique identifier for the instance in Amazon EMR.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\s a -> s {_iId = a})

-- | The identifier of the instance group to which this instance belongs.
iInstanceGroupId :: Lens' Instance (Maybe Text)
iInstanceGroupId = lens _iInstanceGroupId (\s a -> s {_iInstanceGroupId = a})

-- | The private DNS name of the instance.
iPrivateDNSName :: Lens' Instance (Maybe Text)
iPrivateDNSName = lens _iPrivateDNSName (\s a -> s {_iPrivateDNSName = a})

-- | The public IP address of the instance.
iPublicIPAddress :: Lens' Instance (Maybe Text)
iPublicIPAddress = lens _iPublicIPAddress (\s a -> s {_iPublicIPAddress = a})

instance FromJSON Instance where
  parseJSON =
    withObject
      "Instance"
      ( \x ->
          Instance'
            <$> (x .:? "Status")
            <*> (x .:? "PublicDnsName")
            <*> (x .:? "EbsVolumes" .!= mempty)
            <*> (x .:? "Ec2InstanceId")
            <*> (x .:? "InstanceType")
            <*> (x .:? "Market")
            <*> (x .:? "PrivateIpAddress")
            <*> (x .:? "InstanceFleetId")
            <*> (x .:? "Id")
            <*> (x .:? "InstanceGroupId")
            <*> (x .:? "PrivateDnsName")
            <*> (x .:? "PublicIpAddress")
      )

instance Hashable Instance

instance NFData Instance
