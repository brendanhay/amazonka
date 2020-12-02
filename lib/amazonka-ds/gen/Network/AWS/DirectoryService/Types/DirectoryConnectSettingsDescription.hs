{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an AD Connector directory.
--
--
--
-- /See:/ 'directoryConnectSettingsDescription' smart constructor.
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
  { _dcsdCustomerUserName ::
      !(Maybe Text),
    _dcsdSubnetIds ::
      !(Maybe [Text]),
    _dcsdVPCId ::
      !(Maybe Text),
    _dcsdSecurityGroupId ::
      !(Maybe Text),
    _dcsdConnectIPs ::
      !(Maybe [Text]),
    _dcsdAvailabilityZones ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryConnectSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsdCustomerUserName' - The user name of the service account in the on-premises directory.
--
-- * 'dcsdSubnetIds' - A list of subnet identifiers in the VPC that the AD Connector is in.
--
-- * 'dcsdVPCId' - The identifier of the VPC that the AD Connector is in.
--
-- * 'dcsdSecurityGroupId' - The security group identifier for the AD Connector directory.
--
-- * 'dcsdConnectIPs' - The IP addresses of the AD Connector servers.
--
-- * 'dcsdAvailabilityZones' - A list of the Availability Zones that the directory is in.
directoryConnectSettingsDescription ::
  DirectoryConnectSettingsDescription
directoryConnectSettingsDescription =
  DirectoryConnectSettingsDescription'
    { _dcsdCustomerUserName =
        Nothing,
      _dcsdSubnetIds = Nothing,
      _dcsdVPCId = Nothing,
      _dcsdSecurityGroupId = Nothing,
      _dcsdConnectIPs = Nothing,
      _dcsdAvailabilityZones = Nothing
    }

-- | The user name of the service account in the on-premises directory.
dcsdCustomerUserName :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdCustomerUserName = lens _dcsdCustomerUserName (\s a -> s {_dcsdCustomerUserName = a})

-- | A list of subnet identifiers in the VPC that the AD Connector is in.
dcsdSubnetIds :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdSubnetIds = lens _dcsdSubnetIds (\s a -> s {_dcsdSubnetIds = a}) . _Default . _Coerce

-- | The identifier of the VPC that the AD Connector is in.
dcsdVPCId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdVPCId = lens _dcsdVPCId (\s a -> s {_dcsdVPCId = a})

-- | The security group identifier for the AD Connector directory.
dcsdSecurityGroupId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdSecurityGroupId = lens _dcsdSecurityGroupId (\s a -> s {_dcsdSecurityGroupId = a})

-- | The IP addresses of the AD Connector servers.
dcsdConnectIPs :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdConnectIPs = lens _dcsdConnectIPs (\s a -> s {_dcsdConnectIPs = a}) . _Default . _Coerce

-- | A list of the Availability Zones that the directory is in.
dcsdAvailabilityZones :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdAvailabilityZones = lens _dcsdAvailabilityZones (\s a -> s {_dcsdAvailabilityZones = a}) . _Default . _Coerce

instance FromJSON DirectoryConnectSettingsDescription where
  parseJSON =
    withObject
      "DirectoryConnectSettingsDescription"
      ( \x ->
          DirectoryConnectSettingsDescription'
            <$> (x .:? "CustomerUserName")
            <*> (x .:? "SubnetIds" .!= mempty)
            <*> (x .:? "VpcId")
            <*> (x .:? "SecurityGroupId")
            <*> (x .:? "ConnectIps" .!= mempty)
            <*> (x .:? "AvailabilityZones" .!= mempty)
      )

instance Hashable DirectoryConnectSettingsDescription

instance NFData DirectoryConnectSettingsDescription
