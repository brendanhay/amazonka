{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the directory.
--
--
--
-- /See:/ 'directoryVPCSettingsDescription' smart constructor.
data DirectoryVPCSettingsDescription = DirectoryVPCSettingsDescription'
  { _dvsdSubnetIds ::
      !(Maybe [Text]),
    _dvsdVPCId :: !(Maybe Text),
    _dvsdSecurityGroupId ::
      !(Maybe Text),
    _dvsdAvailabilityZones ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryVPCSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsdSubnetIds' - The identifiers of the subnets for the directory servers.
--
-- * 'dvsdVPCId' - The identifier of the VPC that the directory is in.
--
-- * 'dvsdSecurityGroupId' - The domain controller security group identifier for the directory.
--
-- * 'dvsdAvailabilityZones' - The list of Availability Zones that the directory is in.
directoryVPCSettingsDescription ::
  DirectoryVPCSettingsDescription
directoryVPCSettingsDescription =
  DirectoryVPCSettingsDescription'
    { _dvsdSubnetIds = Nothing,
      _dvsdVPCId = Nothing,
      _dvsdSecurityGroupId = Nothing,
      _dvsdAvailabilityZones = Nothing
    }

-- | The identifiers of the subnets for the directory servers.
dvsdSubnetIds :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdSubnetIds = lens _dvsdSubnetIds (\s a -> s {_dvsdSubnetIds = a}) . _Default . _Coerce

-- | The identifier of the VPC that the directory is in.
dvsdVPCId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdVPCId = lens _dvsdVPCId (\s a -> s {_dvsdVPCId = a})

-- | The domain controller security group identifier for the directory.
dvsdSecurityGroupId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdSecurityGroupId = lens _dvsdSecurityGroupId (\s a -> s {_dvsdSecurityGroupId = a})

-- | The list of Availability Zones that the directory is in.
dvsdAvailabilityZones :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdAvailabilityZones = lens _dvsdAvailabilityZones (\s a -> s {_dvsdAvailabilityZones = a}) . _Default . _Coerce

instance FromJSON DirectoryVPCSettingsDescription where
  parseJSON =
    withObject
      "DirectoryVPCSettingsDescription"
      ( \x ->
          DirectoryVPCSettingsDescription'
            <$> (x .:? "SubnetIds" .!= mempty)
            <*> (x .:? "VpcId")
            <*> (x .:? "SecurityGroupId")
            <*> (x .:? "AvailabilityZones" .!= mempty)
      )

instance Hashable DirectoryVPCSettingsDescription

instance NFData DirectoryVPCSettingsDescription
