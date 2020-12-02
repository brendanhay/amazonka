{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DomainController
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainController where

import Network.AWS.DirectoryService.Types.DomainControllerStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the domain controllers for a specified directory.
--
--
--
-- /See:/ 'domainController' smart constructor.
data DomainController = DomainController'
  { _dcStatus ::
      !(Maybe DomainControllerStatus),
    _dcDirectoryId :: !(Maybe Text),
    _dcVPCId :: !(Maybe Text),
    _dcLaunchTime :: !(Maybe POSIX),
    _dcSubnetId :: !(Maybe Text),
    _dcAvailabilityZone :: !(Maybe Text),
    _dcStatusLastUpdatedDateTime :: !(Maybe POSIX),
    _dcStatusReason :: !(Maybe Text),
    _dcDNSIPAddr :: !(Maybe Text),
    _dcDomainControllerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainController' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcStatus' - The status of the domain controller.
--
-- * 'dcDirectoryId' - Identifier of the directory where the domain controller resides.
--
-- * 'dcVPCId' - The identifier of the VPC that contains the domain controller.
--
-- * 'dcLaunchTime' - Specifies when the domain controller was created.
--
-- * 'dcSubnetId' - Identifier of the subnet in the VPC that contains the domain controller.
--
-- * 'dcAvailabilityZone' - The Availability Zone where the domain controller is located.
--
-- * 'dcStatusLastUpdatedDateTime' - The date and time that the status was last updated.
--
-- * 'dcStatusReason' - A description of the domain controller state.
--
-- * 'dcDNSIPAddr' - The IP address of the domain controller.
--
-- * 'dcDomainControllerId' - Identifies a specific domain controller in the directory.
domainController ::
  DomainController
domainController =
  DomainController'
    { _dcStatus = Nothing,
      _dcDirectoryId = Nothing,
      _dcVPCId = Nothing,
      _dcLaunchTime = Nothing,
      _dcSubnetId = Nothing,
      _dcAvailabilityZone = Nothing,
      _dcStatusLastUpdatedDateTime = Nothing,
      _dcStatusReason = Nothing,
      _dcDNSIPAddr = Nothing,
      _dcDomainControllerId = Nothing
    }

-- | The status of the domain controller.
dcStatus :: Lens' DomainController (Maybe DomainControllerStatus)
dcStatus = lens _dcStatus (\s a -> s {_dcStatus = a})

-- | Identifier of the directory where the domain controller resides.
dcDirectoryId :: Lens' DomainController (Maybe Text)
dcDirectoryId = lens _dcDirectoryId (\s a -> s {_dcDirectoryId = a})

-- | The identifier of the VPC that contains the domain controller.
dcVPCId :: Lens' DomainController (Maybe Text)
dcVPCId = lens _dcVPCId (\s a -> s {_dcVPCId = a})

-- | Specifies when the domain controller was created.
dcLaunchTime :: Lens' DomainController (Maybe UTCTime)
dcLaunchTime = lens _dcLaunchTime (\s a -> s {_dcLaunchTime = a}) . mapping _Time

-- | Identifier of the subnet in the VPC that contains the domain controller.
dcSubnetId :: Lens' DomainController (Maybe Text)
dcSubnetId = lens _dcSubnetId (\s a -> s {_dcSubnetId = a})

-- | The Availability Zone where the domain controller is located.
dcAvailabilityZone :: Lens' DomainController (Maybe Text)
dcAvailabilityZone = lens _dcAvailabilityZone (\s a -> s {_dcAvailabilityZone = a})

-- | The date and time that the status was last updated.
dcStatusLastUpdatedDateTime :: Lens' DomainController (Maybe UTCTime)
dcStatusLastUpdatedDateTime = lens _dcStatusLastUpdatedDateTime (\s a -> s {_dcStatusLastUpdatedDateTime = a}) . mapping _Time

-- | A description of the domain controller state.
dcStatusReason :: Lens' DomainController (Maybe Text)
dcStatusReason = lens _dcStatusReason (\s a -> s {_dcStatusReason = a})

-- | The IP address of the domain controller.
dcDNSIPAddr :: Lens' DomainController (Maybe Text)
dcDNSIPAddr = lens _dcDNSIPAddr (\s a -> s {_dcDNSIPAddr = a})

-- | Identifies a specific domain controller in the directory.
dcDomainControllerId :: Lens' DomainController (Maybe Text)
dcDomainControllerId = lens _dcDomainControllerId (\s a -> s {_dcDomainControllerId = a})

instance FromJSON DomainController where
  parseJSON =
    withObject
      "DomainController"
      ( \x ->
          DomainController'
            <$> (x .:? "Status")
            <*> (x .:? "DirectoryId")
            <*> (x .:? "VpcId")
            <*> (x .:? "LaunchTime")
            <*> (x .:? "SubnetId")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "StatusLastUpdatedDateTime")
            <*> (x .:? "StatusReason")
            <*> (x .:? "DnsIpAddr")
            <*> (x .:? "DomainControllerId")
      )

instance Hashable DomainController

instance NFData DomainController
