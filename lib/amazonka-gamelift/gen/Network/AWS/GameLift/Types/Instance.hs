{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Instance where

import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.GameLift.Types.InstanceStatus
import Network.AWS.GameLift.Types.OperatingSystem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Properties that describe an instance of a virtual computing resource that hosts one or more game servers. A fleet may contain zero or more instances.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iCreationTime :: !(Maybe POSIX),
    _iInstanceId :: !(Maybe Text),
    _iStatus :: !(Maybe InstanceStatus),
    _iIPAddress :: !(Maybe Text),
    _iOperatingSystem :: !(Maybe OperatingSystem),
    _iType :: !(Maybe EC2InstanceType),
    _iFleetId :: !(Maybe Text),
    _iDNSName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'iInstanceId' - A unique identifier for an instance.
--
-- * 'iStatus' - Current status of the instance. Possible statuses include the following:     * __PENDING__ -- The instance is in the process of being created and launching server processes as defined in the fleet's run-time configuration.      * __ACTIVE__ -- The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.      * __TERMINATING__ -- The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
--
-- * 'iIPAddress' - IP address that is assigned to the instance.
--
-- * 'iOperatingSystem' - Operating system that is running on this instance.
--
-- * 'iType' - EC2 instance type that defines the computing resources of this instance.
--
-- * 'iFleetId' - A unique identifier for a fleet that the instance is in.
--
-- * 'iDNSName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
instance' ::
  Instance
instance' =
  Instance'
    { _iCreationTime = Nothing,
      _iInstanceId = Nothing,
      _iStatus = Nothing,
      _iIPAddress = Nothing,
      _iOperatingSystem = Nothing,
      _iType = Nothing,
      _iFleetId = Nothing,
      _iDNSName = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
iCreationTime :: Lens' Instance (Maybe UTCTime)
iCreationTime = lens _iCreationTime (\s a -> s {_iCreationTime = a}) . mapping _Time

-- | A unique identifier for an instance.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s {_iInstanceId = a})

-- | Current status of the instance. Possible statuses include the following:     * __PENDING__ -- The instance is in the process of being created and launching server processes as defined in the fleet's run-time configuration.      * __ACTIVE__ -- The instance has been successfully created and at least one server process has successfully launched and reported back to Amazon GameLift that it is ready to host a game session. The instance is now considered ready to host game sessions.      * __TERMINATING__ -- The instance is in the process of shutting down. This may happen to reduce capacity during a scaling down event or to recycle resources in the event of a problem.
iStatus :: Lens' Instance (Maybe InstanceStatus)
iStatus = lens _iStatus (\s a -> s {_iStatus = a})

-- | IP address that is assigned to the instance.
iIPAddress :: Lens' Instance (Maybe Text)
iIPAddress = lens _iIPAddress (\s a -> s {_iIPAddress = a})

-- | Operating system that is running on this instance.
iOperatingSystem :: Lens' Instance (Maybe OperatingSystem)
iOperatingSystem = lens _iOperatingSystem (\s a -> s {_iOperatingSystem = a})

-- | EC2 instance type that defines the computing resources of this instance.
iType :: Lens' Instance (Maybe EC2InstanceType)
iType = lens _iType (\s a -> s {_iType = a})

-- | A unique identifier for a fleet that the instance is in.
iFleetId :: Lens' Instance (Maybe Text)
iFleetId = lens _iFleetId (\s a -> s {_iFleetId = a})

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
iDNSName :: Lens' Instance (Maybe Text)
iDNSName = lens _iDNSName (\s a -> s {_iDNSName = a})

instance FromJSON Instance where
  parseJSON =
    withObject
      "Instance"
      ( \x ->
          Instance'
            <$> (x .:? "CreationTime")
            <*> (x .:? "InstanceId")
            <*> (x .:? "Status")
            <*> (x .:? "IpAddress")
            <*> (x .:? "OperatingSystem")
            <*> (x .:? "Type")
            <*> (x .:? "FleetId")
            <*> (x .:? "DnsName")
      )

instance Hashable Instance

instance NFData Instance
