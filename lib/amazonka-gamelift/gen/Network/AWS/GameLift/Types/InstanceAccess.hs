{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceAccess where

import Network.AWS.GameLift.Types.InstanceCredentials
import Network.AWS.GameLift.Types.OperatingSystem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information required to remotely connect to a fleet instance. Access is requested by calling 'GetInstanceAccess' .
--
--
--
-- /See:/ 'instanceAccess' smart constructor.
data InstanceAccess = InstanceAccess'
  { _iaInstanceId ::
      !(Maybe Text),
    _iaIPAddress :: !(Maybe Text),
    _iaOperatingSystem :: !(Maybe OperatingSystem),
    _iaCredentials :: !(Maybe (Sensitive InstanceCredentials)),
    _iaFleetId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaInstanceId' - A unique identifier for an instance being accessed.
--
-- * 'iaIPAddress' - IP address that is assigned to the instance.
--
-- * 'iaOperatingSystem' - Operating system that is running on the instance.
--
-- * 'iaCredentials' - Credentials required to access the instance.
--
-- * 'iaFleetId' - A unique identifier for a fleet containing the instance being accessed.
instanceAccess ::
  InstanceAccess
instanceAccess =
  InstanceAccess'
    { _iaInstanceId = Nothing,
      _iaIPAddress = Nothing,
      _iaOperatingSystem = Nothing,
      _iaCredentials = Nothing,
      _iaFleetId = Nothing
    }

-- | A unique identifier for an instance being accessed.
iaInstanceId :: Lens' InstanceAccess (Maybe Text)
iaInstanceId = lens _iaInstanceId (\s a -> s {_iaInstanceId = a})

-- | IP address that is assigned to the instance.
iaIPAddress :: Lens' InstanceAccess (Maybe Text)
iaIPAddress = lens _iaIPAddress (\s a -> s {_iaIPAddress = a})

-- | Operating system that is running on the instance.
iaOperatingSystem :: Lens' InstanceAccess (Maybe OperatingSystem)
iaOperatingSystem = lens _iaOperatingSystem (\s a -> s {_iaOperatingSystem = a})

-- | Credentials required to access the instance.
iaCredentials :: Lens' InstanceAccess (Maybe InstanceCredentials)
iaCredentials = lens _iaCredentials (\s a -> s {_iaCredentials = a}) . mapping _Sensitive

-- | A unique identifier for a fleet containing the instance being accessed.
iaFleetId :: Lens' InstanceAccess (Maybe Text)
iaFleetId = lens _iaFleetId (\s a -> s {_iaFleetId = a})

instance FromJSON InstanceAccess where
  parseJSON =
    withObject
      "InstanceAccess"
      ( \x ->
          InstanceAccess'
            <$> (x .:? "InstanceId")
            <*> (x .:? "IpAddress")
            <*> (x .:? "OperatingSystem")
            <*> (x .:? "Credentials")
            <*> (x .:? "FleetId")
      )

instance Hashable InstanceAccess

instance NFData InstanceAccess
