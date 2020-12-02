{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.NetworkConnectionAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.NetworkConnectionAction where

import Network.AWS.GuardDuty.Types.LocalIPDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.RemoteIPDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the NETWORK_CONNECTION action described in the finding.
--
--
--
-- /See:/ 'networkConnectionAction' smart constructor.
data NetworkConnectionAction = NetworkConnectionAction'
  { _ncaRemoteIPDetails ::
      !(Maybe RemoteIPDetails),
    _ncaProtocol :: !(Maybe Text),
    _ncaLocalIPDetails ::
      !(Maybe LocalIPDetails),
    _ncaRemotePortDetails ::
      !(Maybe RemotePortDetails),
    _ncaBlocked :: !(Maybe Bool),
    _ncaConnectionDirection :: !(Maybe Text),
    _ncaLocalPortDetails ::
      !(Maybe LocalPortDetails)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkConnectionAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncaRemoteIPDetails' - The remote IP information of the connection.
--
-- * 'ncaProtocol' - The network connection protocol.
--
-- * 'ncaLocalIPDetails' - The local IP information of the connection.
--
-- * 'ncaRemotePortDetails' - The remote port information of the connection.
--
-- * 'ncaBlocked' - Indicates whether EC2 blocked the network connection to your instance.
--
-- * 'ncaConnectionDirection' - The network connection direction.
--
-- * 'ncaLocalPortDetails' - The local port information of the connection.
networkConnectionAction ::
  NetworkConnectionAction
networkConnectionAction =
  NetworkConnectionAction'
    { _ncaRemoteIPDetails = Nothing,
      _ncaProtocol = Nothing,
      _ncaLocalIPDetails = Nothing,
      _ncaRemotePortDetails = Nothing,
      _ncaBlocked = Nothing,
      _ncaConnectionDirection = Nothing,
      _ncaLocalPortDetails = Nothing
    }

-- | The remote IP information of the connection.
ncaRemoteIPDetails :: Lens' NetworkConnectionAction (Maybe RemoteIPDetails)
ncaRemoteIPDetails = lens _ncaRemoteIPDetails (\s a -> s {_ncaRemoteIPDetails = a})

-- | The network connection protocol.
ncaProtocol :: Lens' NetworkConnectionAction (Maybe Text)
ncaProtocol = lens _ncaProtocol (\s a -> s {_ncaProtocol = a})

-- | The local IP information of the connection.
ncaLocalIPDetails :: Lens' NetworkConnectionAction (Maybe LocalIPDetails)
ncaLocalIPDetails = lens _ncaLocalIPDetails (\s a -> s {_ncaLocalIPDetails = a})

-- | The remote port information of the connection.
ncaRemotePortDetails :: Lens' NetworkConnectionAction (Maybe RemotePortDetails)
ncaRemotePortDetails = lens _ncaRemotePortDetails (\s a -> s {_ncaRemotePortDetails = a})

-- | Indicates whether EC2 blocked the network connection to your instance.
ncaBlocked :: Lens' NetworkConnectionAction (Maybe Bool)
ncaBlocked = lens _ncaBlocked (\s a -> s {_ncaBlocked = a})

-- | The network connection direction.
ncaConnectionDirection :: Lens' NetworkConnectionAction (Maybe Text)
ncaConnectionDirection = lens _ncaConnectionDirection (\s a -> s {_ncaConnectionDirection = a})

-- | The local port information of the connection.
ncaLocalPortDetails :: Lens' NetworkConnectionAction (Maybe LocalPortDetails)
ncaLocalPortDetails = lens _ncaLocalPortDetails (\s a -> s {_ncaLocalPortDetails = a})

instance FromJSON NetworkConnectionAction where
  parseJSON =
    withObject
      "NetworkConnectionAction"
      ( \x ->
          NetworkConnectionAction'
            <$> (x .:? "remoteIpDetails")
            <*> (x .:? "protocol")
            <*> (x .:? "localIpDetails")
            <*> (x .:? "remotePortDetails")
            <*> (x .:? "blocked")
            <*> (x .:? "connectionDirection")
            <*> (x .:? "localPortDetails")
      )

instance Hashable NetworkConnectionAction

instance NFData NetworkConnectionAction
