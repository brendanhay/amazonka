{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.NeighborConnectionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.NeighborConnectionDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about neighboring servers.
--
--
--
-- /See:/ 'neighborConnectionDetail' smart constructor.
data NeighborConnectionDetail = NeighborConnectionDetail'
  { _ncdTransportProtocol ::
      !(Maybe Text),
    _ncdDestinationPort :: !(Maybe Int),
    _ncdSourceServerId :: !Text,
    _ncdDestinationServerId :: !Text,
    _ncdConnectionsCount :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NeighborConnectionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncdTransportProtocol' - The network protocol used for the connection.
--
-- * 'ncdDestinationPort' - The destination network port for the connection.
--
-- * 'ncdSourceServerId' - The ID of the server that opened the network connection.
--
-- * 'ncdDestinationServerId' - The ID of the server that accepted the network connection.
--
-- * 'ncdConnectionsCount' - The number of open network connections with the neighboring server.
neighborConnectionDetail ::
  -- | 'ncdSourceServerId'
  Text ->
  -- | 'ncdDestinationServerId'
  Text ->
  -- | 'ncdConnectionsCount'
  Integer ->
  NeighborConnectionDetail
neighborConnectionDetail
  pSourceServerId_
  pDestinationServerId_
  pConnectionsCount_ =
    NeighborConnectionDetail'
      { _ncdTransportProtocol = Nothing,
        _ncdDestinationPort = Nothing,
        _ncdSourceServerId = pSourceServerId_,
        _ncdDestinationServerId = pDestinationServerId_,
        _ncdConnectionsCount = pConnectionsCount_
      }

-- | The network protocol used for the connection.
ncdTransportProtocol :: Lens' NeighborConnectionDetail (Maybe Text)
ncdTransportProtocol = lens _ncdTransportProtocol (\s a -> s {_ncdTransportProtocol = a})

-- | The destination network port for the connection.
ncdDestinationPort :: Lens' NeighborConnectionDetail (Maybe Int)
ncdDestinationPort = lens _ncdDestinationPort (\s a -> s {_ncdDestinationPort = a})

-- | The ID of the server that opened the network connection.
ncdSourceServerId :: Lens' NeighborConnectionDetail Text
ncdSourceServerId = lens _ncdSourceServerId (\s a -> s {_ncdSourceServerId = a})

-- | The ID of the server that accepted the network connection.
ncdDestinationServerId :: Lens' NeighborConnectionDetail Text
ncdDestinationServerId = lens _ncdDestinationServerId (\s a -> s {_ncdDestinationServerId = a})

-- | The number of open network connections with the neighboring server.
ncdConnectionsCount :: Lens' NeighborConnectionDetail Integer
ncdConnectionsCount = lens _ncdConnectionsCount (\s a -> s {_ncdConnectionsCount = a})

instance FromJSON NeighborConnectionDetail where
  parseJSON =
    withObject
      "NeighborConnectionDetail"
      ( \x ->
          NeighborConnectionDetail'
            <$> (x .:? "transportProtocol")
            <*> (x .:? "destinationPort")
            <*> (x .: "sourceServerId")
            <*> (x .: "destinationServerId")
            <*> (x .: "connectionsCount")
      )

instance Hashable NeighborConnectionDetail

instance NFData NeighborConnectionDetail
