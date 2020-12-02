{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkBinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkBinding where

import Network.AWS.ECS.Types.TransportProtocol
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on the network bindings between a container and its host container instance. After a task reaches the @RUNNING@ status, manual and automatic host and container port assignments are visible in the @networkBindings@ section of 'DescribeTasks' API responses.
--
--
--
-- /See:/ 'networkBinding' smart constructor.
data NetworkBinding = NetworkBinding'
  { _nbBindIP :: !(Maybe Text),
    _nbProtocol :: !(Maybe TransportProtocol),
    _nbHostPort :: !(Maybe Int),
    _nbContainerPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkBinding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nbBindIP' - The IP address that the container is bound to on the container instance.
--
-- * 'nbProtocol' - The protocol used for the network binding.
--
-- * 'nbHostPort' - The port number on the host that is used with the network binding.
--
-- * 'nbContainerPort' - The port number on the container that is used with the network binding.
networkBinding ::
  NetworkBinding
networkBinding =
  NetworkBinding'
    { _nbBindIP = Nothing,
      _nbProtocol = Nothing,
      _nbHostPort = Nothing,
      _nbContainerPort = Nothing
    }

-- | The IP address that the container is bound to on the container instance.
nbBindIP :: Lens' NetworkBinding (Maybe Text)
nbBindIP = lens _nbBindIP (\s a -> s {_nbBindIP = a})

-- | The protocol used for the network binding.
nbProtocol :: Lens' NetworkBinding (Maybe TransportProtocol)
nbProtocol = lens _nbProtocol (\s a -> s {_nbProtocol = a})

-- | The port number on the host that is used with the network binding.
nbHostPort :: Lens' NetworkBinding (Maybe Int)
nbHostPort = lens _nbHostPort (\s a -> s {_nbHostPort = a})

-- | The port number on the container that is used with the network binding.
nbContainerPort :: Lens' NetworkBinding (Maybe Int)
nbContainerPort = lens _nbContainerPort (\s a -> s {_nbContainerPort = a})

instance FromJSON NetworkBinding where
  parseJSON =
    withObject
      "NetworkBinding"
      ( \x ->
          NetworkBinding'
            <$> (x .:? "bindIP")
            <*> (x .:? "protocol")
            <*> (x .:? "hostPort")
            <*> (x .:? "containerPort")
      )

instance Hashable NetworkBinding

instance NFData NetworkBinding

instance ToJSON NetworkBinding where
  toJSON NetworkBinding' {..} =
    object
      ( catMaybes
          [ ("bindIP" .=) <$> _nbBindIP,
            ("protocol" .=) <$> _nbProtocol,
            ("hostPort" .=) <$> _nbHostPort,
            ("containerPort" .=) <$> _nbContainerPort
          ]
      )
