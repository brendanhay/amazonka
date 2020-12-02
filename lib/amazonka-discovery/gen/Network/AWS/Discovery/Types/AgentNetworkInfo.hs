{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentNetworkInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentNetworkInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Network details about the host where the agent/connector resides.
--
--
--
-- /See:/ 'agentNetworkInfo' smart constructor.
data AgentNetworkInfo = AgentNetworkInfo'
  { _aniIpAddress ::
      !(Maybe Text),
    _aniMacAddress :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AgentNetworkInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aniIpAddress' - The IP address for the host where the agent/connector resides.
--
-- * 'aniMacAddress' - The MAC address for the host where the agent/connector resides.
agentNetworkInfo ::
  AgentNetworkInfo
agentNetworkInfo =
  AgentNetworkInfo'
    { _aniIpAddress = Nothing,
      _aniMacAddress = Nothing
    }

-- | The IP address for the host where the agent/connector resides.
aniIpAddress :: Lens' AgentNetworkInfo (Maybe Text)
aniIpAddress = lens _aniIpAddress (\s a -> s {_aniIpAddress = a})

-- | The MAC address for the host where the agent/connector resides.
aniMacAddress :: Lens' AgentNetworkInfo (Maybe Text)
aniMacAddress = lens _aniMacAddress (\s a -> s {_aniMacAddress = a})

instance FromJSON AgentNetworkInfo where
  parseJSON =
    withObject
      "AgentNetworkInfo"
      ( \x ->
          AgentNetworkInfo' <$> (x .:? "ipAddress") <*> (x .:? "macAddress")
      )

instance Hashable AgentNetworkInfo

instance NFData AgentNetworkInfo
