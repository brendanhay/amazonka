{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupLaunchConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.ServerLaunchConfiguration

-- | Launch configuration for a server group.
--
--
--
-- /See:/ 'serverGroupLaunchConfiguration' smart constructor.
data ServerGroupLaunchConfiguration = ServerGroupLaunchConfiguration'
  { _sglcServerGroupId ::
      !(Maybe Text),
    _sglcLaunchOrder ::
      !(Maybe Int),
    _sglcServerLaunchConfigurations ::
      !( Maybe
           [ServerLaunchConfiguration]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerGroupLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sglcServerGroupId' - The ID of the server group with which the launch configuration is associated.
--
-- * 'sglcLaunchOrder' - The launch order of servers in the server group.
--
-- * 'sglcServerLaunchConfigurations' - The launch configuration for servers in the server group.
serverGroupLaunchConfiguration ::
  ServerGroupLaunchConfiguration
serverGroupLaunchConfiguration =
  ServerGroupLaunchConfiguration'
    { _sglcServerGroupId = Nothing,
      _sglcLaunchOrder = Nothing,
      _sglcServerLaunchConfigurations = Nothing
    }

-- | The ID of the server group with which the launch configuration is associated.
sglcServerGroupId :: Lens' ServerGroupLaunchConfiguration (Maybe Text)
sglcServerGroupId = lens _sglcServerGroupId (\s a -> s {_sglcServerGroupId = a})

-- | The launch order of servers in the server group.
sglcLaunchOrder :: Lens' ServerGroupLaunchConfiguration (Maybe Int)
sglcLaunchOrder = lens _sglcLaunchOrder (\s a -> s {_sglcLaunchOrder = a})

-- | The launch configuration for servers in the server group.
sglcServerLaunchConfigurations :: Lens' ServerGroupLaunchConfiguration [ServerLaunchConfiguration]
sglcServerLaunchConfigurations = lens _sglcServerLaunchConfigurations (\s a -> s {_sglcServerLaunchConfigurations = a}) . _Default . _Coerce

instance FromJSON ServerGroupLaunchConfiguration where
  parseJSON =
    withObject
      "ServerGroupLaunchConfiguration"
      ( \x ->
          ServerGroupLaunchConfiguration'
            <$> (x .:? "serverGroupId")
            <*> (x .:? "launchOrder")
            <*> (x .:? "serverLaunchConfigurations" .!= mempty)
      )

instance Hashable ServerGroupLaunchConfiguration

instance NFData ServerGroupLaunchConfiguration

instance ToJSON ServerGroupLaunchConfiguration where
  toJSON ServerGroupLaunchConfiguration' {..} =
    object
      ( catMaybes
          [ ("serverGroupId" .=) <$> _sglcServerGroupId,
            ("launchOrder" .=) <$> _sglcLaunchOrder,
            ("serverLaunchConfigurations" .=)
              <$> _sglcServerLaunchConfigurations
          ]
      )
