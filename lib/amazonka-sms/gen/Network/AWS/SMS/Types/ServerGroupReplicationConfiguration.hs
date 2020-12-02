{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupReplicationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.ServerReplicationConfiguration

-- | Replication configuration for a server group.
--
--
--
-- /See:/ 'serverGroupReplicationConfiguration' smart constructor.
data ServerGroupReplicationConfiguration = ServerGroupReplicationConfiguration'
  { _sgrcServerGroupId ::
      !(Maybe Text),
    _sgrcServerReplicationConfigurations ::
      !( Maybe
           [ServerReplicationConfiguration]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerGroupReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgrcServerGroupId' - The ID of the server group with which this replication configuration is associated.
--
-- * 'sgrcServerReplicationConfigurations' - The replication configuration for servers in the server group.
serverGroupReplicationConfiguration ::
  ServerGroupReplicationConfiguration
serverGroupReplicationConfiguration =
  ServerGroupReplicationConfiguration'
    { _sgrcServerGroupId =
        Nothing,
      _sgrcServerReplicationConfigurations = Nothing
    }

-- | The ID of the server group with which this replication configuration is associated.
sgrcServerGroupId :: Lens' ServerGroupReplicationConfiguration (Maybe Text)
sgrcServerGroupId = lens _sgrcServerGroupId (\s a -> s {_sgrcServerGroupId = a})

-- | The replication configuration for servers in the server group.
sgrcServerReplicationConfigurations :: Lens' ServerGroupReplicationConfiguration [ServerReplicationConfiguration]
sgrcServerReplicationConfigurations = lens _sgrcServerReplicationConfigurations (\s a -> s {_sgrcServerReplicationConfigurations = a}) . _Default . _Coerce

instance FromJSON ServerGroupReplicationConfiguration where
  parseJSON =
    withObject
      "ServerGroupReplicationConfiguration"
      ( \x ->
          ServerGroupReplicationConfiguration'
            <$> (x .:? "serverGroupId")
            <*> (x .:? "serverReplicationConfigurations" .!= mempty)
      )

instance Hashable ServerGroupReplicationConfiguration

instance NFData ServerGroupReplicationConfiguration

instance ToJSON ServerGroupReplicationConfiguration where
  toJSON ServerGroupReplicationConfiguration' {..} =
    object
      ( catMaybes
          [ ("serverGroupId" .=) <$> _sgrcServerGroupId,
            ("serverReplicationConfigurations" .=)
              <$> _sgrcServerReplicationConfigurations
          ]
      )
