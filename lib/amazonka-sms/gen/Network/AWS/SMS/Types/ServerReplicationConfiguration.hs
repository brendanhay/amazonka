{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerReplicationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerReplicationParameters

-- | Replication configuration of a server.
--
--
--
-- /See:/ 'serverReplicationConfiguration' smart constructor.
data ServerReplicationConfiguration = ServerReplicationConfiguration'
  { _srcServerReplicationParameters ::
      !( Maybe
           ServerReplicationParameters
       ),
    _srcServer :: !(Maybe Server)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcServerReplicationParameters' - The parameters for replicating the server.
--
-- * 'srcServer' - The ID of the server with which this replication configuration is associated.
serverReplicationConfiguration ::
  ServerReplicationConfiguration
serverReplicationConfiguration =
  ServerReplicationConfiguration'
    { _srcServerReplicationParameters =
        Nothing,
      _srcServer = Nothing
    }

-- | The parameters for replicating the server.
srcServerReplicationParameters :: Lens' ServerReplicationConfiguration (Maybe ServerReplicationParameters)
srcServerReplicationParameters = lens _srcServerReplicationParameters (\s a -> s {_srcServerReplicationParameters = a})

-- | The ID of the server with which this replication configuration is associated.
srcServer :: Lens' ServerReplicationConfiguration (Maybe Server)
srcServer = lens _srcServer (\s a -> s {_srcServer = a})

instance FromJSON ServerReplicationConfiguration where
  parseJSON =
    withObject
      "ServerReplicationConfiguration"
      ( \x ->
          ServerReplicationConfiguration'
            <$> (x .:? "serverReplicationParameters") <*> (x .:? "server")
      )

instance Hashable ServerReplicationConfiguration

instance NFData ServerReplicationConfiguration

instance ToJSON ServerReplicationConfiguration where
  toJSON ServerReplicationConfiguration' {..} =
    object
      ( catMaybes
          [ ("serverReplicationParameters" .=)
              <$> _srcServerReplicationParameters,
            ("server" .=) <$> _srcServer
          ]
      )
