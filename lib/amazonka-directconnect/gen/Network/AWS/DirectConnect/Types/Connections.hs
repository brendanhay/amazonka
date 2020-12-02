{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Connections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Connections where

import Network.AWS.DirectConnect.Types.Connection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'connections' smart constructor.
newtype Connections = Connections'
  { _cConnections ::
      Maybe [Connection]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Connections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConnections' - The connections.
connections ::
  Connections
connections = Connections' {_cConnections = Nothing}

-- | The connections.
cConnections :: Lens' Connections [Connection]
cConnections = lens _cConnections (\s a -> s {_cConnections = a}) . _Default . _Coerce

instance FromJSON Connections where
  parseJSON =
    withObject
      "Connections"
      (\x -> Connections' <$> (x .:? "connections" .!= mempty))

instance Hashable Connections

instance NFData Connections
