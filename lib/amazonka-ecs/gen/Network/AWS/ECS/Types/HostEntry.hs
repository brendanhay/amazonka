{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HostEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HostEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Hostnames and IP address entries that are added to the @/etc/hosts@ file of a container via the @extraHosts@ parameter of its 'ContainerDefinition' .
--
--
--
-- /See:/ 'hostEntry' smart constructor.
data HostEntry = HostEntry'
  { _heHostname :: !Text,
    _heIpAddress :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HostEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heHostname' - The hostname to use in the @/etc/hosts@ entry.
--
-- * 'heIpAddress' - The IP address to use in the @/etc/hosts@ entry.
hostEntry ::
  -- | 'heHostname'
  Text ->
  -- | 'heIpAddress'
  Text ->
  HostEntry
hostEntry pHostname_ pIpAddress_ =
  HostEntry' {_heHostname = pHostname_, _heIpAddress = pIpAddress_}

-- | The hostname to use in the @/etc/hosts@ entry.
heHostname :: Lens' HostEntry Text
heHostname = lens _heHostname (\s a -> s {_heHostname = a})

-- | The IP address to use in the @/etc/hosts@ entry.
heIpAddress :: Lens' HostEntry Text
heIpAddress = lens _heIpAddress (\s a -> s {_heIpAddress = a})

instance FromJSON HostEntry where
  parseJSON =
    withObject
      "HostEntry"
      (\x -> HostEntry' <$> (x .: "hostname") <*> (x .: "ipAddress"))

instance Hashable HostEntry

instance NFData HostEntry

instance ToJSON HostEntry where
  toJSON HostEntry' {..} =
    object
      ( catMaybes
          [ Just ("hostname" .= _heHostname),
            Just ("ipAddress" .= _heIpAddress)
          ]
      )
