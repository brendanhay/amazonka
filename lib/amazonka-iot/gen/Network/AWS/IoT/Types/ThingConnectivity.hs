{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingConnectivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingConnectivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The connectivity status of the thing.
--
--
--
-- /See:/ 'thingConnectivity' smart constructor.
data ThingConnectivity = ThingConnectivity'
  { _tcConnected ::
      !(Maybe Bool),
    _tcTimestamp :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingConnectivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcConnected' - True if the thing is connected to the AWS IoT service; false if it is not connected.
--
-- * 'tcTimestamp' - The epoch time (in milliseconds) when the thing last connected or disconnected. If the thing has been disconnected for more than a few weeks, the time value might be missing.
thingConnectivity ::
  ThingConnectivity
thingConnectivity =
  ThingConnectivity'
    { _tcConnected = Nothing,
      _tcTimestamp = Nothing
    }

-- | True if the thing is connected to the AWS IoT service; false if it is not connected.
tcConnected :: Lens' ThingConnectivity (Maybe Bool)
tcConnected = lens _tcConnected (\s a -> s {_tcConnected = a})

-- | The epoch time (in milliseconds) when the thing last connected or disconnected. If the thing has been disconnected for more than a few weeks, the time value might be missing.
tcTimestamp :: Lens' ThingConnectivity (Maybe Integer)
tcTimestamp = lens _tcTimestamp (\s a -> s {_tcTimestamp = a})

instance FromJSON ThingConnectivity where
  parseJSON =
    withObject
      "ThingConnectivity"
      ( \x ->
          ThingConnectivity' <$> (x .:? "connected") <*> (x .:? "timestamp")
      )

instance Hashable ThingConnectivity

instance NFData ThingConnectivity
