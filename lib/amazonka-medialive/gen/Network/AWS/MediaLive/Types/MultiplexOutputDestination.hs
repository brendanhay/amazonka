{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexOutputDestination where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
import Network.AWS.Prelude

-- | Multiplex output destination settings
--
-- /See:/ 'multiplexOutputDestination' smart constructor.
newtype MultiplexOutputDestination = MultiplexOutputDestination'
  { _modMediaConnectSettings ::
      Maybe
        MultiplexMediaConnectOutputDestinationSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexOutputDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'modMediaConnectSettings' - Multiplex MediaConnect output destination settings.
multiplexOutputDestination ::
  MultiplexOutputDestination
multiplexOutputDestination =
  MultiplexOutputDestination' {_modMediaConnectSettings = Nothing}

-- | Multiplex MediaConnect output destination settings.
modMediaConnectSettings :: Lens' MultiplexOutputDestination (Maybe MultiplexMediaConnectOutputDestinationSettings)
modMediaConnectSettings = lens _modMediaConnectSettings (\s a -> s {_modMediaConnectSettings = a})

instance FromJSON MultiplexOutputDestination where
  parseJSON =
    withObject
      "MultiplexOutputDestination"
      ( \x ->
          MultiplexOutputDestination' <$> (x .:? "mediaConnectSettings")
      )

instance Hashable MultiplexOutputDestination

instance NFData MultiplexOutputDestination
