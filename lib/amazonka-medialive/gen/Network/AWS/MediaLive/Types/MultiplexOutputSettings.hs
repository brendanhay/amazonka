{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexOutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import Network.AWS.Prelude

-- | Multiplex Output Settings
--
-- /See:/ 'multiplexOutputSettings' smart constructor.
newtype MultiplexOutputSettings = MultiplexOutputSettings'
  { _mosDestination ::
      OutputLocationRef
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mosDestination' - Destination is a Multiplex.
multiplexOutputSettings ::
  -- | 'mosDestination'
  OutputLocationRef ->
  MultiplexOutputSettings
multiplexOutputSettings pDestination_ =
  MultiplexOutputSettings' {_mosDestination = pDestination_}

-- | Destination is a Multiplex.
mosDestination :: Lens' MultiplexOutputSettings OutputLocationRef
mosDestination = lens _mosDestination (\s a -> s {_mosDestination = a})

instance FromJSON MultiplexOutputSettings where
  parseJSON =
    withObject
      "MultiplexOutputSettings"
      (\x -> MultiplexOutputSettings' <$> (x .: "destination"))

instance Hashable MultiplexOutputSettings

instance NFData MultiplexOutputSettings

instance ToJSON MultiplexOutputSettings where
  toJSON MultiplexOutputSettings' {..} =
    object (catMaybes [Just ("destination" .= _mosDestination)])
