{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDestination where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputDestinationVPC
import Network.AWS.Prelude

-- | The settings for a PUSH type input.
--
-- /See:/ 'inputDestination' smart constructor.
data InputDestination = InputDestination'
  { _idURL :: !(Maybe Text),
    _idIP :: !(Maybe Text),
    _idVPC :: !(Maybe InputDestinationVPC),
    _idPort :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idURL' - This represents the endpoint that the customer stream will be pushed to.
--
-- * 'idIP' - The system-generated static IP address of endpoint. It remains fixed for the lifetime of the input.
--
-- * 'idVPC' - Undocumented member.
--
-- * 'idPort' - The port number for the input.
inputDestination ::
  InputDestination
inputDestination =
  InputDestination'
    { _idURL = Nothing,
      _idIP = Nothing,
      _idVPC = Nothing,
      _idPort = Nothing
    }

-- | This represents the endpoint that the customer stream will be pushed to.
idURL :: Lens' InputDestination (Maybe Text)
idURL = lens _idURL (\s a -> s {_idURL = a})

-- | The system-generated static IP address of endpoint. It remains fixed for the lifetime of the input.
idIP :: Lens' InputDestination (Maybe Text)
idIP = lens _idIP (\s a -> s {_idIP = a})

-- | Undocumented member.
idVPC :: Lens' InputDestination (Maybe InputDestinationVPC)
idVPC = lens _idVPC (\s a -> s {_idVPC = a})

-- | The port number for the input.
idPort :: Lens' InputDestination (Maybe Text)
idPort = lens _idPort (\s a -> s {_idPort = a})

instance FromJSON InputDestination where
  parseJSON =
    withObject
      "InputDestination"
      ( \x ->
          InputDestination'
            <$> (x .:? "url")
            <*> (x .:? "ip")
            <*> (x .:? "vpc")
            <*> (x .:? "port")
      )

instance Hashable InputDestination

instance NFData InputDestination
