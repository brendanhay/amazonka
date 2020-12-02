{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.LocalPortDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.LocalPortDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the port for the local connection.
--
--
--
-- /See:/ 'localPortDetails' smart constructor.
data LocalPortDetails = LocalPortDetails'
  { _lpdPortName ::
      !(Maybe Text),
    _lpdPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LocalPortDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpdPortName' - The port name of the local connection.
--
-- * 'lpdPort' - The port number of the local connection.
localPortDetails ::
  LocalPortDetails
localPortDetails =
  LocalPortDetails' {_lpdPortName = Nothing, _lpdPort = Nothing}

-- | The port name of the local connection.
lpdPortName :: Lens' LocalPortDetails (Maybe Text)
lpdPortName = lens _lpdPortName (\s a -> s {_lpdPortName = a})

-- | The port number of the local connection.
lpdPort :: Lens' LocalPortDetails (Maybe Int)
lpdPort = lens _lpdPort (\s a -> s {_lpdPort = a})

instance FromJSON LocalPortDetails where
  parseJSON =
    withObject
      "LocalPortDetails"
      ( \x ->
          LocalPortDetails' <$> (x .:? "portName") <*> (x .:? "port")
      )

instance Hashable LocalPortDetails

instance NFData LocalPortDetails
