{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.RemotePortDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.RemotePortDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the remote port.
--
--
--
-- /See:/ 'remotePortDetails' smart constructor.
data RemotePortDetails = RemotePortDetails'
  { _rpdPortName ::
      !(Maybe Text),
    _rpdPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemotePortDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpdPortName' - The port name of the remote connection.
--
-- * 'rpdPort' - The port number of the remote connection.
remotePortDetails ::
  RemotePortDetails
remotePortDetails =
  RemotePortDetails' {_rpdPortName = Nothing, _rpdPort = Nothing}

-- | The port name of the remote connection.
rpdPortName :: Lens' RemotePortDetails (Maybe Text)
rpdPortName = lens _rpdPortName (\s a -> s {_rpdPortName = a})

-- | The port number of the remote connection.
rpdPort :: Lens' RemotePortDetails (Maybe Int)
rpdPort = lens _rpdPort (\s a -> s {_rpdPort = a})

instance FromJSON RemotePortDetails where
  parseJSON =
    withObject
      "RemotePortDetails"
      ( \x ->
          RemotePortDetails' <$> (x .:? "portName") <*> (x .:? "port")
      )

instance Hashable RemotePortDetails

instance NFData RemotePortDetails
