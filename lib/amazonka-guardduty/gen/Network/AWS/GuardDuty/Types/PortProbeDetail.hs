{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PortProbeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PortProbeDetail where

import Network.AWS.GuardDuty.Types.LocalIPDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.RemoteIPDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the port probe details.
--
--
--
-- /See:/ 'portProbeDetail' smart constructor.
data PortProbeDetail = PortProbeDetail'
  { _ppdRemoteIPDetails ::
      !(Maybe RemoteIPDetails),
    _ppdLocalIPDetails :: !(Maybe LocalIPDetails),
    _ppdLocalPortDetails :: !(Maybe LocalPortDetails)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PortProbeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppdRemoteIPDetails' - The remote IP information of the connection.
--
-- * 'ppdLocalIPDetails' - The local IP information of the connection.
--
-- * 'ppdLocalPortDetails' - The local port information of the connection.
portProbeDetail ::
  PortProbeDetail
portProbeDetail =
  PortProbeDetail'
    { _ppdRemoteIPDetails = Nothing,
      _ppdLocalIPDetails = Nothing,
      _ppdLocalPortDetails = Nothing
    }

-- | The remote IP information of the connection.
ppdRemoteIPDetails :: Lens' PortProbeDetail (Maybe RemoteIPDetails)
ppdRemoteIPDetails = lens _ppdRemoteIPDetails (\s a -> s {_ppdRemoteIPDetails = a})

-- | The local IP information of the connection.
ppdLocalIPDetails :: Lens' PortProbeDetail (Maybe LocalIPDetails)
ppdLocalIPDetails = lens _ppdLocalIPDetails (\s a -> s {_ppdLocalIPDetails = a})

-- | The local port information of the connection.
ppdLocalPortDetails :: Lens' PortProbeDetail (Maybe LocalPortDetails)
ppdLocalPortDetails = lens _ppdLocalPortDetails (\s a -> s {_ppdLocalPortDetails = a})

instance FromJSON PortProbeDetail where
  parseJSON =
    withObject
      "PortProbeDetail"
      ( \x ->
          PortProbeDetail'
            <$> (x .:? "remoteIpDetails")
            <*> (x .:? "localIpDetails")
            <*> (x .:? "localPortDetails")
      )

instance Hashable PortProbeDetail

instance NFData PortProbeDetail
