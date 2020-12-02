{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpContainerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpContainerSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.M2tsSettings
import Network.AWS.Prelude

-- | Udp Container Settings
--
-- /See:/ 'udpContainerSettings' smart constructor.
newtype UdpContainerSettings = UdpContainerSettings'
  { _ucsM2tsSettings ::
      Maybe M2tsSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UdpContainerSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsM2tsSettings' - Undocumented member.
udpContainerSettings ::
  UdpContainerSettings
udpContainerSettings =
  UdpContainerSettings' {_ucsM2tsSettings = Nothing}

-- | Undocumented member.
ucsM2tsSettings :: Lens' UdpContainerSettings (Maybe M2tsSettings)
ucsM2tsSettings = lens _ucsM2tsSettings (\s a -> s {_ucsM2tsSettings = a})

instance FromJSON UdpContainerSettings where
  parseJSON =
    withObject
      "UdpContainerSettings"
      (\x -> UdpContainerSettings' <$> (x .:? "m2tsSettings"))

instance Hashable UdpContainerSettings

instance NFData UdpContainerSettings

instance ToJSON UdpContainerSettings where
  toJSON UdpContainerSettings' {..} =
    object (catMaybes [("m2tsSettings" .=) <$> _ucsM2tsSettings])
