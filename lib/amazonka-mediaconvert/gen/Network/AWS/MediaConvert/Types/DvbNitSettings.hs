{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbNitSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbNitSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- /See:/ 'dvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { _dnsNetworkId ::
      !(Maybe Nat),
    _dnsNetworkName :: !(Maybe Text),
    _dnsNitInterval :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbNitSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnsNetworkId' - The numeric value placed in the Network Information Table (NIT).
--
-- * 'dnsNetworkName' - The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- * 'dnsNitInterval' - The number of milliseconds between instances of this table in the output transport stream.
dvbNitSettings ::
  DvbNitSettings
dvbNitSettings =
  DvbNitSettings'
    { _dnsNetworkId = Nothing,
      _dnsNetworkName = Nothing,
      _dnsNitInterval = Nothing
    }

-- | The numeric value placed in the Network Information Table (NIT).
dnsNetworkId :: Lens' DvbNitSettings (Maybe Natural)
dnsNetworkId = lens _dnsNetworkId (\s a -> s {_dnsNetworkId = a}) . mapping _Nat

-- | The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
dnsNetworkName :: Lens' DvbNitSettings (Maybe Text)
dnsNetworkName = lens _dnsNetworkName (\s a -> s {_dnsNetworkName = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
dnsNitInterval :: Lens' DvbNitSettings (Maybe Natural)
dnsNitInterval = lens _dnsNitInterval (\s a -> s {_dnsNitInterval = a}) . mapping _Nat

instance FromJSON DvbNitSettings where
  parseJSON =
    withObject
      "DvbNitSettings"
      ( \x ->
          DvbNitSettings'
            <$> (x .:? "networkId")
            <*> (x .:? "networkName")
            <*> (x .:? "nitInterval")
      )

instance Hashable DvbNitSettings

instance NFData DvbNitSettings

instance ToJSON DvbNitSettings where
  toJSON DvbNitSettings' {..} =
    object
      ( catMaybes
          [ ("networkId" .=) <$> _dnsNetworkId,
            ("networkName" .=) <$> _dnsNetworkName,
            ("nitInterval" .=) <$> _dnsNitInterval
          ]
      )
