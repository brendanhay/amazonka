{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbNitSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbNitSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | DVB Network Information Table (NIT)
--
-- /See:/ 'dvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { _dnsRepInterval ::
      !(Maybe Nat),
    _dnsNetworkName :: !Text,
    _dnsNetworkId :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbNitSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnsRepInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'dnsNetworkName' - The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- * 'dnsNetworkId' - The numeric value placed in the Network Information Table (NIT).
dvbNitSettings ::
  -- | 'dnsNetworkName'
  Text ->
  -- | 'dnsNetworkId'
  Natural ->
  DvbNitSettings
dvbNitSettings pNetworkName_ pNetworkId_ =
  DvbNitSettings'
    { _dnsRepInterval = Nothing,
      _dnsNetworkName = pNetworkName_,
      _dnsNetworkId = _Nat # pNetworkId_
    }

-- | The number of milliseconds between instances of this table in the output transport stream.
dnsRepInterval :: Lens' DvbNitSettings (Maybe Natural)
dnsRepInterval = lens _dnsRepInterval (\s a -> s {_dnsRepInterval = a}) . mapping _Nat

-- | The network name text placed in the networkNameDescriptor inside the Network Information Table. Maximum length is 256 characters.
dnsNetworkName :: Lens' DvbNitSettings Text
dnsNetworkName = lens _dnsNetworkName (\s a -> s {_dnsNetworkName = a})

-- | The numeric value placed in the Network Information Table (NIT).
dnsNetworkId :: Lens' DvbNitSettings Natural
dnsNetworkId = lens _dnsNetworkId (\s a -> s {_dnsNetworkId = a}) . _Nat

instance FromJSON DvbNitSettings where
  parseJSON =
    withObject
      "DvbNitSettings"
      ( \x ->
          DvbNitSettings'
            <$> (x .:? "repInterval")
            <*> (x .: "networkName")
            <*> (x .: "networkId")
      )

instance Hashable DvbNitSettings

instance NFData DvbNitSettings

instance ToJSON DvbNitSettings where
  toJSON DvbNitSettings' {..} =
    object
      ( catMaybes
          [ ("repInterval" .=) <$> _dnsRepInterval,
            Just ("networkName" .= _dnsNetworkName),
            Just ("networkId" .= _dnsNetworkId)
          ]
      )
