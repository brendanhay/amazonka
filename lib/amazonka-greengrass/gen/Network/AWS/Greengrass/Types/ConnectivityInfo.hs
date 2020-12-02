{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ConnectivityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConnectivityInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a Greengrass core's connectivity.
--
-- /See:/ 'connectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { _ciPortNumber ::
      !(Maybe Int),
    _ciId :: !(Maybe Text),
    _ciMetadata :: !(Maybe Text),
    _ciHostAddress :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConnectivityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciPortNumber' - The port of the Greengrass core. Usually 8883.
--
-- * 'ciId' - The ID of the connectivity information.
--
-- * 'ciMetadata' - Metadata for this endpoint.
--
-- * 'ciHostAddress' - The endpoint for the Greengrass core. Can be an IP address or DNS.
connectivityInfo ::
  ConnectivityInfo
connectivityInfo =
  ConnectivityInfo'
    { _ciPortNumber = Nothing,
      _ciId = Nothing,
      _ciMetadata = Nothing,
      _ciHostAddress = Nothing
    }

-- | The port of the Greengrass core. Usually 8883.
ciPortNumber :: Lens' ConnectivityInfo (Maybe Int)
ciPortNumber = lens _ciPortNumber (\s a -> s {_ciPortNumber = a})

-- | The ID of the connectivity information.
ciId :: Lens' ConnectivityInfo (Maybe Text)
ciId = lens _ciId (\s a -> s {_ciId = a})

-- | Metadata for this endpoint.
ciMetadata :: Lens' ConnectivityInfo (Maybe Text)
ciMetadata = lens _ciMetadata (\s a -> s {_ciMetadata = a})

-- | The endpoint for the Greengrass core. Can be an IP address or DNS.
ciHostAddress :: Lens' ConnectivityInfo (Maybe Text)
ciHostAddress = lens _ciHostAddress (\s a -> s {_ciHostAddress = a})

instance FromJSON ConnectivityInfo where
  parseJSON =
    withObject
      "ConnectivityInfo"
      ( \x ->
          ConnectivityInfo'
            <$> (x .:? "PortNumber")
            <*> (x .:? "Id")
            <*> (x .:? "Metadata")
            <*> (x .:? "HostAddress")
      )

instance Hashable ConnectivityInfo

instance NFData ConnectivityInfo

instance ToJSON ConnectivityInfo where
  toJSON ConnectivityInfo' {..} =
    object
      ( catMaybes
          [ ("PortNumber" .=) <$> _ciPortNumber,
            ("Id" .=) <$> _ciId,
            ("Metadata" .=) <$> _ciMetadata,
            ("HostAddress" .=) <$> _ciHostAddress
          ]
      )
