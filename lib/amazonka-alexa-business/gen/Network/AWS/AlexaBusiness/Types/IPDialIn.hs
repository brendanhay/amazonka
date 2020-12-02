{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.IPDialIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.IPDialIn where

import Network.AWS.AlexaBusiness.Types.CommsProtocol
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The IP endpoint and protocol for calling.
--
--
--
-- /See:/ 'ipDialIn' smart constructor.
data IPDialIn = IPDialIn'
  { _idiEndpoint :: !Text,
    _idiCommsProtocol :: !CommsProtocol
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPDialIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idiEndpoint' - The IP address.
--
-- * 'idiCommsProtocol' - The protocol, including SIP, SIPS, and H323.
ipDialIn ::
  -- | 'idiEndpoint'
  Text ->
  -- | 'idiCommsProtocol'
  CommsProtocol ->
  IPDialIn
ipDialIn pEndpoint_ pCommsProtocol_ =
  IPDialIn'
    { _idiEndpoint = pEndpoint_,
      _idiCommsProtocol = pCommsProtocol_
    }

-- | The IP address.
idiEndpoint :: Lens' IPDialIn Text
idiEndpoint = lens _idiEndpoint (\s a -> s {_idiEndpoint = a})

-- | The protocol, including SIP, SIPS, and H323.
idiCommsProtocol :: Lens' IPDialIn CommsProtocol
idiCommsProtocol = lens _idiCommsProtocol (\s a -> s {_idiCommsProtocol = a})

instance FromJSON IPDialIn where
  parseJSON =
    withObject
      "IPDialIn"
      (\x -> IPDialIn' <$> (x .: "Endpoint") <*> (x .: "CommsProtocol"))

instance Hashable IPDialIn

instance NFData IPDialIn

instance ToJSON IPDialIn where
  toJSON IPDialIn' {..} =
    object
      ( catMaybes
          [ Just ("Endpoint" .= _idiEndpoint),
            Just ("CommsProtocol" .= _idiCommsProtocol)
          ]
      )
