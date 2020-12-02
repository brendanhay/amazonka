{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PSTNDialIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PSTNDialIn where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The information for public switched telephone network (PSTN) conferencing.
--
--
--
-- /See:/ 'pSTNDialIn' smart constructor.
data PSTNDialIn = PSTNDialIn'
  { _pstndiCountryCode :: !Text,
    _pstndiPhoneNumber :: !Text,
    _pstndiOneClickIdDelay :: !Text,
    _pstndiOneClickPinDelay :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PSTNDialIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pstndiCountryCode' - The zip code.
--
-- * 'pstndiPhoneNumber' - The phone number to call to join the conference.
--
-- * 'pstndiOneClickIdDelay' - The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
--
-- * 'pstndiOneClickPinDelay' - The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
pSTNDialIn ::
  -- | 'pstndiCountryCode'
  Text ->
  -- | 'pstndiPhoneNumber'
  Text ->
  -- | 'pstndiOneClickIdDelay'
  Text ->
  -- | 'pstndiOneClickPinDelay'
  Text ->
  PSTNDialIn
pSTNDialIn
  pCountryCode_
  pPhoneNumber_
  pOneClickIdDelay_
  pOneClickPinDelay_ =
    PSTNDialIn'
      { _pstndiCountryCode = pCountryCode_,
        _pstndiPhoneNumber = pPhoneNumber_,
        _pstndiOneClickIdDelay = pOneClickIdDelay_,
        _pstndiOneClickPinDelay = pOneClickPinDelay_
      }

-- | The zip code.
pstndiCountryCode :: Lens' PSTNDialIn Text
pstndiCountryCode = lens _pstndiCountryCode (\s a -> s {_pstndiCountryCode = a})

-- | The phone number to call to join the conference.
pstndiPhoneNumber :: Lens' PSTNDialIn Text
pstndiPhoneNumber = lens _pstndiPhoneNumber (\s a -> s {_pstndiPhoneNumber = a})

-- | The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
pstndiOneClickIdDelay :: Lens' PSTNDialIn Text
pstndiOneClickIdDelay = lens _pstndiOneClickIdDelay (\s a -> s {_pstndiOneClickIdDelay = a})

-- | The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
pstndiOneClickPinDelay :: Lens' PSTNDialIn Text
pstndiOneClickPinDelay = lens _pstndiOneClickPinDelay (\s a -> s {_pstndiOneClickPinDelay = a})

instance FromJSON PSTNDialIn where
  parseJSON =
    withObject
      "PSTNDialIn"
      ( \x ->
          PSTNDialIn'
            <$> (x .: "CountryCode")
            <*> (x .: "PhoneNumber")
            <*> (x .: "OneClickIdDelay")
            <*> (x .: "OneClickPinDelay")
      )

instance Hashable PSTNDialIn

instance NFData PSTNDialIn

instance ToJSON PSTNDialIn where
  toJSON PSTNDialIn' {..} =
    object
      ( catMaybes
          [ Just ("CountryCode" .= _pstndiCountryCode),
            Just ("PhoneNumber" .= _pstndiPhoneNumber),
            Just ("OneClickIdDelay" .= _pstndiOneClickIdDelay),
            Just ("OneClickPinDelay" .= _pstndiOneClickPinDelay)
          ]
      )
