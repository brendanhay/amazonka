{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventContextDataType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the user context data captured at the time of an event request.
--
--
--
-- /See:/ 'eventContextDataType' smart constructor.
data EventContextDataType = EventContextDataType'
  { _ecdtIPAddress ::
      !(Maybe Text),
    _ecdtCountry :: !(Maybe Text),
    _ecdtCity :: !(Maybe Text),
    _ecdtDeviceName :: !(Maybe Text),
    _ecdtTimezone :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventContextDataType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecdtIPAddress' - The user's IP address.
--
-- * 'ecdtCountry' - The user's country.
--
-- * 'ecdtCity' - The user's city.
--
-- * 'ecdtDeviceName' - The user's device name.
--
-- * 'ecdtTimezone' - The user's time zone.
eventContextDataType ::
  EventContextDataType
eventContextDataType =
  EventContextDataType'
    { _ecdtIPAddress = Nothing,
      _ecdtCountry = Nothing,
      _ecdtCity = Nothing,
      _ecdtDeviceName = Nothing,
      _ecdtTimezone = Nothing
    }

-- | The user's IP address.
ecdtIPAddress :: Lens' EventContextDataType (Maybe Text)
ecdtIPAddress = lens _ecdtIPAddress (\s a -> s {_ecdtIPAddress = a})

-- | The user's country.
ecdtCountry :: Lens' EventContextDataType (Maybe Text)
ecdtCountry = lens _ecdtCountry (\s a -> s {_ecdtCountry = a})

-- | The user's city.
ecdtCity :: Lens' EventContextDataType (Maybe Text)
ecdtCity = lens _ecdtCity (\s a -> s {_ecdtCity = a})

-- | The user's device name.
ecdtDeviceName :: Lens' EventContextDataType (Maybe Text)
ecdtDeviceName = lens _ecdtDeviceName (\s a -> s {_ecdtDeviceName = a})

-- | The user's time zone.
ecdtTimezone :: Lens' EventContextDataType (Maybe Text)
ecdtTimezone = lens _ecdtTimezone (\s a -> s {_ecdtTimezone = a})

instance FromJSON EventContextDataType where
  parseJSON =
    withObject
      "EventContextDataType"
      ( \x ->
          EventContextDataType'
            <$> (x .:? "IpAddress")
            <*> (x .:? "Country")
            <*> (x .:? "City")
            <*> (x .:? "DeviceName")
            <*> (x .:? "Timezone")
      )

instance Hashable EventContextDataType

instance NFData EventContextDataType
