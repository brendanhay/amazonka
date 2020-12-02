{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.NumberValidateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.NumberValidateResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a phone number.
--
--
--
-- /See:/ 'numberValidateResponse' smart constructor.
data NumberValidateResponse = NumberValidateResponse'
  { _nvCarrier ::
      !(Maybe Text),
    _nvCounty :: !(Maybe Text),
    _nvCountry :: !(Maybe Text),
    _nvCountryCodeNumeric :: !(Maybe Text),
    _nvZipCode :: !(Maybe Text),
    _nvOriginalPhoneNumber :: !(Maybe Text),
    _nvPhoneTypeCode :: !(Maybe Int),
    _nvPhoneType :: !(Maybe Text),
    _nvCity :: !(Maybe Text),
    _nvCountryCodeIso2 :: !(Maybe Text),
    _nvTimezone :: !(Maybe Text),
    _nvOriginalCountryCodeIso2 :: !(Maybe Text),
    _nvCleansedPhoneNumberNational ::
      !(Maybe Text),
    _nvCleansedPhoneNumberE164 :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NumberValidateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nvCarrier' - The carrier or service provider that the phone number is currently registered with. In some countries and regions, this value may be the carrier or service provider that the phone number was originally registered with.
--
-- * 'nvCounty' - The name of the county where the phone number was originally registered.
--
-- * 'nvCountry' - The name of the country or region where the phone number was originally registered.
--
-- * 'nvCountryCodeNumeric' - The numeric code for the country or region where the phone number was originally registered.
--
-- * 'nvZipCode' - The postal or ZIP code for the location where the phone number was originally registered.
--
-- * 'nvOriginalPhoneNumber' - The phone number that was sent in the request body.
--
-- * 'nvPhoneTypeCode' - The phone type, represented by an integer. Valid values are: 0 (mobile), 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
--
-- * 'nvPhoneType' - The description of the phone type. Valid values are: MOBILE, LANDLINE, VOIP,                   INVALID, PREPAID, and OTHER.
--
-- * 'nvCity' - The name of the city where the phone number was originally registered.
--
-- * 'nvCountryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
--
-- * 'nvTimezone' - The time zone for the location where the phone number was originally registered.
--
-- * 'nvOriginalCountryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, that was sent in the request body.
--
-- * 'nvCleansedPhoneNumberNational' - The cleansed phone number, in the format for the location where the phone number was originally registered.
--
-- * 'nvCleansedPhoneNumberE164' - The cleansed phone number, in E.164 format, for the location where the phone number was originally registered.
numberValidateResponse ::
  NumberValidateResponse
numberValidateResponse =
  NumberValidateResponse'
    { _nvCarrier = Nothing,
      _nvCounty = Nothing,
      _nvCountry = Nothing,
      _nvCountryCodeNumeric = Nothing,
      _nvZipCode = Nothing,
      _nvOriginalPhoneNumber = Nothing,
      _nvPhoneTypeCode = Nothing,
      _nvPhoneType = Nothing,
      _nvCity = Nothing,
      _nvCountryCodeIso2 = Nothing,
      _nvTimezone = Nothing,
      _nvOriginalCountryCodeIso2 = Nothing,
      _nvCleansedPhoneNumberNational = Nothing,
      _nvCleansedPhoneNumberE164 = Nothing
    }

-- | The carrier or service provider that the phone number is currently registered with. In some countries and regions, this value may be the carrier or service provider that the phone number was originally registered with.
nvCarrier :: Lens' NumberValidateResponse (Maybe Text)
nvCarrier = lens _nvCarrier (\s a -> s {_nvCarrier = a})

-- | The name of the county where the phone number was originally registered.
nvCounty :: Lens' NumberValidateResponse (Maybe Text)
nvCounty = lens _nvCounty (\s a -> s {_nvCounty = a})

-- | The name of the country or region where the phone number was originally registered.
nvCountry :: Lens' NumberValidateResponse (Maybe Text)
nvCountry = lens _nvCountry (\s a -> s {_nvCountry = a})

-- | The numeric code for the country or region where the phone number was originally registered.
nvCountryCodeNumeric :: Lens' NumberValidateResponse (Maybe Text)
nvCountryCodeNumeric = lens _nvCountryCodeNumeric (\s a -> s {_nvCountryCodeNumeric = a})

-- | The postal or ZIP code for the location where the phone number was originally registered.
nvZipCode :: Lens' NumberValidateResponse (Maybe Text)
nvZipCode = lens _nvZipCode (\s a -> s {_nvZipCode = a})

-- | The phone number that was sent in the request body.
nvOriginalPhoneNumber :: Lens' NumberValidateResponse (Maybe Text)
nvOriginalPhoneNumber = lens _nvOriginalPhoneNumber (\s a -> s {_nvOriginalPhoneNumber = a})

-- | The phone type, represented by an integer. Valid values are: 0 (mobile), 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
nvPhoneTypeCode :: Lens' NumberValidateResponse (Maybe Int)
nvPhoneTypeCode = lens _nvPhoneTypeCode (\s a -> s {_nvPhoneTypeCode = a})

-- | The description of the phone type. Valid values are: MOBILE, LANDLINE, VOIP,                   INVALID, PREPAID, and OTHER.
nvPhoneType :: Lens' NumberValidateResponse (Maybe Text)
nvPhoneType = lens _nvPhoneType (\s a -> s {_nvPhoneType = a})

-- | The name of the city where the phone number was originally registered.
nvCity :: Lens' NumberValidateResponse (Maybe Text)
nvCity = lens _nvCity (\s a -> s {_nvCity = a})

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
nvCountryCodeIso2 :: Lens' NumberValidateResponse (Maybe Text)
nvCountryCodeIso2 = lens _nvCountryCodeIso2 (\s a -> s {_nvCountryCodeIso2 = a})

-- | The time zone for the location where the phone number was originally registered.
nvTimezone :: Lens' NumberValidateResponse (Maybe Text)
nvTimezone = lens _nvTimezone (\s a -> s {_nvTimezone = a})

-- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in the request body.
nvOriginalCountryCodeIso2 :: Lens' NumberValidateResponse (Maybe Text)
nvOriginalCountryCodeIso2 = lens _nvOriginalCountryCodeIso2 (\s a -> s {_nvOriginalCountryCodeIso2 = a})

-- | The cleansed phone number, in the format for the location where the phone number was originally registered.
nvCleansedPhoneNumberNational :: Lens' NumberValidateResponse (Maybe Text)
nvCleansedPhoneNumberNational = lens _nvCleansedPhoneNumberNational (\s a -> s {_nvCleansedPhoneNumberNational = a})

-- | The cleansed phone number, in E.164 format, for the location where the phone number was originally registered.
nvCleansedPhoneNumberE164 :: Lens' NumberValidateResponse (Maybe Text)
nvCleansedPhoneNumberE164 = lens _nvCleansedPhoneNumberE164 (\s a -> s {_nvCleansedPhoneNumberE164 = a})

instance FromJSON NumberValidateResponse where
  parseJSON =
    withObject
      "NumberValidateResponse"
      ( \x ->
          NumberValidateResponse'
            <$> (x .:? "Carrier")
            <*> (x .:? "County")
            <*> (x .:? "Country")
            <*> (x .:? "CountryCodeNumeric")
            <*> (x .:? "ZipCode")
            <*> (x .:? "OriginalPhoneNumber")
            <*> (x .:? "PhoneTypeCode")
            <*> (x .:? "PhoneType")
            <*> (x .:? "City")
            <*> (x .:? "CountryCodeIso2")
            <*> (x .:? "Timezone")
            <*> (x .:? "OriginalCountryCodeIso2")
            <*> (x .:? "CleansedPhoneNumberNational")
            <*> (x .:? "CleansedPhoneNumberE164")
      )

instance Hashable NumberValidateResponse

instance NFData NumberValidateResponse
