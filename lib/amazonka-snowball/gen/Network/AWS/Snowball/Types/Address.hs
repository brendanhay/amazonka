{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Address where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The address that you want the Snow device(s) associated with a specific job to be shipped to. Addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. Although no individual elements of the @Address@ are required, if the address is invalid or unsupported, then an exception is thrown.
--
--
--
-- /See:/ 'address' smart constructor.
data Address = Address'
  { _aIsRestricted :: !(Maybe Bool),
    _aStreet3 :: !(Maybe Text),
    _aLandmark :: !(Maybe Text),
    _aPostalCode :: !(Maybe Text),
    _aCountry :: !(Maybe Text),
    _aStateOrProvince :: !(Maybe Text),
    _aStreet2 :: !(Maybe Text),
    _aAddressId :: !(Maybe Text),
    _aCity :: !(Maybe Text),
    _aPhoneNumber :: !(Maybe Text),
    _aCompany :: !(Maybe Text),
    _aName :: !(Maybe Text),
    _aPrefectureOrDistrict :: !(Maybe Text),
    _aStreet1 :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Address' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aIsRestricted' - If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
--
-- * 'aStreet3' - The third line in a street address that a Snow device is to be delivered to.
--
-- * 'aLandmark' - This field is no longer used and the value is ignored.
--
-- * 'aPostalCode' - The postal code in an address that a Snow device is to be delivered to.
--
-- * 'aCountry' - The country in an address that a Snow device is to be delivered to.
--
-- * 'aStateOrProvince' - The state or province in an address that a Snow device is to be delivered to.
--
-- * 'aStreet2' - The second line in a street address that a Snow device is to be delivered to.
--
-- * 'aAddressId' - The unique ID for an address.
--
-- * 'aCity' - The city in an address that a Snow device is to be delivered to.
--
-- * 'aPhoneNumber' - The phone number associated with an address that a Snow device is to be delivered to.
--
-- * 'aCompany' - The name of the company to receive a Snow device at an address.
--
-- * 'aName' - The name of a person to receive a Snow device at an address.
--
-- * 'aPrefectureOrDistrict' - This field is no longer used and the value is ignored.
--
-- * 'aStreet1' - The first line in a street address that a Snow device is to be delivered to.
address ::
  Address
address =
  Address'
    { _aIsRestricted = Nothing,
      _aStreet3 = Nothing,
      _aLandmark = Nothing,
      _aPostalCode = Nothing,
      _aCountry = Nothing,
      _aStateOrProvince = Nothing,
      _aStreet2 = Nothing,
      _aAddressId = Nothing,
      _aCity = Nothing,
      _aPhoneNumber = Nothing,
      _aCompany = Nothing,
      _aName = Nothing,
      _aPrefectureOrDistrict = Nothing,
      _aStreet1 = Nothing
    }

-- | If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
aIsRestricted :: Lens' Address (Maybe Bool)
aIsRestricted = lens _aIsRestricted (\s a -> s {_aIsRestricted = a})

-- | The third line in a street address that a Snow device is to be delivered to.
aStreet3 :: Lens' Address (Maybe Text)
aStreet3 = lens _aStreet3 (\s a -> s {_aStreet3 = a})

-- | This field is no longer used and the value is ignored.
aLandmark :: Lens' Address (Maybe Text)
aLandmark = lens _aLandmark (\s a -> s {_aLandmark = a})

-- | The postal code in an address that a Snow device is to be delivered to.
aPostalCode :: Lens' Address (Maybe Text)
aPostalCode = lens _aPostalCode (\s a -> s {_aPostalCode = a})

-- | The country in an address that a Snow device is to be delivered to.
aCountry :: Lens' Address (Maybe Text)
aCountry = lens _aCountry (\s a -> s {_aCountry = a})

-- | The state or province in an address that a Snow device is to be delivered to.
aStateOrProvince :: Lens' Address (Maybe Text)
aStateOrProvince = lens _aStateOrProvince (\s a -> s {_aStateOrProvince = a})

-- | The second line in a street address that a Snow device is to be delivered to.
aStreet2 :: Lens' Address (Maybe Text)
aStreet2 = lens _aStreet2 (\s a -> s {_aStreet2 = a})

-- | The unique ID for an address.
aAddressId :: Lens' Address (Maybe Text)
aAddressId = lens _aAddressId (\s a -> s {_aAddressId = a})

-- | The city in an address that a Snow device is to be delivered to.
aCity :: Lens' Address (Maybe Text)
aCity = lens _aCity (\s a -> s {_aCity = a})

-- | The phone number associated with an address that a Snow device is to be delivered to.
aPhoneNumber :: Lens' Address (Maybe Text)
aPhoneNumber = lens _aPhoneNumber (\s a -> s {_aPhoneNumber = a})

-- | The name of the company to receive a Snow device at an address.
aCompany :: Lens' Address (Maybe Text)
aCompany = lens _aCompany (\s a -> s {_aCompany = a})

-- | The name of a person to receive a Snow device at an address.
aName :: Lens' Address (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

-- | This field is no longer used and the value is ignored.
aPrefectureOrDistrict :: Lens' Address (Maybe Text)
aPrefectureOrDistrict = lens _aPrefectureOrDistrict (\s a -> s {_aPrefectureOrDistrict = a})

-- | The first line in a street address that a Snow device is to be delivered to.
aStreet1 :: Lens' Address (Maybe Text)
aStreet1 = lens _aStreet1 (\s a -> s {_aStreet1 = a})

instance FromJSON Address where
  parseJSON =
    withObject
      "Address"
      ( \x ->
          Address'
            <$> (x .:? "IsRestricted")
            <*> (x .:? "Street3")
            <*> (x .:? "Landmark")
            <*> (x .:? "PostalCode")
            <*> (x .:? "Country")
            <*> (x .:? "StateOrProvince")
            <*> (x .:? "Street2")
            <*> (x .:? "AddressId")
            <*> (x .:? "City")
            <*> (x .:? "PhoneNumber")
            <*> (x .:? "Company")
            <*> (x .:? "Name")
            <*> (x .:? "PrefectureOrDistrict")
            <*> (x .:? "Street1")
      )

instance Hashable Address

instance NFData Address

instance ToJSON Address where
  toJSON Address' {..} =
    object
      ( catMaybes
          [ ("IsRestricted" .=) <$> _aIsRestricted,
            ("Street3" .=) <$> _aStreet3,
            ("Landmark" .=) <$> _aLandmark,
            ("PostalCode" .=) <$> _aPostalCode,
            ("Country" .=) <$> _aCountry,
            ("StateOrProvince" .=) <$> _aStateOrProvince,
            ("Street2" .=) <$> _aStreet2,
            ("AddressId" .=) <$> _aAddressId,
            ("City" .=) <$> _aCity,
            ("PhoneNumber" .=) <$> _aPhoneNumber,
            ("Company" .=) <$> _aCompany,
            ("Name" .=) <$> _aName,
            ("PrefectureOrDistrict" .=) <$> _aPrefectureOrDistrict,
            ("Street1" .=) <$> _aStreet1
          ]
      )
