{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberSummary where

import Network.AWS.Connect.Types.PhoneNumberCountryCode
import Network.AWS.Connect.Types.PhoneNumberType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about a phone number for a contact center.
--
--
--
-- /See:/ 'phoneNumberSummary' smart constructor.
data PhoneNumberSummary = PhoneNumberSummary'
  { _pnsPhoneNumberType ::
      !(Maybe PhoneNumberType),
    _pnsARN :: !(Maybe Text),
    _pnsPhoneNumber :: !(Maybe Text),
    _pnsPhoneNumberCountryCode ::
      !(Maybe PhoneNumberCountryCode),
    _pnsId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PhoneNumberSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnsPhoneNumberType' - The type of phone number.
--
-- * 'pnsARN' - The Amazon Resource Name (ARN) of the phone number.
--
-- * 'pnsPhoneNumber' - The phone number.
--
-- * 'pnsPhoneNumberCountryCode' - The ISO country code.
--
-- * 'pnsId' - The identifier of the phone number.
phoneNumberSummary ::
  PhoneNumberSummary
phoneNumberSummary =
  PhoneNumberSummary'
    { _pnsPhoneNumberType = Nothing,
      _pnsARN = Nothing,
      _pnsPhoneNumber = Nothing,
      _pnsPhoneNumberCountryCode = Nothing,
      _pnsId = Nothing
    }

-- | The type of phone number.
pnsPhoneNumberType :: Lens' PhoneNumberSummary (Maybe PhoneNumberType)
pnsPhoneNumberType = lens _pnsPhoneNumberType (\s a -> s {_pnsPhoneNumberType = a})

-- | The Amazon Resource Name (ARN) of the phone number.
pnsARN :: Lens' PhoneNumberSummary (Maybe Text)
pnsARN = lens _pnsARN (\s a -> s {_pnsARN = a})

-- | The phone number.
pnsPhoneNumber :: Lens' PhoneNumberSummary (Maybe Text)
pnsPhoneNumber = lens _pnsPhoneNumber (\s a -> s {_pnsPhoneNumber = a})

-- | The ISO country code.
pnsPhoneNumberCountryCode :: Lens' PhoneNumberSummary (Maybe PhoneNumberCountryCode)
pnsPhoneNumberCountryCode = lens _pnsPhoneNumberCountryCode (\s a -> s {_pnsPhoneNumberCountryCode = a})

-- | The identifier of the phone number.
pnsId :: Lens' PhoneNumberSummary (Maybe Text)
pnsId = lens _pnsId (\s a -> s {_pnsId = a})

instance FromJSON PhoneNumberSummary where
  parseJSON =
    withObject
      "PhoneNumberSummary"
      ( \x ->
          PhoneNumberSummary'
            <$> (x .:? "PhoneNumberType")
            <*> (x .:? "Arn")
            <*> (x .:? "PhoneNumber")
            <*> (x .:? "PhoneNumberCountryCode")
            <*> (x .:? "Id")
      )

instance Hashable PhoneNumberSummary

instance NFData PhoneNumberSummary
