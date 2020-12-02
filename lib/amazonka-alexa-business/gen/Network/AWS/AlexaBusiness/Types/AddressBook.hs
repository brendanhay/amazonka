{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.AddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.AddressBook where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An address book with attributes.
--
--
--
-- /See:/ 'addressBook' smart constructor.
data AddressBook = AddressBook'
  { _abAddressBookARN :: !(Maybe Text),
    _abName :: !(Maybe Text),
    _abDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abAddressBookARN' - The ARN of the address book.
--
-- * 'abName' - The name of the address book.
--
-- * 'abDescription' - The description of the address book.
addressBook ::
  AddressBook
addressBook =
  AddressBook'
    { _abAddressBookARN = Nothing,
      _abName = Nothing,
      _abDescription = Nothing
    }

-- | The ARN of the address book.
abAddressBookARN :: Lens' AddressBook (Maybe Text)
abAddressBookARN = lens _abAddressBookARN (\s a -> s {_abAddressBookARN = a})

-- | The name of the address book.
abName :: Lens' AddressBook (Maybe Text)
abName = lens _abName (\s a -> s {_abName = a})

-- | The description of the address book.
abDescription :: Lens' AddressBook (Maybe Text)
abDescription = lens _abDescription (\s a -> s {_abDescription = a})

instance FromJSON AddressBook where
  parseJSON =
    withObject
      "AddressBook"
      ( \x ->
          AddressBook'
            <$> (x .:? "AddressBookArn")
            <*> (x .:? "Name")
            <*> (x .:? "Description")
      )

instance Hashable AddressBook

instance NFData AddressBook
