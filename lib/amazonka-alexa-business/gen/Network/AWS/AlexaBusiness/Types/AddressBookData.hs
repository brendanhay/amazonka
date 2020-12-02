{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.AddressBookData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.AddressBookData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information related to an address book.
--
--
--
-- /See:/ 'addressBookData' smart constructor.
data AddressBookData = AddressBookData'
  { _abdAddressBookARN ::
      !(Maybe Text),
    _abdName :: !(Maybe Text),
    _abdDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddressBookData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abdAddressBookARN' - The ARN of the address book.
--
-- * 'abdName' - The name of the address book.
--
-- * 'abdDescription' - The description of the address book.
addressBookData ::
  AddressBookData
addressBookData =
  AddressBookData'
    { _abdAddressBookARN = Nothing,
      _abdName = Nothing,
      _abdDescription = Nothing
    }

-- | The ARN of the address book.
abdAddressBookARN :: Lens' AddressBookData (Maybe Text)
abdAddressBookARN = lens _abdAddressBookARN (\s a -> s {_abdAddressBookARN = a})

-- | The name of the address book.
abdName :: Lens' AddressBookData (Maybe Text)
abdName = lens _abdName (\s a -> s {_abdName = a})

-- | The description of the address book.
abdDescription :: Lens' AddressBookData (Maybe Text)
abdDescription = lens _abdDescription (\s a -> s {_abdDescription = a})

instance FromJSON AddressBookData where
  parseJSON =
    withObject
      "AddressBookData"
      ( \x ->
          AddressBookData'
            <$> (x .:? "AddressBookArn")
            <*> (x .:? "Name")
            <*> (x .:? "Description")
      )

instance Hashable AddressBookData

instance NFData AddressBookData
