{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.AddressBookData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.AddressBookData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information related to an address book.
--
-- /See:/ 'newAddressBookData' smart constructor.
data AddressBookData = AddressBookData'
  { -- | The ARN of the address book.
    addressBookArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the address book.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the address book.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddressBookData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressBookArn', 'addressBookData_addressBookArn' - The ARN of the address book.
--
-- 'name', 'addressBookData_name' - The name of the address book.
--
-- 'description', 'addressBookData_description' - The description of the address book.
newAddressBookData ::
  AddressBookData
newAddressBookData =
  AddressBookData'
    { addressBookArn = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ARN of the address book.
addressBookData_addressBookArn :: Lens.Lens' AddressBookData (Prelude.Maybe Prelude.Text)
addressBookData_addressBookArn = Lens.lens (\AddressBookData' {addressBookArn} -> addressBookArn) (\s@AddressBookData' {} a -> s {addressBookArn = a} :: AddressBookData)

-- | The name of the address book.
addressBookData_name :: Lens.Lens' AddressBookData (Prelude.Maybe Prelude.Text)
addressBookData_name = Lens.lens (\AddressBookData' {name} -> name) (\s@AddressBookData' {} a -> s {name = a} :: AddressBookData)

-- | The description of the address book.
addressBookData_description :: Lens.Lens' AddressBookData (Prelude.Maybe Prelude.Text)
addressBookData_description = Lens.lens (\AddressBookData' {description} -> description) (\s@AddressBookData' {} a -> s {description = a} :: AddressBookData)

instance Prelude.FromJSON AddressBookData where
  parseJSON =
    Prelude.withObject
      "AddressBookData"
      ( \x ->
          AddressBookData'
            Prelude.<$> (x Prelude..:? "AddressBookArn")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable AddressBookData

instance Prelude.NFData AddressBookData
