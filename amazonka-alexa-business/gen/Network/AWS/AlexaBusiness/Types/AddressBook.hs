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
-- Module      : Network.AWS.AlexaBusiness.Types.AddressBook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.AddressBook where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An address book with attributes.
--
-- /See:/ 'newAddressBook' smart constructor.
data AddressBook = AddressBook'
  { -- | The ARN of the address book.
    addressBookArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the address book.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the address book.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressBookArn', 'addressBook_addressBookArn' - The ARN of the address book.
--
-- 'name', 'addressBook_name' - The name of the address book.
--
-- 'description', 'addressBook_description' - The description of the address book.
newAddressBook ::
  AddressBook
newAddressBook =
  AddressBook'
    { addressBookArn = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ARN of the address book.
addressBook_addressBookArn :: Lens.Lens' AddressBook (Prelude.Maybe Prelude.Text)
addressBook_addressBookArn = Lens.lens (\AddressBook' {addressBookArn} -> addressBookArn) (\s@AddressBook' {} a -> s {addressBookArn = a} :: AddressBook)

-- | The name of the address book.
addressBook_name :: Lens.Lens' AddressBook (Prelude.Maybe Prelude.Text)
addressBook_name = Lens.lens (\AddressBook' {name} -> name) (\s@AddressBook' {} a -> s {name = a} :: AddressBook)

-- | The description of the address book.
addressBook_description :: Lens.Lens' AddressBook (Prelude.Maybe Prelude.Text)
addressBook_description = Lens.lens (\AddressBook' {description} -> description) (\s@AddressBook' {} a -> s {description = a} :: AddressBook)

instance Prelude.FromJSON AddressBook where
  parseJSON =
    Prelude.withObject
      "AddressBook"
      ( \x ->
          AddressBook'
            Prelude.<$> (x Prelude..:? "AddressBookArn")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable AddressBook

instance Prelude.NFData AddressBook
