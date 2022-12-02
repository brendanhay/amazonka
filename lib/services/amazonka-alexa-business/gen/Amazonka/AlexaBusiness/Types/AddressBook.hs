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
-- Module      : Amazonka.AlexaBusiness.Types.AddressBook
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.AddressBook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An address book with attributes.
--
-- /See:/ 'newAddressBook' smart constructor.
data AddressBook = AddressBook'
  { -- | The name of the address book.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the address book.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the address book.
    addressBookArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddressBook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'addressBook_name' - The name of the address book.
--
-- 'description', 'addressBook_description' - The description of the address book.
--
-- 'addressBookArn', 'addressBook_addressBookArn' - The ARN of the address book.
newAddressBook ::
  AddressBook
newAddressBook =
  AddressBook'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      addressBookArn = Prelude.Nothing
    }

-- | The name of the address book.
addressBook_name :: Lens.Lens' AddressBook (Prelude.Maybe Prelude.Text)
addressBook_name = Lens.lens (\AddressBook' {name} -> name) (\s@AddressBook' {} a -> s {name = a} :: AddressBook)

-- | The description of the address book.
addressBook_description :: Lens.Lens' AddressBook (Prelude.Maybe Prelude.Text)
addressBook_description = Lens.lens (\AddressBook' {description} -> description) (\s@AddressBook' {} a -> s {description = a} :: AddressBook)

-- | The ARN of the address book.
addressBook_addressBookArn :: Lens.Lens' AddressBook (Prelude.Maybe Prelude.Text)
addressBook_addressBookArn = Lens.lens (\AddressBook' {addressBookArn} -> addressBookArn) (\s@AddressBook' {} a -> s {addressBookArn = a} :: AddressBook)

instance Data.FromJSON AddressBook where
  parseJSON =
    Data.withObject
      "AddressBook"
      ( \x ->
          AddressBook'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "AddressBookArn")
      )

instance Prelude.Hashable AddressBook where
  hashWithSalt _salt AddressBook' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` addressBookArn

instance Prelude.NFData AddressBook where
  rnf AddressBook' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf addressBookArn
