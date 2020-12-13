{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.AddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.AddressBook
  ( AddressBook (..),

    -- * Smart constructor
    mkAddressBook,

    -- * Lenses
    abAddressBookARN,
    abName,
    abDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An address book with attributes.
--
-- /See:/ 'mkAddressBook' smart constructor.
data AddressBook = AddressBook'
  { -- | The ARN of the address book.
    addressBookARN :: Lude.Maybe Lude.Text,
    -- | The name of the address book.
    name :: Lude.Maybe Lude.Text,
    -- | The description of the address book.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddressBook' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the address book.
-- * 'name' - The name of the address book.
-- * 'description' - The description of the address book.
mkAddressBook ::
  AddressBook
mkAddressBook =
  AddressBook'
    { addressBookARN = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the address book.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abAddressBookARN :: Lens.Lens' AddressBook (Lude.Maybe Lude.Text)
abAddressBookARN = Lens.lens (addressBookARN :: AddressBook -> Lude.Maybe Lude.Text) (\s a -> s {addressBookARN = a} :: AddressBook)
{-# DEPRECATED abAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

-- | The name of the address book.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abName :: Lens.Lens' AddressBook (Lude.Maybe Lude.Text)
abName = Lens.lens (name :: AddressBook -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AddressBook)
{-# DEPRECATED abName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the address book.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abDescription :: Lens.Lens' AddressBook (Lude.Maybe Lude.Text)
abDescription = Lens.lens (description :: AddressBook -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AddressBook)
{-# DEPRECATED abDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON AddressBook where
  parseJSON =
    Lude.withObject
      "AddressBook"
      ( \x ->
          AddressBook'
            Lude.<$> (x Lude..:? "AddressBookArn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
      )
