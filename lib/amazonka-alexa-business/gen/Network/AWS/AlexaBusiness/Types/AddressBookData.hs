{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.AddressBookData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.AddressBookData
  ( AddressBookData (..),

    -- * Smart constructor
    mkAddressBookData,

    -- * Lenses
    abdAddressBookARN,
    abdName,
    abdDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information related to an address book.
--
-- /See:/ 'mkAddressBookData' smart constructor.
data AddressBookData = AddressBookData'
  { -- | The ARN of the address book.
    addressBookARN :: Lude.Maybe Lude.Text,
    -- | The name of the address book.
    name :: Lude.Maybe Lude.Text,
    -- | The description of the address book.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddressBookData' with the minimum fields required to make a request.
--
-- * 'addressBookARN' - The ARN of the address book.
-- * 'name' - The name of the address book.
-- * 'description' - The description of the address book.
mkAddressBookData ::
  AddressBookData
mkAddressBookData =
  AddressBookData'
    { addressBookARN = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the address book.
--
-- /Note:/ Consider using 'addressBookARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abdAddressBookARN :: Lens.Lens' AddressBookData (Lude.Maybe Lude.Text)
abdAddressBookARN = Lens.lens (addressBookARN :: AddressBookData -> Lude.Maybe Lude.Text) (\s a -> s {addressBookARN = a} :: AddressBookData)
{-# DEPRECATED abdAddressBookARN "Use generic-lens or generic-optics with 'addressBookARN' instead." #-}

-- | The name of the address book.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abdName :: Lens.Lens' AddressBookData (Lude.Maybe Lude.Text)
abdName = Lens.lens (name :: AddressBookData -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AddressBookData)
{-# DEPRECATED abdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the address book.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abdDescription :: Lens.Lens' AddressBookData (Lude.Maybe Lude.Text)
abdDescription = Lens.lens (description :: AddressBookData -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AddressBookData)
{-# DEPRECATED abdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON AddressBookData where
  parseJSON =
    Lude.withObject
      "AddressBookData"
      ( \x ->
          AddressBookData'
            Lude.<$> (x Lude..:? "AddressBookArn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
      )
