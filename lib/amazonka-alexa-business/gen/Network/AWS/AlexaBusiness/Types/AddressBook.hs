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
    abAddressBookArn,
    abDescription,
    abName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.AddressBookDescription as Types
import qualified Network.AWS.AlexaBusiness.Types.AddressBookName as Types
import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An address book with attributes.
--
-- /See:/ 'mkAddressBook' smart constructor.
data AddressBook = AddressBook'
  { -- | The ARN of the address book.
    addressBookArn :: Core.Maybe Types.Arn,
    -- | The description of the address book.
    description :: Core.Maybe Types.AddressBookDescription,
    -- | The name of the address book.
    name :: Core.Maybe Types.AddressBookName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddressBook' value with any optional fields omitted.
mkAddressBook ::
  AddressBook
mkAddressBook =
  AddressBook'
    { addressBookArn = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the address book.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abAddressBookArn :: Lens.Lens' AddressBook (Core.Maybe Types.Arn)
abAddressBookArn = Lens.field @"addressBookArn"
{-# DEPRECATED abAddressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead." #-}

-- | The description of the address book.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abDescription :: Lens.Lens' AddressBook (Core.Maybe Types.AddressBookDescription)
abDescription = Lens.field @"description"
{-# DEPRECATED abDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the address book.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abName :: Lens.Lens' AddressBook (Core.Maybe Types.AddressBookName)
abName = Lens.field @"name"
{-# DEPRECATED abName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON AddressBook where
  parseJSON =
    Core.withObject "AddressBook" Core.$
      \x ->
        AddressBook'
          Core.<$> (x Core..:? "AddressBookArn")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Name")
