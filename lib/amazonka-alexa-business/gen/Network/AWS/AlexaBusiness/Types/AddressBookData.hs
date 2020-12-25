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
    abdAddressBookArn,
    abdDescription,
    abdName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.AddressBookDescription as Types
import qualified Network.AWS.AlexaBusiness.Types.AddressBookName as Types
import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information related to an address book.
--
-- /See:/ 'mkAddressBookData' smart constructor.
data AddressBookData = AddressBookData'
  { -- | The ARN of the address book.
    addressBookArn :: Core.Maybe Types.Arn,
    -- | The description of the address book.
    description :: Core.Maybe Types.AddressBookDescription,
    -- | The name of the address book.
    name :: Core.Maybe Types.AddressBookName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddressBookData' value with any optional fields omitted.
mkAddressBookData ::
  AddressBookData
mkAddressBookData =
  AddressBookData'
    { addressBookArn = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the address book.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abdAddressBookArn :: Lens.Lens' AddressBookData (Core.Maybe Types.Arn)
abdAddressBookArn = Lens.field @"addressBookArn"
{-# DEPRECATED abdAddressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead." #-}

-- | The description of the address book.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abdDescription :: Lens.Lens' AddressBookData (Core.Maybe Types.AddressBookDescription)
abdDescription = Lens.field @"description"
{-# DEPRECATED abdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the address book.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abdName :: Lens.Lens' AddressBookData (Core.Maybe Types.AddressBookName)
abdName = Lens.field @"name"
{-# DEPRECATED abdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON AddressBookData where
  parseJSON =
    Core.withObject "AddressBookData" Core.$
      \x ->
        AddressBookData'
          Core.<$> (x Core..:? "AddressBookArn")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Name")
