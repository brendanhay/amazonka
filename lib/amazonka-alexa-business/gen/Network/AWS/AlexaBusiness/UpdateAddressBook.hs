{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates address book details by the address book ARN.
module Network.AWS.AlexaBusiness.UpdateAddressBook
  ( -- * Creating a request
    UpdateAddressBook (..),
    mkUpdateAddressBook,

    -- ** Request lenses
    uabAddressBookArn,
    uabDescription,
    uabName,

    -- * Destructuring the response
    UpdateAddressBookResponse (..),
    mkUpdateAddressBookResponse,

    -- ** Response lenses
    uabrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAddressBook' smart constructor.
data UpdateAddressBook = UpdateAddressBook'
  { -- | The ARN of the room to update.
    addressBookArn :: Types.AddressBookArn,
    -- | The updated description of the room.
    description :: Core.Maybe Types.Description,
    -- | The updated name of the room.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAddressBook' value with any optional fields omitted.
mkUpdateAddressBook ::
  -- | 'addressBookArn'
  Types.AddressBookArn ->
  UpdateAddressBook
mkUpdateAddressBook addressBookArn =
  UpdateAddressBook'
    { addressBookArn,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the room to update.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabAddressBookArn :: Lens.Lens' UpdateAddressBook Types.AddressBookArn
uabAddressBookArn = Lens.field @"addressBookArn"
{-# DEPRECATED uabAddressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead." #-}

-- | The updated description of the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabDescription :: Lens.Lens' UpdateAddressBook (Core.Maybe Types.Description)
uabDescription = Lens.field @"description"
{-# DEPRECATED uabDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated name of the room.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabName :: Lens.Lens' UpdateAddressBook (Core.Maybe Types.Name)
uabName = Lens.field @"name"
{-# DEPRECATED uabName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateAddressBook where
  toJSON UpdateAddressBook {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AddressBookArn" Core..= addressBookArn),
            ("Description" Core..=) Core.<$> description,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateAddressBook where
  type Rs UpdateAddressBook = UpdateAddressBookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateAddressBook")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAddressBookResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAddressBookResponse' smart constructor.
newtype UpdateAddressBookResponse = UpdateAddressBookResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAddressBookResponse' value with any optional fields omitted.
mkUpdateAddressBookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateAddressBookResponse
mkUpdateAddressBookResponse responseStatus =
  UpdateAddressBookResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabrrsResponseStatus :: Lens.Lens' UpdateAddressBookResponse Core.Int
uabrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uabrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
