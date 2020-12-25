{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an address book by the address book ARN.
module Network.AWS.AlexaBusiness.DeleteAddressBook
  ( -- * Creating a request
    DeleteAddressBook (..),
    mkDeleteAddressBook,

    -- ** Request lenses
    dabAddressBookArn,

    -- * Destructuring the response
    DeleteAddressBookResponse (..),
    mkDeleteAddressBookResponse,

    -- ** Response lenses
    dabrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAddressBook' smart constructor.
newtype DeleteAddressBook = DeleteAddressBook'
  { -- | The ARN of the address book to delete.
    addressBookArn :: Types.AddressBookArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAddressBook' value with any optional fields omitted.
mkDeleteAddressBook ::
  -- | 'addressBookArn'
  Types.AddressBookArn ->
  DeleteAddressBook
mkDeleteAddressBook addressBookArn =
  DeleteAddressBook' {addressBookArn}

-- | The ARN of the address book to delete.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dabAddressBookArn :: Lens.Lens' DeleteAddressBook Types.AddressBookArn
dabAddressBookArn = Lens.field @"addressBookArn"
{-# DEPRECATED dabAddressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead." #-}

instance Core.FromJSON DeleteAddressBook where
  toJSON DeleteAddressBook {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AddressBookArn" Core..= addressBookArn)]
      )

instance Core.AWSRequest DeleteAddressBook where
  type Rs DeleteAddressBook = DeleteAddressBookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteAddressBook")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAddressBookResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAddressBookResponse' smart constructor.
newtype DeleteAddressBookResponse = DeleteAddressBookResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAddressBookResponse' value with any optional fields omitted.
mkDeleteAddressBookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAddressBookResponse
mkDeleteAddressBookResponse responseStatus =
  DeleteAddressBookResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dabrrsResponseStatus :: Lens.Lens' DeleteAddressBookResponse Core.Int
dabrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dabrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
