{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a contact with a given address book.
module Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
  ( -- * Creating a request
    AssociateContactWithAddressBook (..),
    mkAssociateContactWithAddressBook,

    -- ** Request lenses
    acwabContactArn,
    acwabAddressBookArn,

    -- * Destructuring the response
    AssociateContactWithAddressBookResponse (..),
    mkAssociateContactWithAddressBookResponse,

    -- ** Response lenses
    acwabrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateContactWithAddressBook' smart constructor.
data AssociateContactWithAddressBook = AssociateContactWithAddressBook'
  { -- | The ARN of the contact to associate with an address book.
    contactArn :: Types.Arn,
    -- | The ARN of the address book with which to associate the contact.
    addressBookArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateContactWithAddressBook' value with any optional fields omitted.
mkAssociateContactWithAddressBook ::
  -- | 'contactArn'
  Types.Arn ->
  -- | 'addressBookArn'
  Types.Arn ->
  AssociateContactWithAddressBook
mkAssociateContactWithAddressBook contactArn addressBookArn =
  AssociateContactWithAddressBook' {contactArn, addressBookArn}

-- | The ARN of the contact to associate with an address book.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabContactArn :: Lens.Lens' AssociateContactWithAddressBook Types.Arn
acwabContactArn = Lens.field @"contactArn"
{-# DEPRECATED acwabContactArn "Use generic-lens or generic-optics with 'contactArn' instead." #-}

-- | The ARN of the address book with which to associate the contact.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabAddressBookArn :: Lens.Lens' AssociateContactWithAddressBook Types.Arn
acwabAddressBookArn = Lens.field @"addressBookArn"
{-# DEPRECATED acwabAddressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead." #-}

instance Core.FromJSON AssociateContactWithAddressBook where
  toJSON AssociateContactWithAddressBook {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContactArn" Core..= contactArn),
            Core.Just ("AddressBookArn" Core..= addressBookArn)
          ]
      )

instance Core.AWSRequest AssociateContactWithAddressBook where
  type
    Rs AssociateContactWithAddressBook =
      AssociateContactWithAddressBookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AlexaForBusiness.AssociateContactWithAddressBook"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateContactWithAddressBookResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateContactWithAddressBookResponse' smart constructor.
newtype AssociateContactWithAddressBookResponse = AssociateContactWithAddressBookResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateContactWithAddressBookResponse' value with any optional fields omitted.
mkAssociateContactWithAddressBookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateContactWithAddressBookResponse
mkAssociateContactWithAddressBookResponse responseStatus =
  AssociateContactWithAddressBookResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabrrsResponseStatus :: Lens.Lens' AssociateContactWithAddressBookResponse Core.Int
acwabrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED acwabrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
