{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets address the book details by the address book ARN.
module Network.AWS.AlexaBusiness.GetAddressBook
  ( -- * Creating a request
    GetAddressBook (..),
    mkGetAddressBook,

    -- ** Request lenses
    gabAddressBookArn,

    -- * Destructuring the response
    GetAddressBookResponse (..),
    mkGetAddressBookResponse,

    -- ** Response lenses
    gabrrsAddressBook,
    gabrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAddressBook' smart constructor.
newtype GetAddressBook = GetAddressBook'
  { -- | The ARN of the address book for which to request details.
    addressBookArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAddressBook' value with any optional fields omitted.
mkGetAddressBook ::
  -- | 'addressBookArn'
  Types.Arn ->
  GetAddressBook
mkGetAddressBook addressBookArn = GetAddressBook' {addressBookArn}

-- | The ARN of the address book for which to request details.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabAddressBookArn :: Lens.Lens' GetAddressBook Types.Arn
gabAddressBookArn = Lens.field @"addressBookArn"
{-# DEPRECATED gabAddressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead." #-}

instance Core.FromJSON GetAddressBook where
  toJSON GetAddressBook {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AddressBookArn" Core..= addressBookArn)]
      )

instance Core.AWSRequest GetAddressBook where
  type Rs GetAddressBook = GetAddressBookResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.GetAddressBook")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAddressBookResponse'
            Core.<$> (x Core..:? "AddressBook") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAddressBookResponse' smart constructor.
data GetAddressBookResponse = GetAddressBookResponse'
  { -- | The details of the requested address book.
    addressBook :: Core.Maybe Types.AddressBook,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAddressBookResponse' value with any optional fields omitted.
mkGetAddressBookResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAddressBookResponse
mkGetAddressBookResponse responseStatus =
  GetAddressBookResponse'
    { addressBook = Core.Nothing,
      responseStatus
    }

-- | The details of the requested address book.
--
-- /Note:/ Consider using 'addressBook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrrsAddressBook :: Lens.Lens' GetAddressBookResponse (Core.Maybe Types.AddressBook)
gabrrsAddressBook = Lens.field @"addressBook"
{-# DEPRECATED gabrrsAddressBook "Use generic-lens or generic-optics with 'addressBook' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrrsResponseStatus :: Lens.Lens' GetAddressBookResponse Core.Int
gabrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gabrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
