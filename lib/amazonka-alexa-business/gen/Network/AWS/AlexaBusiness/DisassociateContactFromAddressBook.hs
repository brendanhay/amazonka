{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a contact from a given address book.
module Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
    (
    -- * Creating a request
      DisassociateContactFromAddressBook (..)
    , mkDisassociateContactFromAddressBook
    -- ** Request lenses
    , dcfabContactArn
    , dcfabAddressBookArn

    -- * Destructuring the response
    , DisassociateContactFromAddressBookResponse (..)
    , mkDisassociateContactFromAddressBookResponse
    -- ** Response lenses
    , dcfabrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateContactFromAddressBook' smart constructor.
data DisassociateContactFromAddressBook = DisassociateContactFromAddressBook'
  { contactArn :: Types.ContactArn
    -- ^ The ARN of the contact to disassociate from an address book.
  , addressBookArn :: Types.AddressBookArn
    -- ^ The ARN of the address from which to disassociate the contact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateContactFromAddressBook' value with any optional fields omitted.
mkDisassociateContactFromAddressBook
    :: Types.ContactArn -- ^ 'contactArn'
    -> Types.AddressBookArn -- ^ 'addressBookArn'
    -> DisassociateContactFromAddressBook
mkDisassociateContactFromAddressBook contactArn addressBookArn
  = DisassociateContactFromAddressBook'{contactArn, addressBookArn}

-- | The ARN of the contact to disassociate from an address book.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfabContactArn :: Lens.Lens' DisassociateContactFromAddressBook Types.ContactArn
dcfabContactArn = Lens.field @"contactArn"
{-# INLINEABLE dcfabContactArn #-}
{-# DEPRECATED contactArn "Use generic-lens or generic-optics with 'contactArn' instead"  #-}

-- | The ARN of the address from which to disassociate the contact.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfabAddressBookArn :: Lens.Lens' DisassociateContactFromAddressBook Types.AddressBookArn
dcfabAddressBookArn = Lens.field @"addressBookArn"
{-# INLINEABLE dcfabAddressBookArn #-}
{-# DEPRECATED addressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead"  #-}

instance Core.ToQuery DisassociateContactFromAddressBook where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateContactFromAddressBook where
        toHeaders DisassociateContactFromAddressBook{..}
          = Core.pure
              ("X-Amz-Target",
               "AlexaForBusiness.DisassociateContactFromAddressBook")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateContactFromAddressBook where
        toJSON DisassociateContactFromAddressBook{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContactArn" Core..= contactArn),
                  Core.Just ("AddressBookArn" Core..= addressBookArn)])

instance Core.AWSRequest DisassociateContactFromAddressBook where
        type Rs DisassociateContactFromAddressBook =
             DisassociateContactFromAddressBookResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateContactFromAddressBookResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateContactFromAddressBookResponse' smart constructor.
newtype DisassociateContactFromAddressBookResponse = DisassociateContactFromAddressBookResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateContactFromAddressBookResponse' value with any optional fields omitted.
mkDisassociateContactFromAddressBookResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateContactFromAddressBookResponse
mkDisassociateContactFromAddressBookResponse responseStatus
  = DisassociateContactFromAddressBookResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfabrrsResponseStatus :: Lens.Lens' DisassociateContactFromAddressBookResponse Core.Int
dcfabrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcfabrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
