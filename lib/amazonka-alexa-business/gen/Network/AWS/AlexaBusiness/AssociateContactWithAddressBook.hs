{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AssociateContactWithAddressBook (..)
    , mkAssociateContactWithAddressBook
    -- ** Request lenses
    , acwabContactArn
    , acwabAddressBookArn

    -- * Destructuring the response
    , AssociateContactWithAddressBookResponse (..)
    , mkAssociateContactWithAddressBookResponse
    -- ** Response lenses
    , acwabrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateContactWithAddressBook' smart constructor.
data AssociateContactWithAddressBook = AssociateContactWithAddressBook'
  { contactArn :: Types.Arn
    -- ^ The ARN of the contact to associate with an address book.
  , addressBookArn :: Types.Arn
    -- ^ The ARN of the address book with which to associate the contact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateContactWithAddressBook' value with any optional fields omitted.
mkAssociateContactWithAddressBook
    :: Types.Arn -- ^ 'contactArn'
    -> Types.Arn -- ^ 'addressBookArn'
    -> AssociateContactWithAddressBook
mkAssociateContactWithAddressBook contactArn addressBookArn
  = AssociateContactWithAddressBook'{contactArn, addressBookArn}

-- | The ARN of the contact to associate with an address book.
--
-- /Note:/ Consider using 'contactArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabContactArn :: Lens.Lens' AssociateContactWithAddressBook Types.Arn
acwabContactArn = Lens.field @"contactArn"
{-# INLINEABLE acwabContactArn #-}
{-# DEPRECATED contactArn "Use generic-lens or generic-optics with 'contactArn' instead"  #-}

-- | The ARN of the address book with which to associate the contact.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabAddressBookArn :: Lens.Lens' AssociateContactWithAddressBook Types.Arn
acwabAddressBookArn = Lens.field @"addressBookArn"
{-# INLINEABLE acwabAddressBookArn #-}
{-# DEPRECATED addressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead"  #-}

instance Core.ToQuery AssociateContactWithAddressBook where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateContactWithAddressBook where
        toHeaders AssociateContactWithAddressBook{..}
          = Core.pure
              ("X-Amz-Target",
               "AlexaForBusiness.AssociateContactWithAddressBook")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateContactWithAddressBook where
        toJSON AssociateContactWithAddressBook{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContactArn" Core..= contactArn),
                  Core.Just ("AddressBookArn" Core..= addressBookArn)])

instance Core.AWSRequest AssociateContactWithAddressBook where
        type Rs AssociateContactWithAddressBook =
             AssociateContactWithAddressBookResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateContactWithAddressBookResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateContactWithAddressBookResponse' smart constructor.
newtype AssociateContactWithAddressBookResponse = AssociateContactWithAddressBookResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateContactWithAddressBookResponse' value with any optional fields omitted.
mkAssociateContactWithAddressBookResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateContactWithAddressBookResponse
mkAssociateContactWithAddressBookResponse responseStatus
  = AssociateContactWithAddressBookResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acwabrrsResponseStatus :: Lens.Lens' AssociateContactWithAddressBookResponse Core.Int
acwabrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acwabrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
