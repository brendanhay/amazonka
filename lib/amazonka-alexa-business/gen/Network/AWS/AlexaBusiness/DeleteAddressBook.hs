{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteAddressBook (..)
    , mkDeleteAddressBook
    -- ** Request lenses
    , dabAddressBookArn

    -- * Destructuring the response
    , DeleteAddressBookResponse (..)
    , mkDeleteAddressBookResponse
    -- ** Response lenses
    , dabrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAddressBook' smart constructor.
newtype DeleteAddressBook = DeleteAddressBook'
  { addressBookArn :: Types.AddressBookArn
    -- ^ The ARN of the address book to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAddressBook' value with any optional fields omitted.
mkDeleteAddressBook
    :: Types.AddressBookArn -- ^ 'addressBookArn'
    -> DeleteAddressBook
mkDeleteAddressBook addressBookArn
  = DeleteAddressBook'{addressBookArn}

-- | The ARN of the address book to delete.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dabAddressBookArn :: Lens.Lens' DeleteAddressBook Types.AddressBookArn
dabAddressBookArn = Lens.field @"addressBookArn"
{-# INLINEABLE dabAddressBookArn #-}
{-# DEPRECATED addressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead"  #-}

instance Core.ToQuery DeleteAddressBook where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAddressBook where
        toHeaders DeleteAddressBook{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteAddressBook")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAddressBook where
        toJSON DeleteAddressBook{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AddressBookArn" Core..= addressBookArn)])

instance Core.AWSRequest DeleteAddressBook where
        type Rs DeleteAddressBook = DeleteAddressBookResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteAddressBookResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAddressBookResponse' smart constructor.
newtype DeleteAddressBookResponse = DeleteAddressBookResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAddressBookResponse' value with any optional fields omitted.
mkDeleteAddressBookResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAddressBookResponse
mkDeleteAddressBookResponse responseStatus
  = DeleteAddressBookResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dabrrsResponseStatus :: Lens.Lens' DeleteAddressBookResponse Core.Int
dabrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dabrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
