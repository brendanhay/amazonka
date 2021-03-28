{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetAddressBook (..)
    , mkGetAddressBook
    -- ** Request lenses
    , gabAddressBookArn

    -- * Destructuring the response
    , GetAddressBookResponse (..)
    , mkGetAddressBookResponse
    -- ** Response lenses
    , gabrrsAddressBook
    , gabrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAddressBook' smart constructor.
newtype GetAddressBook = GetAddressBook'
  { addressBookArn :: Types.Arn
    -- ^ The ARN of the address book for which to request details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAddressBook' value with any optional fields omitted.
mkGetAddressBook
    :: Types.Arn -- ^ 'addressBookArn'
    -> GetAddressBook
mkGetAddressBook addressBookArn = GetAddressBook'{addressBookArn}

-- | The ARN of the address book for which to request details.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabAddressBookArn :: Lens.Lens' GetAddressBook Types.Arn
gabAddressBookArn = Lens.field @"addressBookArn"
{-# INLINEABLE gabAddressBookArn #-}
{-# DEPRECATED addressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead"  #-}

instance Core.ToQuery GetAddressBook where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAddressBook where
        toHeaders GetAddressBook{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.GetAddressBook")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAddressBook where
        toJSON GetAddressBook{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AddressBookArn" Core..= addressBookArn)])

instance Core.AWSRequest GetAddressBook where
        type Rs GetAddressBook = GetAddressBookResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAddressBookResponse' Core.<$>
                   (x Core..:? "AddressBook") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAddressBookResponse' smart constructor.
data GetAddressBookResponse = GetAddressBookResponse'
  { addressBook :: Core.Maybe Types.AddressBook
    -- ^ The details of the requested address book.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAddressBookResponse' value with any optional fields omitted.
mkGetAddressBookResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAddressBookResponse
mkGetAddressBookResponse responseStatus
  = GetAddressBookResponse'{addressBook = Core.Nothing,
                            responseStatus}

-- | The details of the requested address book.
--
-- /Note:/ Consider using 'addressBook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrrsAddressBook :: Lens.Lens' GetAddressBookResponse (Core.Maybe Types.AddressBook)
gabrrsAddressBook = Lens.field @"addressBook"
{-# INLINEABLE gabrrsAddressBook #-}
{-# DEPRECATED addressBook "Use generic-lens or generic-optics with 'addressBook' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrrsResponseStatus :: Lens.Lens' GetAddressBookResponse Core.Int
gabrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gabrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
