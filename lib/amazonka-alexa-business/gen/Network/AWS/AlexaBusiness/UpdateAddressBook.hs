{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateAddressBook (..)
    , mkUpdateAddressBook
    -- ** Request lenses
    , uabAddressBookArn
    , uabDescription
    , uabName

    -- * Destructuring the response
    , UpdateAddressBookResponse (..)
    , mkUpdateAddressBookResponse
    -- ** Response lenses
    , uabrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAddressBook' smart constructor.
data UpdateAddressBook = UpdateAddressBook'
  { addressBookArn :: Types.AddressBookArn
    -- ^ The ARN of the room to update.
  , description :: Core.Maybe Types.Description
    -- ^ The updated description of the room.
  , name :: Core.Maybe Types.Name
    -- ^ The updated name of the room.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAddressBook' value with any optional fields omitted.
mkUpdateAddressBook
    :: Types.AddressBookArn -- ^ 'addressBookArn'
    -> UpdateAddressBook
mkUpdateAddressBook addressBookArn
  = UpdateAddressBook'{addressBookArn, description = Core.Nothing,
                       name = Core.Nothing}

-- | The ARN of the room to update.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabAddressBookArn :: Lens.Lens' UpdateAddressBook Types.AddressBookArn
uabAddressBookArn = Lens.field @"addressBookArn"
{-# INLINEABLE uabAddressBookArn #-}
{-# DEPRECATED addressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead"  #-}

-- | The updated description of the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabDescription :: Lens.Lens' UpdateAddressBook (Core.Maybe Types.Description)
uabDescription = Lens.field @"description"
{-# INLINEABLE uabDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The updated name of the room.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabName :: Lens.Lens' UpdateAddressBook (Core.Maybe Types.Name)
uabName = Lens.field @"name"
{-# INLINEABLE uabName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateAddressBook where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAddressBook where
        toHeaders UpdateAddressBook{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateAddressBook")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAddressBook where
        toJSON UpdateAddressBook{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AddressBookArn" Core..= addressBookArn),
                  ("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateAddressBook where
        type Rs UpdateAddressBook = UpdateAddressBookResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateAddressBookResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAddressBookResponse' smart constructor.
newtype UpdateAddressBookResponse = UpdateAddressBookResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAddressBookResponse' value with any optional fields omitted.
mkUpdateAddressBookResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateAddressBookResponse
mkUpdateAddressBookResponse responseStatus
  = UpdateAddressBookResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uabrrsResponseStatus :: Lens.Lens' UpdateAddressBookResponse Core.Int
uabrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uabrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
