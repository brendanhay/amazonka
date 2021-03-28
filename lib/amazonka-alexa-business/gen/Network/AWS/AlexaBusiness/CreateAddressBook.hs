{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateAddressBook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address book with the specified details.
module Network.AWS.AlexaBusiness.CreateAddressBook
    (
    -- * Creating a request
      CreateAddressBook (..)
    , mkCreateAddressBook
    -- ** Request lenses
    , cabName
    , cabClientRequestToken
    , cabDescription

    -- * Destructuring the response
    , CreateAddressBookResponse (..)
    , mkCreateAddressBookResponse
    -- ** Response lenses
    , cabrrsAddressBookArn
    , cabrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAddressBook' smart constructor.
data CreateAddressBook = CreateAddressBook'
  { name :: Types.Name
    -- ^ The name of the address book.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, user-specified identifier for the request that ensures idempotency.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the address book.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAddressBook' value with any optional fields omitted.
mkCreateAddressBook
    :: Types.Name -- ^ 'name'
    -> CreateAddressBook
mkCreateAddressBook name
  = CreateAddressBook'{name, clientRequestToken = Core.Nothing,
                       description = Core.Nothing}

-- | The name of the address book.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabName :: Lens.Lens' CreateAddressBook Types.Name
cabName = Lens.field @"name"
{-# INLINEABLE cabName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique, user-specified identifier for the request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabClientRequestToken :: Lens.Lens' CreateAddressBook (Core.Maybe Types.ClientRequestToken)
cabClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cabClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The description of the address book.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabDescription :: Lens.Lens' CreateAddressBook (Core.Maybe Types.Description)
cabDescription = Lens.field @"description"
{-# INLINEABLE cabDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreateAddressBook where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAddressBook where
        toHeaders CreateAddressBook{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateAddressBook")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateAddressBook where
        toJSON CreateAddressBook{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Description" Core..=) Core.<$> description])

instance Core.AWSRequest CreateAddressBook where
        type Rs CreateAddressBook = CreateAddressBookResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAddressBookResponse' Core.<$>
                   (x Core..:? "AddressBookArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAddressBookResponse' smart constructor.
data CreateAddressBookResponse = CreateAddressBookResponse'
  { addressBookArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the newly created address book.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAddressBookResponse' value with any optional fields omitted.
mkCreateAddressBookResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAddressBookResponse
mkCreateAddressBookResponse responseStatus
  = CreateAddressBookResponse'{addressBookArn = Core.Nothing,
                               responseStatus}

-- | The ARN of the newly created address book.
--
-- /Note:/ Consider using 'addressBookArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrrsAddressBookArn :: Lens.Lens' CreateAddressBookResponse (Core.Maybe Types.Arn)
cabrrsAddressBookArn = Lens.field @"addressBookArn"
{-# INLINEABLE cabrrsAddressBookArn #-}
{-# DEPRECATED addressBookArn "Use generic-lens or generic-optics with 'addressBookArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cabrrsResponseStatus :: Lens.Lens' CreateAddressBookResponse Core.Int
cabrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cabrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
