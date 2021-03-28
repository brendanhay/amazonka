{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address for a Snow device to be shipped to. In most regions, addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. If the address is invalid or unsupported, then an exception is thrown.
module Network.AWS.Snowball.CreateAddress
    (
    -- * Creating a request
      CreateAddress (..)
    , mkCreateAddress
    -- ** Request lenses
    , caAddress

    -- * Destructuring the response
    , CreateAddressResponse (..)
    , mkCreateAddressResponse
    -- ** Response lenses
    , carrsAddressId
    , carrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCreateAddress' smart constructor.
newtype CreateAddress = CreateAddress'
  { address :: Types.Address
    -- ^ The address that you want the Snow device shipped to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAddress' value with any optional fields omitted.
mkCreateAddress
    :: Types.Address -- ^ 'address'
    -> CreateAddress
mkCreateAddress address = CreateAddress'{address}

-- | The address that you want the Snow device shipped to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAddress :: Lens.Lens' CreateAddress Types.Address
caAddress = Lens.field @"address"
{-# INLINEABLE caAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

instance Core.ToQuery CreateAddress where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAddress where
        toHeaders CreateAddress{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.CreateAddress")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateAddress where
        toJSON CreateAddress{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Address" Core..= address)])

instance Core.AWSRequest CreateAddress where
        type Rs CreateAddress = CreateAddressResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAddressResponse' Core.<$>
                   (x Core..:? "AddressId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAddressResponse' smart constructor.
data CreateAddressResponse = CreateAddressResponse'
  { addressId :: Core.Maybe Core.Text
    -- ^ The automatically generated ID for a specific address. You'll use this ID when you create a job to specify which address you want the Snow device for that job shipped to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAddressResponse' value with any optional fields omitted.
mkCreateAddressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAddressResponse
mkCreateAddressResponse responseStatus
  = CreateAddressResponse'{addressId = Core.Nothing, responseStatus}

-- | The automatically generated ID for a specific address. You'll use this ID when you create a job to specify which address you want the Snow device for that job shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAddressId :: Lens.Lens' CreateAddressResponse (Core.Maybe Core.Text)
carrsAddressId = Lens.field @"addressId"
{-# INLINEABLE carrsAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAddressResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
