{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes an @AddressId@ and returns specific details about that address in the form of an @Address@ object.
module Network.AWS.Snowball.DescribeAddress
    (
    -- * Creating a request
      DescribeAddress (..)
    , mkDescribeAddress
    -- ** Request lenses
    , daAddressId

    -- * Destructuring the response
    , DescribeAddressResponse (..)
    , mkDescribeAddressResponse
    -- ** Response lenses
    , drsAddress
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkDescribeAddress' smart constructor.
newtype DescribeAddress = DescribeAddress'
  { addressId :: Types.AddressId
    -- ^ The automatically generated ID for a specific address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddress' value with any optional fields omitted.
mkDescribeAddress
    :: Types.AddressId -- ^ 'addressId'
    -> DescribeAddress
mkDescribeAddress addressId = DescribeAddress'{addressId}

-- | The automatically generated ID for a specific address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAddressId :: Lens.Lens' DescribeAddress Types.AddressId
daAddressId = Lens.field @"addressId"
{-# INLINEABLE daAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

instance Core.ToQuery DescribeAddress where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAddress where
        toHeaders DescribeAddress{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.DescribeAddress")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAddress where
        toJSON DescribeAddress{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AddressId" Core..= addressId)])

instance Core.AWSRequest DescribeAddress where
        type Rs DescribeAddress = DescribeAddressResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAddressResponse' Core.<$>
                   (x Core..:? "Address") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAddressResponse' smart constructor.
data DescribeAddressResponse = DescribeAddressResponse'
  { address :: Core.Maybe Types.Address
    -- ^ The address that you want the Snow device(s) associated with a specific job to be shipped to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddressResponse' value with any optional fields omitted.
mkDescribeAddressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAddressResponse
mkDescribeAddressResponse responseStatus
  = DescribeAddressResponse'{address = Core.Nothing, responseStatus}

-- | The address that you want the Snow device(s) associated with a specific job to be shipped to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAddress :: Lens.Lens' DescribeAddressResponse (Core.Maybe Types.Address)
drsAddress = Lens.field @"address"
{-# INLINEABLE drsAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeAddressResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
