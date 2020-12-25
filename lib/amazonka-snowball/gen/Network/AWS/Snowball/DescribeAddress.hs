{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeAddress (..),
    mkDescribeAddress,

    -- ** Request lenses
    daAddressId,

    -- * Destructuring the response
    DescribeAddressResponse (..),
    mkDescribeAddressResponse,

    -- ** Response lenses
    drsAddress,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkDescribeAddress' smart constructor.
newtype DescribeAddress = DescribeAddress'
  { -- | The automatically generated ID for a specific address.
    addressId :: Types.AddressId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddress' value with any optional fields omitted.
mkDescribeAddress ::
  -- | 'addressId'
  Types.AddressId ->
  DescribeAddress
mkDescribeAddress addressId = DescribeAddress' {addressId}

-- | The automatically generated ID for a specific address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAddressId :: Lens.Lens' DescribeAddress Types.AddressId
daAddressId = Lens.field @"addressId"
{-# DEPRECATED daAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

instance Core.FromJSON DescribeAddress where
  toJSON DescribeAddress {..} =
    Core.object
      (Core.catMaybes [Core.Just ("AddressId" Core..= addressId)])

instance Core.AWSRequest DescribeAddress where
  type Rs DescribeAddress = DescribeAddressResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.DescribeAddress"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddressResponse'
            Core.<$> (x Core..:? "Address") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAddressResponse' smart constructor.
data DescribeAddressResponse = DescribeAddressResponse'
  { -- | The address that you want the Snow device(s) associated with a specific job to be shipped to.
    address :: Core.Maybe Types.Address,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddressResponse' value with any optional fields omitted.
mkDescribeAddressResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAddressResponse
mkDescribeAddressResponse responseStatus =
  DescribeAddressResponse' {address = Core.Nothing, responseStatus}

-- | The address that you want the Snow device(s) associated with a specific job to be shipped to.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAddress :: Lens.Lens' DescribeAddressResponse (Core.Maybe Types.Address)
drsAddress = Lens.field @"address"
{-# DEPRECATED drsAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeAddressResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
