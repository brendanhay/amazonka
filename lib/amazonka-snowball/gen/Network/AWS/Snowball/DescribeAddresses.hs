{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a specified number of @ADDRESS@ objects. Calling this API in one of the US regions will return addresses from the list of all addresses associated with this account in all US regions.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.DescribeAddresses
  ( -- * Creating a request
    DescribeAddresses (..),
    mkDescribeAddresses,

    -- ** Request lenses
    daMaxResults,
    daNextToken,

    -- * Destructuring the response
    DescribeAddressesResponse (..),
    mkDescribeAddressesResponse,

    -- ** Response lenses
    darrsAddresses,
    darrsNextToken,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkDescribeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { -- | The number of @ADDRESS@ objects to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | HTTP requests are stateless. To identify what object comes "next" in the list of @ADDRESS@ objects, you have the option of specifying a value for @NextToken@ as the starting point for your list of returned addresses.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddresses' value with any optional fields omitted.
mkDescribeAddresses ::
  DescribeAddresses
mkDescribeAddresses =
  DescribeAddresses'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The number of @ADDRESS@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxResults :: Lens.Lens' DescribeAddresses (Core.Maybe Core.Natural)
daMaxResults = Lens.field @"maxResults"
{-# DEPRECATED daMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @ADDRESS@ objects, you have the option of specifying a value for @NextToken@ as the starting point for your list of returned addresses.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeAddresses (Core.Maybe Types.NextToken)
daNextToken = Lens.field @"nextToken"
{-# DEPRECATED daNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAddresses where
  toJSON DescribeAddresses {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAddresses where
  type Rs DescribeAddresses = DescribeAddressesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.DescribeAddresses"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAddressesResponse'
            Core.<$> (x Core..:? "Addresses")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAddresses where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"addresses" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { -- | The Snow device shipping addresses that were created for this account.
    addresses :: Core.Maybe [Types.Address],
    -- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @DescribeAddresses@ call, your list of returned addresses will start from this point in the array.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAddressesResponse' value with any optional fields omitted.
mkDescribeAddressesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAddressesResponse
mkDescribeAddressesResponse responseStatus =
  DescribeAddressesResponse'
    { addresses = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The Snow device shipping addresses that were created for this account.
--
-- /Note:/ Consider using 'addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAddresses :: Lens.Lens' DescribeAddressesResponse (Core.Maybe [Types.Address])
darrsAddresses = Lens.field @"addresses"
{-# DEPRECATED darrsAddresses "Use generic-lens or generic-optics with 'addresses' instead." #-}

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @DescribeAddresses@ call, your list of returned addresses will start from this point in the array.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsNextToken :: Lens.Lens' DescribeAddressesResponse (Core.Maybe Types.NextToken)
darrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED darrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAddressesResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
