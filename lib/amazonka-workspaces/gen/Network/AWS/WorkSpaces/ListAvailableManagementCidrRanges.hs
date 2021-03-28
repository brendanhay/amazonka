{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of IP address ranges, specified as IPv4 CIDR blocks, that you can use for the network management interface when you enable Bring Your Own License (BYOL). 
--
-- This operation can be run only by AWS accounts that are enabled for BYOL. If your account isn't enabled for BYOL, you'll receive an @AccessDeniedException@ error.
-- The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
    (
    -- * Creating a request
      ListAvailableManagementCidrRanges (..)
    , mkListAvailableManagementCidrRanges
    -- ** Request lenses
    , lamcrManagementCidrRangeConstraint
    , lamcrMaxResults
    , lamcrNextToken

    -- * Destructuring the response
    , ListAvailableManagementCidrRangesResponse (..)
    , mkListAvailableManagementCidrRangesResponse
    -- ** Response lenses
    , lamcrrrsManagementCidrRanges
    , lamcrrrsNextToken
    , lamcrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkListAvailableManagementCidrRanges' smart constructor.
data ListAvailableManagementCidrRanges = ListAvailableManagementCidrRanges'
  { managementCidrRangeConstraint :: Types.ManagementCidrRangeConstraint
    -- ^ The IP address range to search. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block).
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAvailableManagementCidrRanges' value with any optional fields omitted.
mkListAvailableManagementCidrRanges
    :: Types.ManagementCidrRangeConstraint -- ^ 'managementCidrRangeConstraint'
    -> ListAvailableManagementCidrRanges
mkListAvailableManagementCidrRanges managementCidrRangeConstraint
  = ListAvailableManagementCidrRanges'{managementCidrRangeConstraint,
                                       maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The IP address range to search. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block).
--
-- /Note:/ Consider using 'managementCidrRangeConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrManagementCidrRangeConstraint :: Lens.Lens' ListAvailableManagementCidrRanges Types.ManagementCidrRangeConstraint
lamcrManagementCidrRangeConstraint = Lens.field @"managementCidrRangeConstraint"
{-# INLINEABLE lamcrManagementCidrRangeConstraint #-}
{-# DEPRECATED managementCidrRangeConstraint "Use generic-lens or generic-optics with 'managementCidrRangeConstraint' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrMaxResults :: Lens.Lens' ListAvailableManagementCidrRanges (Core.Maybe Core.Natural)
lamcrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lamcrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrNextToken :: Lens.Lens' ListAvailableManagementCidrRanges (Core.Maybe Types.PaginationToken)
lamcrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lamcrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAvailableManagementCidrRanges where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAvailableManagementCidrRanges where
        toHeaders ListAvailableManagementCidrRanges{..}
          = Core.pure
              ("X-Amz-Target",
               "WorkspacesService.ListAvailableManagementCidrRanges")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAvailableManagementCidrRanges where
        toJSON ListAvailableManagementCidrRanges{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ManagementCidrRangeConstraint" Core..=
                       managementCidrRangeConstraint),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAvailableManagementCidrRanges where
        type Rs ListAvailableManagementCidrRanges =
             ListAvailableManagementCidrRangesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAvailableManagementCidrRangesResponse' Core.<$>
                   (x Core..:? "ManagementCidrRanges") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAvailableManagementCidrRanges where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"managementCidrRanges" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAvailableManagementCidrRangesResponse' smart constructor.
data ListAvailableManagementCidrRangesResponse = ListAvailableManagementCidrRangesResponse'
  { managementCidrRanges :: Core.Maybe [Types.DedicatedTenancyManagementCidrRange]
    -- ^ The list of available IP address ranges, specified as IPv4 CIDR blocks.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to use to retrieve the next set of results, or null if no more results are available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAvailableManagementCidrRangesResponse' value with any optional fields omitted.
mkListAvailableManagementCidrRangesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAvailableManagementCidrRangesResponse
mkListAvailableManagementCidrRangesResponse responseStatus
  = ListAvailableManagementCidrRangesResponse'{managementCidrRanges =
                                                 Core.Nothing,
                                               nextToken = Core.Nothing, responseStatus}

-- | The list of available IP address ranges, specified as IPv4 CIDR blocks.
--
-- /Note:/ Consider using 'managementCidrRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrrrsManagementCidrRanges :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Core.Maybe [Types.DedicatedTenancyManagementCidrRange])
lamcrrrsManagementCidrRanges = Lens.field @"managementCidrRanges"
{-# INLINEABLE lamcrrrsManagementCidrRanges #-}
{-# DEPRECATED managementCidrRanges "Use generic-lens or generic-optics with 'managementCidrRanges' instead"  #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrrrsNextToken :: Lens.Lens' ListAvailableManagementCidrRangesResponse (Core.Maybe Types.PaginationToken)
lamcrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lamcrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamcrrrsResponseStatus :: Lens.Lens' ListAvailableManagementCidrRangesResponse Core.Int
lamcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lamcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
