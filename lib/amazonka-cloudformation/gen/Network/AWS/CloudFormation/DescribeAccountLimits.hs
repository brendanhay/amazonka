{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeAccountLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your account's AWS CloudFormation limits, such as the maximum number of stacks that you can create in your account. For more information about account limits, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html AWS CloudFormation Limits> in the /AWS CloudFormation User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeAccountLimits
    (
    -- * Creating a request
      DescribeAccountLimits (..)
    , mkDescribeAccountLimits
    -- ** Request lenses
    , dalNextToken

    -- * Destructuring the response
    , DescribeAccountLimitsResponse (..)
    , mkDescribeAccountLimitsResponse
    -- ** Response lenses
    , dalrrsAccountLimits
    , dalrrsNextToken
    , dalrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'DescribeAccountLimits' action.
--
-- /See:/ 'mkDescribeAccountLimits' smart constructor.
newtype DescribeAccountLimits = DescribeAccountLimits'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A string that identifies the next page of limits that you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountLimits' value with any optional fields omitted.
mkDescribeAccountLimits
    :: DescribeAccountLimits
mkDescribeAccountLimits
  = DescribeAccountLimits'{nextToken = Core.Nothing}

-- | A string that identifies the next page of limits that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalNextToken :: Lens.Lens' DescribeAccountLimits (Core.Maybe Types.NextToken)
dalNextToken = Lens.field @"nextToken"
{-# INLINEABLE dalNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeAccountLimits where
        toQuery DescribeAccountLimits{..}
          = Core.toQueryPair "Action" ("DescribeAccountLimits" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeAccountLimits where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAccountLimits where
        type Rs DescribeAccountLimits = DescribeAccountLimitsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeAccountLimitsResult"
              (\ s h x ->
                 DescribeAccountLimitsResponse' Core.<$>
                   (x Core..@? "AccountLimits" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeAccountLimits where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"accountLimits" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The output for the 'DescribeAccountLimits' action.
--
-- /See:/ 'mkDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { accountLimits :: Core.Maybe [Types.AccountLimit]
    -- ^ An account limit structure that contain a list of AWS CloudFormation account limits and their values.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the output exceeds 1 MB in size, a string that identifies the next page of limits. If no additional page exists, this value is null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountLimitsResponse' value with any optional fields omitted.
mkDescribeAccountLimitsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAccountLimitsResponse
mkDescribeAccountLimitsResponse responseStatus
  = DescribeAccountLimitsResponse'{accountLimits = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | An account limit structure that contain a list of AWS CloudFormation account limits and their values.
--
-- /Note:/ Consider using 'accountLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsAccountLimits :: Lens.Lens' DescribeAccountLimitsResponse (Core.Maybe [Types.AccountLimit])
dalrrsAccountLimits = Lens.field @"accountLimits"
{-# INLINEABLE dalrrsAccountLimits #-}
{-# DEPRECATED accountLimits "Use generic-lens or generic-optics with 'accountLimits' instead"  #-}

-- | If the output exceeds 1 MB in size, a string that identifies the next page of limits. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsNextToken :: Lens.Lens' DescribeAccountLimitsResponse (Core.Maybe Types.NextToken)
dalrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dalrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalrrsResponseStatus :: Lens.Lens' DescribeAccountLimitsResponse Core.Int
dalrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dalrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
