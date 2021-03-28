{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeOpsItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query a set of OpsItems. You must have permission in AWS Identity and Access Management (IAM) to query a list of OpsItems. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ . 
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeOpsItems
    (
    -- * Creating a request
      DescribeOpsItems (..)
    , mkDescribeOpsItems
    -- ** Request lenses
    , doiMaxResults
    , doiNextToken
    , doiOpsItemFilters

    -- * Destructuring the response
    , DescribeOpsItemsResponse (..)
    , mkDescribeOpsItemsResponse
    -- ** Response lenses
    , doirrsNextToken
    , doirrsOpsItemSummaries
    , doirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeOpsItems' smart constructor.
data DescribeOpsItems = DescribeOpsItems'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to start the list. Use this token to get the next set of results.
  , opsItemFilters :: Core.Maybe [Types.OpsItemFilter]
    -- ^ One or more filters to limit the response.
--
--
--     * Key: CreatedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: LastModifiedBy
-- Operations: Contains, Equals
--
--
--     * Key: LastModifiedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: Priority
-- Operations: Equals
--
--
--     * Key: Source
-- Operations: Contains, Equals
--
--
--     * Key: Status
-- Operations: Equals
--
--
--     * Key: Title
-- Operations: Contains
--
--
--     * Key: OperationalData*
-- Operations: Equals
--
--
--     * Key: OperationalDataKey
-- Operations: Equals
--
--
--     * Key: OperationalDataValue
-- Operations: Equals, Contains
--
--
--     * Key: OpsItemId
-- Operations: Equals
--
--
--     * Key: ResourceId
-- Operations: Contains
--
--
--     * Key: AutomationId
-- Operations: Equals
--
--
-- *If you filter the response by using the OperationalData operator, specify a key-value pair by using the following JSON format: {"key":"key_name","value":"a_value"}
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOpsItems' value with any optional fields omitted.
mkDescribeOpsItems
    :: DescribeOpsItems
mkDescribeOpsItems
  = DescribeOpsItems'{maxResults = Core.Nothing,
                      nextToken = Core.Nothing, opsItemFilters = Core.Nothing}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doiMaxResults :: Lens.Lens' DescribeOpsItems (Core.Maybe Core.Natural)
doiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE doiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doiNextToken :: Lens.Lens' DescribeOpsItems (Core.Maybe Core.Text)
doiNextToken = Lens.field @"nextToken"
{-# INLINEABLE doiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more filters to limit the response.
--
--
--     * Key: CreatedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: LastModifiedBy
-- Operations: Contains, Equals
--
--
--     * Key: LastModifiedTime
-- Operations: GreaterThan, LessThan
--
--
--     * Key: Priority
-- Operations: Equals
--
--
--     * Key: Source
-- Operations: Contains, Equals
--
--
--     * Key: Status
-- Operations: Equals
--
--
--     * Key: Title
-- Operations: Contains
--
--
--     * Key: OperationalData*
-- Operations: Equals
--
--
--     * Key: OperationalDataKey
-- Operations: Equals
--
--
--     * Key: OperationalDataValue
-- Operations: Equals, Contains
--
--
--     * Key: OpsItemId
-- Operations: Equals
--
--
--     * Key: ResourceId
-- Operations: Contains
--
--
--     * Key: AutomationId
-- Operations: Equals
--
--
-- *If you filter the response by using the OperationalData operator, specify a key-value pair by using the following JSON format: {"key":"key_name","value":"a_value"}
--
-- /Note:/ Consider using 'opsItemFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doiOpsItemFilters :: Lens.Lens' DescribeOpsItems (Core.Maybe [Types.OpsItemFilter])
doiOpsItemFilters = Lens.field @"opsItemFilters"
{-# INLINEABLE doiOpsItemFilters #-}
{-# DEPRECATED opsItemFilters "Use generic-lens or generic-optics with 'opsItemFilters' instead"  #-}

instance Core.ToQuery DescribeOpsItems where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOpsItems where
        toHeaders DescribeOpsItems{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribeOpsItems") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeOpsItems where
        toJSON DescribeOpsItems{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("OpsItemFilters" Core..=) Core.<$> opsItemFilters])

instance Core.AWSRequest DescribeOpsItems where
        type Rs DescribeOpsItems = DescribeOpsItemsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOpsItemsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "OpsItemSummaries"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeOpsItems where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"opsItemSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeOpsItemsResponse' smart constructor.
data DescribeOpsItemsResponse = DescribeOpsItemsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , opsItemSummaries :: Core.Maybe [Types.OpsItemSummary]
    -- ^ A list of OpsItems.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeOpsItemsResponse' value with any optional fields omitted.
mkDescribeOpsItemsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOpsItemsResponse
mkDescribeOpsItemsResponse responseStatus
  = DescribeOpsItemsResponse'{nextToken = Core.Nothing,
                              opsItemSummaries = Core.Nothing, responseStatus}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doirrsNextToken :: Lens.Lens' DescribeOpsItemsResponse (Core.Maybe Core.Text)
doirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE doirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of OpsItems.
--
-- /Note:/ Consider using 'opsItemSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doirrsOpsItemSummaries :: Lens.Lens' DescribeOpsItemsResponse (Core.Maybe [Types.OpsItemSummary])
doirrsOpsItemSummaries = Lens.field @"opsItemSummaries"
{-# INLINEABLE doirrsOpsItemSummaries #-}
{-# DEPRECATED opsItemSummaries "Use generic-lens or generic-optics with 'opsItemSummaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doirrsResponseStatus :: Lens.Lens' DescribeOpsItemsResponse Core.Int
doirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE doirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
