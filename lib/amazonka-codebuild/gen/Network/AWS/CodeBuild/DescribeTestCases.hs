{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DescribeTestCases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of details about test cases for a report. 
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.DescribeTestCases
    (
    -- * Creating a request
      DescribeTestCases (..)
    , mkDescribeTestCases
    -- ** Request lenses
    , dtcReportArn
    , dtcFilter
    , dtcMaxResults
    , dtcNextToken

    -- * Destructuring the response
    , DescribeTestCasesResponse (..)
    , mkDescribeTestCasesResponse
    -- ** Response lenses
    , dtcrrsNextToken
    , dtcrrsTestCases
    , dtcrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTestCases' smart constructor.
data DescribeTestCases = DescribeTestCases'
  { reportArn :: Core.Text
    -- ^ The ARN of the report for which test cases are returned. 
  , filter :: Core.Maybe Types.TestCaseFilter
    -- ^ A @TestCaseFilter@ object used to filter the returned reports. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of paginated test cases returned per response. Use @nextToken@ to iterate pages in the list of returned @TestCase@ objects. The default value is 100. 
  , nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTestCases' value with any optional fields omitted.
mkDescribeTestCases
    :: Core.Text -- ^ 'reportArn'
    -> DescribeTestCases
mkDescribeTestCases reportArn
  = DescribeTestCases'{reportArn, filter = Core.Nothing,
                       maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ARN of the report for which test cases are returned. 
--
-- /Note:/ Consider using 'reportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcReportArn :: Lens.Lens' DescribeTestCases Core.Text
dtcReportArn = Lens.field @"reportArn"
{-# INLINEABLE dtcReportArn #-}
{-# DEPRECATED reportArn "Use generic-lens or generic-optics with 'reportArn' instead"  #-}

-- | A @TestCaseFilter@ object used to filter the returned reports. 
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcFilter :: Lens.Lens' DescribeTestCases (Core.Maybe Types.TestCaseFilter)
dtcFilter = Lens.field @"filter"
{-# INLINEABLE dtcFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of paginated test cases returned per response. Use @nextToken@ to iterate pages in the list of returned @TestCase@ objects. The default value is 100. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcMaxResults :: Lens.Lens' DescribeTestCases (Core.Maybe Core.Natural)
dtcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dtcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcNextToken :: Lens.Lens' DescribeTestCases (Core.Maybe Core.Text)
dtcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeTestCases where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeTestCases where
        toHeaders DescribeTestCases{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.DescribeTestCases")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeTestCases where
        toJSON DescribeTestCases{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("reportArn" Core..= reportArn),
                  ("filter" Core..=) Core.<$> filter,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeTestCases where
        type Rs DescribeTestCases = DescribeTestCasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeTestCasesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "testCases" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTestCases where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"testCases" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeTestCasesResponse' smart constructor.
data DescribeTestCasesResponse = DescribeTestCasesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  , testCases :: Core.Maybe [Types.TestCase]
    -- ^ The returned list of test cases. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTestCasesResponse' value with any optional fields omitted.
mkDescribeTestCasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTestCasesResponse
mkDescribeTestCasesResponse responseStatus
  = DescribeTestCasesResponse'{nextToken = Core.Nothing,
                               testCases = Core.Nothing, responseStatus}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrrsNextToken :: Lens.Lens' DescribeTestCasesResponse (Core.Maybe Core.Text)
dtcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The returned list of test cases. 
--
-- /Note:/ Consider using 'testCases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrrsTestCases :: Lens.Lens' DescribeTestCasesResponse (Core.Maybe [Types.TestCase])
dtcrrsTestCases = Lens.field @"testCases"
{-# INLINEABLE dtcrrsTestCases #-}
{-# DEPRECATED testCases "Use generic-lens or generic-optics with 'testCases' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrrsResponseStatus :: Lens.Lens' DescribeTestCasesResponse Core.Int
dtcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
