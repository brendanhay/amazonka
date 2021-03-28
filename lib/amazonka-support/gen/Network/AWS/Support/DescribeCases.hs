{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeCases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cases that you specify by passing one or more case IDs. You can use the @afterTime@ and @beforeTime@ parameters to filter the cases by date. You can set values for the @includeResolvedCases@ and @includeCommunications@ parameters to specify how much information to return.
--
-- The response returns the following in JSON format:
--
--     * One or more <https://docs.aws.amazon.com/awssupport/latest/APIReference/API_CaseDetails.html CaseDetails> data types.
--
--
--     * One or more @nextToken@ values, which specify where to paginate the returned records represented by the @CaseDetails@ objects.
--
--
-- Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request might return an error.
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCases
    (
    -- * Creating a request
      DescribeCases (..)
    , mkDescribeCases
    -- ** Request lenses
    , dcAfterTime
    , dcBeforeTime
    , dcCaseIdList
    , dcDisplayId
    , dcIncludeCommunications
    , dcIncludeResolvedCases
    , dcLanguage
    , dcMaxResults
    , dcNextToken

    -- * Destructuring the response
    , DescribeCasesResponse (..)
    , mkDescribeCasesResponse
    -- ** Response lenses
    , drsCases
    , drsNextToken
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkDescribeCases' smart constructor.
data DescribeCases = DescribeCases'
  { afterTime :: Core.Maybe Types.AfterTime
    -- ^ The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
  , beforeTime :: Core.Maybe Types.BeforeTime
    -- ^ The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
  , caseIdList :: Core.Maybe [Types.CaseId]
    -- ^ A list of ID numbers of the support cases you want returned. The maximum number of cases is 100.
  , displayId :: Core.Maybe Types.DisplayId
    -- ^ The ID displayed for a case in the AWS Support Center user interface.
  , includeCommunications :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include communications in the @DescribeCases@ response. By default, communications are incuded.
  , includeResolvedCases :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include resolved support cases in the @DescribeCases@ response. By default, resolved cases aren't included.
  , language :: Core.Maybe Types.Language
    -- ^ The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return before paginating.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A resumption point for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCases' value with any optional fields omitted.
mkDescribeCases
    :: DescribeCases
mkDescribeCases
  = DescribeCases'{afterTime = Core.Nothing,
                   beforeTime = Core.Nothing, caseIdList = Core.Nothing,
                   displayId = Core.Nothing, includeCommunications = Core.Nothing,
                   includeResolvedCases = Core.Nothing, language = Core.Nothing,
                   maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'afterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAfterTime :: Lens.Lens' DescribeCases (Core.Maybe Types.AfterTime)
dcAfterTime = Lens.field @"afterTime"
{-# INLINEABLE dcAfterTime #-}
{-# DEPRECATED afterTime "Use generic-lens or generic-optics with 'afterTime' instead"  #-}

-- | The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'beforeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcBeforeTime :: Lens.Lens' DescribeCases (Core.Maybe Types.BeforeTime)
dcBeforeTime = Lens.field @"beforeTime"
{-# INLINEABLE dcBeforeTime #-}
{-# DEPRECATED beforeTime "Use generic-lens or generic-optics with 'beforeTime' instead"  #-}

-- | A list of ID numbers of the support cases you want returned. The maximum number of cases is 100.
--
-- /Note:/ Consider using 'caseIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCaseIdList :: Lens.Lens' DescribeCases (Core.Maybe [Types.CaseId])
dcCaseIdList = Lens.field @"caseIdList"
{-# INLINEABLE dcCaseIdList #-}
{-# DEPRECATED caseIdList "Use generic-lens or generic-optics with 'caseIdList' instead"  #-}

-- | The ID displayed for a case in the AWS Support Center user interface.
--
-- /Note:/ Consider using 'displayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDisplayId :: Lens.Lens' DescribeCases (Core.Maybe Types.DisplayId)
dcDisplayId = Lens.field @"displayId"
{-# INLINEABLE dcDisplayId #-}
{-# DEPRECATED displayId "Use generic-lens or generic-optics with 'displayId' instead"  #-}

-- | Specifies whether to include communications in the @DescribeCases@ response. By default, communications are incuded.
--
-- /Note:/ Consider using 'includeCommunications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcIncludeCommunications :: Lens.Lens' DescribeCases (Core.Maybe Core.Bool)
dcIncludeCommunications = Lens.field @"includeCommunications"
{-# INLINEABLE dcIncludeCommunications #-}
{-# DEPRECATED includeCommunications "Use generic-lens or generic-optics with 'includeCommunications' instead"  #-}

-- | Specifies whether to include resolved support cases in the @DescribeCases@ response. By default, resolved cases aren't included.
--
-- /Note:/ Consider using 'includeResolvedCases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcIncludeResolvedCases :: Lens.Lens' DescribeCases (Core.Maybe Core.Bool)
dcIncludeResolvedCases = Lens.field @"includeResolvedCases"
{-# INLINEABLE dcIncludeResolvedCases #-}
{-# DEPRECATED includeResolvedCases "Use generic-lens or generic-optics with 'includeResolvedCases' instead"  #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLanguage :: Lens.Lens' DescribeCases (Core.Maybe Types.Language)
dcLanguage = Lens.field @"language"
{-# INLINEABLE dcLanguage #-}
{-# DEPRECATED language "Use generic-lens or generic-optics with 'language' instead"  #-}

-- | The maximum number of results to return before paginating.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMaxResults :: Lens.Lens' DescribeCases (Core.Maybe Core.Natural)
dcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcNextToken :: Lens.Lens' DescribeCases (Core.Maybe Types.NextToken)
dcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeCases where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCases where
        toHeaders DescribeCases{..}
          = Core.pure ("X-Amz-Target", "AWSSupport_20130415.DescribeCases")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCases where
        toJSON DescribeCases{..}
          = Core.object
              (Core.catMaybes
                 [("afterTime" Core..=) Core.<$> afterTime,
                  ("beforeTime" Core..=) Core.<$> beforeTime,
                  ("caseIdList" Core..=) Core.<$> caseIdList,
                  ("displayId" Core..=) Core.<$> displayId,
                  ("includeCommunications" Core..=) Core.<$> includeCommunications,
                  ("includeResolvedCases" Core..=) Core.<$> includeResolvedCases,
                  ("language" Core..=) Core.<$> language,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeCases where
        type Rs DescribeCases = DescribeCasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCasesResponse' Core.<$>
                   (x Core..:? "cases") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCases where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"cases" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Returns an array of <https://docs.aws.amazon.com/awssupport/latest/APIReference/API_CaseDetails.html CaseDetails> objects and a @nextToken@ that defines a point for pagination in the result set.
--
-- /See:/ 'mkDescribeCasesResponse' smart constructor.
data DescribeCasesResponse = DescribeCasesResponse'
  { cases :: Core.Maybe [Types.CaseDetails]
    -- ^ The details for the cases that match the request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A resumption point for pagination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCasesResponse' value with any optional fields omitted.
mkDescribeCasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCasesResponse
mkDescribeCasesResponse responseStatus
  = DescribeCasesResponse'{cases = Core.Nothing,
                           nextToken = Core.Nothing, responseStatus}

-- | The details for the cases that match the request.
--
-- /Note:/ Consider using 'cases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCases :: Lens.Lens' DescribeCasesResponse (Core.Maybe [Types.CaseDetails])
drsCases = Lens.field @"cases"
{-# INLINEABLE drsCases #-}
{-# DEPRECATED cases "Use generic-lens or generic-optics with 'cases' instead"  #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeCasesResponse (Core.Maybe Types.NextToken)
drsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCasesResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
