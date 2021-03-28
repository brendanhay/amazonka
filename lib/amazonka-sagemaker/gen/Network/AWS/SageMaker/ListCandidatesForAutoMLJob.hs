{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCandidatesForAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Candidates created for the job.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCandidatesForAutoMLJob
    (
    -- * Creating a request
      ListCandidatesForAutoMLJob (..)
    , mkListCandidatesForAutoMLJob
    -- ** Request lenses
    , lcfamljAutoMLJobName
    , lcfamljCandidateNameEquals
    , lcfamljMaxResults
    , lcfamljNextToken
    , lcfamljSortBy
    , lcfamljSortOrder
    , lcfamljStatusEquals

    -- * Destructuring the response
    , ListCandidatesForAutoMLJobResponse (..)
    , mkListCandidatesForAutoMLJobResponse
    -- ** Response lenses
    , lcfamljrrsCandidates
    , lcfamljrrsNextToken
    , lcfamljrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListCandidatesForAutoMLJob' smart constructor.
data ListCandidatesForAutoMLJob = ListCandidatesForAutoMLJob'
  { autoMLJobName :: Types.AutoMLJobName
    -- ^ List the Candidates created for the job by providing the job's name.
  , candidateNameEquals :: Core.Maybe Types.CandidateNameEquals
    -- ^ List the Candidates for the job and filter by candidate name.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ List the job's Candidates up to a specified limit.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
  , sortBy :: Core.Maybe Types.CandidateSortBy
    -- ^ The parameter by which to sort the results. The default is Descending.
  , sortOrder :: Core.Maybe Types.AutoMLSortOrder
    -- ^ The sort order for the results. The default is Ascending.
  , statusEquals :: Core.Maybe Types.CandidateStatus
    -- ^ List the Candidates for the job and filter by status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCandidatesForAutoMLJob' value with any optional fields omitted.
mkListCandidatesForAutoMLJob
    :: Types.AutoMLJobName -- ^ 'autoMLJobName'
    -> ListCandidatesForAutoMLJob
mkListCandidatesForAutoMLJob autoMLJobName
  = ListCandidatesForAutoMLJob'{autoMLJobName,
                                candidateNameEquals = Core.Nothing, maxResults = Core.Nothing,
                                nextToken = Core.Nothing, sortBy = Core.Nothing,
                                sortOrder = Core.Nothing, statusEquals = Core.Nothing}

-- | List the Candidates created for the job by providing the job's name.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljAutoMLJobName :: Lens.Lens' ListCandidatesForAutoMLJob Types.AutoMLJobName
lcfamljAutoMLJobName = Lens.field @"autoMLJobName"
{-# INLINEABLE lcfamljAutoMLJobName #-}
{-# DEPRECATED autoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead"  #-}

-- | List the Candidates for the job and filter by candidate name.
--
-- /Note:/ Consider using 'candidateNameEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljCandidateNameEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.CandidateNameEquals)
lcfamljCandidateNameEquals = Lens.field @"candidateNameEquals"
{-# INLINEABLE lcfamljCandidateNameEquals #-}
{-# DEPRECATED candidateNameEquals "Use generic-lens or generic-optics with 'candidateNameEquals' instead"  #-}

-- | List the job's Candidates up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljMaxResults :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Core.Natural)
lcfamljMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcfamljMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljNextToken :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.NextToken)
lcfamljNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcfamljNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The parameter by which to sort the results. The default is Descending.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljSortBy :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.CandidateSortBy)
lcfamljSortBy = Lens.field @"sortBy"
{-# INLINEABLE lcfamljSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljSortOrder :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.AutoMLSortOrder)
lcfamljSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lcfamljSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | List the Candidates for the job and filter by status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljStatusEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Types.CandidateStatus)
lcfamljStatusEquals = Lens.field @"statusEquals"
{-# INLINEABLE lcfamljStatusEquals #-}
{-# DEPRECATED statusEquals "Use generic-lens or generic-optics with 'statusEquals' instead"  #-}

instance Core.ToQuery ListCandidatesForAutoMLJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListCandidatesForAutoMLJob where
        toHeaders ListCandidatesForAutoMLJob{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.ListCandidatesForAutoMLJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListCandidatesForAutoMLJob where
        toJSON ListCandidatesForAutoMLJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AutoMLJobName" Core..= autoMLJobName),
                  ("CandidateNameEquals" Core..=) Core.<$> candidateNameEquals,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder,
                  ("StatusEquals" Core..=) Core.<$> statusEquals])

instance Core.AWSRequest ListCandidatesForAutoMLJob where
        type Rs ListCandidatesForAutoMLJob =
             ListCandidatesForAutoMLJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCandidatesForAutoMLJobResponse' Core.<$>
                   (x Core..:? "Candidates" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCandidatesForAutoMLJob where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"candidates") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListCandidatesForAutoMLJobResponse' smart constructor.
data ListCandidatesForAutoMLJobResponse = ListCandidatesForAutoMLJobResponse'
  { candidates :: [Types.AutoMLCandidate]
    -- ^ Summaries about the Candidates.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListCandidatesForAutoMLJobResponse' value with any optional fields omitted.
mkListCandidatesForAutoMLJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCandidatesForAutoMLJobResponse
mkListCandidatesForAutoMLJobResponse responseStatus
  = ListCandidatesForAutoMLJobResponse'{candidates = Core.mempty,
                                        nextToken = Core.Nothing, responseStatus}

-- | Summaries about the Candidates.
--
-- /Note:/ Consider using 'candidates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrrsCandidates :: Lens.Lens' ListCandidatesForAutoMLJobResponse [Types.AutoMLCandidate]
lcfamljrrsCandidates = Lens.field @"candidates"
{-# INLINEABLE lcfamljrrsCandidates #-}
{-# DEPRECATED candidates "Use generic-lens or generic-optics with 'candidates' instead"  #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrrsNextToken :: Lens.Lens' ListCandidatesForAutoMLJobResponse (Core.Maybe Types.NextToken)
lcfamljrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcfamljrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrrsResponseStatus :: Lens.Lens' ListCandidatesForAutoMLJobResponse Core.Int
lcfamljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcfamljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
