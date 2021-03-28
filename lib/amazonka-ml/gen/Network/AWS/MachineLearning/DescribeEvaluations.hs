{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeEvaluations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DescribeEvaluations@ that match the search criteria in the request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeEvaluations
    (
    -- * Creating a request
      DescribeEvaluations (..)
    , mkDescribeEvaluations
    -- ** Request lenses
    , deEQ
    , deFilterVariable
    , deGE
    , deGT
    , deLE
    , deLT
    , deLimit
    , deNE
    , deNextToken
    , dePrefix
    , deSortOrder

    -- * Destructuring the response
    , DescribeEvaluationsResponse (..)
    , mkDescribeEvaluationsResponse
    -- ** Response lenses
    , derfrsNextToken
    , derfrsResults
    , derfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEvaluations' smart constructor.
data DescribeEvaluations = DescribeEvaluations'
  { eq :: Core.Maybe Types.EQ
    -- ^ The equal to operator. The @Evaluation@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
  , filterVariable :: Core.Maybe Types.EvaluationFilterVariable
    -- ^ Use one of the following variable to filter a list of @Evaluation@ objects:
--
--
--     * @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation date.
--
--     * @Status@ - Sets the search criteria to the @Evaluation@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked an @Evaluation@ .
--
--     * @MLModelId@ - Sets the search criteria to the @MLModel@ that was evaluated.
--
--     * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in @Evaluation@ .
--
--     * @DataUri@ - Sets the search criteria to the data file(s) used in @Evaluation@ . The URL can identify either a file or an Amazon Simple Storage Solution (Amazon S3) bucket or directory.
--
  , ge :: Core.Maybe Types.GE
    -- ^ The greater than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ . 
  , gt :: Core.Maybe Types.GT
    -- ^ The greater than operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
  , le :: Core.Maybe Types.LE
    -- ^ The less than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
  , lt :: Core.Maybe Types.LT
    -- ^ The less than operator. The @Evaluation@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of @Evaluation@ to include in the result.
  , ne :: Core.Maybe Types.NE
    -- ^ The not equal to operator. The @Evaluation@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
  , nextToken :: Core.Maybe Types.StringType
    -- ^ The ID of the page in the paginated results.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, an @Evaluation@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @Evaluation@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ : 
--
--     * 2014-09
--
--
--     * 2014-09-09
--
--
--     * 2014-09-09-Holiday
--
--
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ A two-value parameter that determines the sequence of the resulting list of @Evaluation@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEvaluations' value with any optional fields omitted.
mkDescribeEvaluations
    :: DescribeEvaluations
mkDescribeEvaluations
  = DescribeEvaluations'{eq = Core.Nothing,
                         filterVariable = Core.Nothing, ge = Core.Nothing,
                         gt = Core.Nothing, le = Core.Nothing, lt = Core.Nothing,
                         limit = Core.Nothing, ne = Core.Nothing, nextToken = Core.Nothing,
                         prefix = Core.Nothing, sortOrder = Core.Nothing}

-- | The equal to operator. The @Evaluation@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- /Note:/ Consider using 'eq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEQ :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.EQ)
deEQ = Lens.field @"eq"
{-# INLINEABLE deEQ #-}
{-# DEPRECATED eq "Use generic-lens or generic-optics with 'eq' instead"  #-}

-- | Use one of the following variable to filter a list of @Evaluation@ objects:
--
--
--     * @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation date.
--
--     * @Status@ - Sets the search criteria to the @Evaluation@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked an @Evaluation@ .
--
--     * @MLModelId@ - Sets the search criteria to the @MLModel@ that was evaluated.
--
--     * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in @Evaluation@ .
--
--     * @DataUri@ - Sets the search criteria to the data file(s) used in @Evaluation@ . The URL can identify either a file or an Amazon Simple Storage Solution (Amazon S3) bucket or directory.
--
--
-- /Note:/ Consider using 'filterVariable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deFilterVariable :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.EvaluationFilterVariable)
deFilterVariable = Lens.field @"filterVariable"
{-# INLINEABLE deFilterVariable #-}
{-# DEPRECATED filterVariable "Use generic-lens or generic-optics with 'filterVariable' instead"  #-}

-- | The greater than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ . 
--
-- /Note:/ Consider using 'ge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deGE :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.GE)
deGE = Lens.field @"ge"
{-# INLINEABLE deGE #-}
{-# DEPRECATED ge "Use generic-lens or generic-optics with 'ge' instead"  #-}

-- | The greater than operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- /Note:/ Consider using 'gt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deGT :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.GT)
deGT = Lens.field @"gt"
{-# INLINEABLE deGT #-}
{-# DEPRECATED gt "Use generic-lens or generic-optics with 'gt' instead"  #-}

-- | The less than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
--
-- /Note:/ Consider using 'le' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLE :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.LE)
deLE = Lens.field @"le"
{-# INLINEABLE deLE #-}
{-# DEPRECATED le "Use generic-lens or generic-optics with 'le' instead"  #-}

-- | The less than operator. The @Evaluation@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- /Note:/ Consider using 'lt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLT :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.LT)
deLT = Lens.field @"lt"
{-# INLINEABLE deLT #-}
{-# DEPRECATED lt "Use generic-lens or generic-optics with 'lt' instead"  #-}

-- | The maximum number of @Evaluation@ to include in the result.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLimit :: Lens.Lens' DescribeEvaluations (Core.Maybe Core.Natural)
deLimit = Lens.field @"limit"
{-# INLINEABLE deLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The not equal to operator. The @Evaluation@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- /Note:/ Consider using 'ne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNE :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.NE)
deNE = Lens.field @"ne"
{-# INLINEABLE deNE #-}
{-# DEPRECATED ne "Use generic-lens or generic-optics with 'ne' instead"  #-}

-- | The ID of the page in the paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.StringType)
deNextToken = Lens.field @"nextToken"
{-# INLINEABLE deNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, an @Evaluation@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @Evaluation@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ : 
--
--     * 2014-09
--
--
--     * 2014-09-09
--
--
--     * 2014-09-09-Holiday
--
--
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dePrefix :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.Prefix)
dePrefix = Lens.field @"prefix"
{-# INLINEABLE dePrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | A two-value parameter that determines the sequence of the resulting list of @Evaluation@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSortOrder :: Lens.Lens' DescribeEvaluations (Core.Maybe Types.SortOrder)
deSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE deSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery DescribeEvaluations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEvaluations where
        toHeaders DescribeEvaluations{..}
          = Core.pure
              ("X-Amz-Target", "AmazonML_20141212.DescribeEvaluations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEvaluations where
        toJSON DescribeEvaluations{..}
          = Core.object
              (Core.catMaybes
                 [("EQ" Core..=) Core.<$> eq,
                  ("FilterVariable" Core..=) Core.<$> filterVariable,
                  ("GE" Core..=) Core.<$> ge, ("GT" Core..=) Core.<$> gt,
                  ("LE" Core..=) Core.<$> le, ("LT" Core..=) Core.<$> lt,
                  ("Limit" Core..=) Core.<$> limit, ("NE" Core..=) Core.<$> ne,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Prefix" Core..=) Core.<$> prefix,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest DescribeEvaluations where
        type Rs DescribeEvaluations = DescribeEvaluationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEvaluationsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Results" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEvaluations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"results" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the query results from a @DescribeEvaluations@ operation. The content is essentially a list of @Evaluation@ .
--
-- /See:/ 'mkDescribeEvaluationsResponse' smart constructor.
data DescribeEvaluationsResponse = DescribeEvaluationsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The ID of the next page in the paginated results that indicates at least one more page follows.
  , results :: Core.Maybe [Types.Evaluation]
    -- ^ A list of @Evaluation@ that meet the search criteria. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEvaluationsResponse' value with any optional fields omitted.
mkDescribeEvaluationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEvaluationsResponse
mkDescribeEvaluationsResponse responseStatus
  = DescribeEvaluationsResponse'{nextToken = Core.Nothing,
                                 results = Core.Nothing, responseStatus}

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsNextToken :: Lens.Lens' DescribeEvaluationsResponse (Core.Maybe Types.NextToken)
derfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE derfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @Evaluation@ that meet the search criteria. 
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsResults :: Lens.Lens' DescribeEvaluationsResponse (Core.Maybe [Types.Evaluation])
derfrsResults = Lens.field @"results"
{-# INLINEABLE derfrsResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsResponseStatus :: Lens.Lens' DescribeEvaluationsResponse Core.Int
derfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
