{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeBatchPredictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @BatchPrediction@ operations that match the search criteria in the request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeBatchPredictions
    (
    -- * Creating a request
      DescribeBatchPredictions (..)
    , mkDescribeBatchPredictions
    -- ** Request lenses
    , dbpEQ
    , dbpFilterVariable
    , dbpGE
    , dbpGT
    , dbpLE
    , dbpLT
    , dbpLimit
    , dbpNE
    , dbpNextToken
    , dbpPrefix
    , dbpSortOrder

    -- * Destructuring the response
    , DescribeBatchPredictionsResponse (..)
    , mkDescribeBatchPredictionsResponse
    -- ** Response lenses
    , dbprfrsNextToken
    , dbprfrsResults
    , dbprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBatchPredictions' smart constructor.
data DescribeBatchPredictions = DescribeBatchPredictions'
  { eq :: Core.Maybe Types.ComparatorValue
    -- ^ The equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
  , filterVariable :: Core.Maybe Types.BatchPredictionFilterVariable
    -- ^ Use one of the following variables to filter a list of @BatchPrediction@ :
--
--
--     * @CreatedAt@ - Sets the search criteria to the @BatchPrediction@ creation date.
--
--     * @Status@ - Sets the search criteria to the @BatchPrediction@ status.
--
--     * @Name@ - Sets the search criteria to the contents of the @BatchPrediction@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @BatchPrediction@ creation.
--
--     * @MLModelId@ - Sets the search criteria to the @MLModel@ used in the @BatchPrediction@ .
--
--     * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in the @BatchPrediction@ .
--
--     * @DataURI@ - Sets the search criteria to the data file(s) used in the @BatchPrediction@ . The URL can identify either a file or an Amazon Simple Storage Solution (Amazon S3) bucket or directory.
--
  , ge :: Core.Maybe Types.ComparatorValue
    -- ^ The greater than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ . 
  , gt :: Core.Maybe Types.ComparatorValue
    -- ^ The greater than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
  , le :: Core.Maybe Types.ComparatorValue
    -- ^ The less than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
  , lt :: Core.Maybe Types.ComparatorValue
    -- ^ The less than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
  , limit :: Core.Maybe Core.Natural
    -- ^ The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
  , ne :: Core.Maybe Types.ComparatorValue
    -- ^ The not equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
  , nextToken :: Core.Maybe Types.StringType
    -- ^ An ID of the page in the paginated results.
  , prefix :: Core.Maybe Types.ComparatorValue
    -- ^ A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, a @Batch Prediction@ operation could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @BatchPrediction@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ : 
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
    -- ^ A two-value parameter that determines the sequence of the resulting list of @MLModel@ s.
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

-- | Creates a 'DescribeBatchPredictions' value with any optional fields omitted.
mkDescribeBatchPredictions
    :: DescribeBatchPredictions
mkDescribeBatchPredictions
  = DescribeBatchPredictions'{eq = Core.Nothing,
                              filterVariable = Core.Nothing, ge = Core.Nothing,
                              gt = Core.Nothing, le = Core.Nothing, lt = Core.Nothing,
                              limit = Core.Nothing, ne = Core.Nothing, nextToken = Core.Nothing,
                              prefix = Core.Nothing, sortOrder = Core.Nothing}

-- | The equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- /Note:/ Consider using 'eq' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpEQ :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.ComparatorValue)
dbpEQ = Lens.field @"eq"
{-# INLINEABLE dbpEQ #-}
{-# DEPRECATED eq "Use generic-lens or generic-optics with 'eq' instead"  #-}

-- | Use one of the following variables to filter a list of @BatchPrediction@ :
--
--
--     * @CreatedAt@ - Sets the search criteria to the @BatchPrediction@ creation date.
--
--     * @Status@ - Sets the search criteria to the @BatchPrediction@ status.
--
--     * @Name@ - Sets the search criteria to the contents of the @BatchPrediction@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @BatchPrediction@ creation.
--
--     * @MLModelId@ - Sets the search criteria to the @MLModel@ used in the @BatchPrediction@ .
--
--     * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in the @BatchPrediction@ .
--
--     * @DataURI@ - Sets the search criteria to the data file(s) used in the @BatchPrediction@ . The URL can identify either a file or an Amazon Simple Storage Solution (Amazon S3) bucket or directory.
--
--
-- /Note:/ Consider using 'filterVariable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpFilterVariable :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.BatchPredictionFilterVariable)
dbpFilterVariable = Lens.field @"filterVariable"
{-# INLINEABLE dbpFilterVariable #-}
{-# DEPRECATED filterVariable "Use generic-lens or generic-optics with 'filterVariable' instead"  #-}

-- | The greater than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ . 
--
-- /Note:/ Consider using 'ge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpGE :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.ComparatorValue)
dbpGE = Lens.field @"ge"
{-# INLINEABLE dbpGE #-}
{-# DEPRECATED ge "Use generic-lens or generic-optics with 'ge' instead"  #-}

-- | The greater than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- /Note:/ Consider using 'gt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpGT :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.ComparatorValue)
dbpGT = Lens.field @"gt"
{-# INLINEABLE dbpGT #-}
{-# DEPRECATED gt "Use generic-lens or generic-optics with 'gt' instead"  #-}

-- | The less than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
--
-- /Note:/ Consider using 'le' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpLE :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.ComparatorValue)
dbpLE = Lens.field @"le"
{-# INLINEABLE dbpLE #-}
{-# DEPRECATED le "Use generic-lens or generic-optics with 'le' instead"  #-}

-- | The less than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- /Note:/ Consider using 'lt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpLT :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.ComparatorValue)
dbpLT = Lens.field @"lt"
{-# INLINEABLE dbpLT #-}
{-# DEPRECATED lt "Use generic-lens or generic-optics with 'lt' instead"  #-}

-- | The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpLimit :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Core.Natural)
dbpLimit = Lens.field @"limit"
{-# INLINEABLE dbpLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The not equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- /Note:/ Consider using 'ne' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpNE :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.ComparatorValue)
dbpNE = Lens.field @"ne"
{-# INLINEABLE dbpNE #-}
{-# DEPRECATED ne "Use generic-lens or generic-optics with 'ne' instead"  #-}

-- | An ID of the page in the paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpNextToken :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.StringType)
dbpNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, a @Batch Prediction@ operation could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @BatchPrediction@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ : 
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
dbpPrefix :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.ComparatorValue)
dbpPrefix = Lens.field @"prefix"
{-# INLINEABLE dbpPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | A two-value parameter that determines the sequence of the resulting list of @MLModel@ s.
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpSortOrder :: Lens.Lens' DescribeBatchPredictions (Core.Maybe Types.SortOrder)
dbpSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE dbpSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery DescribeBatchPredictions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBatchPredictions where
        toHeaders DescribeBatchPredictions{..}
          = Core.pure
              ("X-Amz-Target", "AmazonML_20141212.DescribeBatchPredictions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBatchPredictions where
        toJSON DescribeBatchPredictions{..}
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

instance Core.AWSRequest DescribeBatchPredictions where
        type Rs DescribeBatchPredictions = DescribeBatchPredictionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBatchPredictionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Results" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeBatchPredictions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"results" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the output of a @DescribeBatchPredictions@ operation. The content is essentially a list of @BatchPrediction@ s.
--
-- /See:/ 'mkDescribeBatchPredictionsResponse' smart constructor.
data DescribeBatchPredictionsResponse = DescribeBatchPredictionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The ID of the next page in the paginated results that indicates at least one more page follows.
  , results :: Core.Maybe [Types.BatchPrediction]
    -- ^ A list of @BatchPrediction@ objects that meet the search criteria. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBatchPredictionsResponse' value with any optional fields omitted.
mkDescribeBatchPredictionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBatchPredictionsResponse
mkDescribeBatchPredictionsResponse responseStatus
  = DescribeBatchPredictionsResponse'{nextToken = Core.Nothing,
                                      results = Core.Nothing, responseStatus}

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbprfrsNextToken :: Lens.Lens' DescribeBatchPredictionsResponse (Core.Maybe Types.NextToken)
dbprfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbprfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @BatchPrediction@ objects that meet the search criteria. 
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbprfrsResults :: Lens.Lens' DescribeBatchPredictionsResponse (Core.Maybe [Types.BatchPrediction])
dbprfrsResults = Lens.field @"results"
{-# INLINEABLE dbprfrsResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbprfrsResponseStatus :: Lens.Lens' DescribeBatchPredictionsResponse Core.Int
dbprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
