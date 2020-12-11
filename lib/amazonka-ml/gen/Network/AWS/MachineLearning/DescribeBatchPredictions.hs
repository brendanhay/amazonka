{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeBatchPredictions (..),
    mkDescribeBatchPredictions,

    -- ** Request lenses
    dbpEQ,
    dbpGE,
    dbpPrefix,
    dbpGT,
    dbpNE,
    dbpNextToken,
    dbpSortOrder,
    dbpLimit,
    dbpLT,
    dbpFilterVariable,
    dbpLE,

    -- * Destructuring the response
    DescribeBatchPredictionsResponse (..),
    mkDescribeBatchPredictionsResponse,

    -- ** Response lenses
    dbpsrsResults,
    dbpsrsNextToken,
    dbpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBatchPredictions' smart constructor.
data DescribeBatchPredictions = DescribeBatchPredictions'
  { eQ ::
      Lude.Maybe Lude.Text,
    gE :: Lude.Maybe Lude.Text,
    prefix :: Lude.Maybe Lude.Text,
    gT :: Lude.Maybe Lude.Text,
    nE :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    limit :: Lude.Maybe Lude.Natural,
    lT :: Lude.Maybe Lude.Text,
    filterVariable ::
      Lude.Maybe BatchPredictionFilterVariable,
    lE :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBatchPredictions' with the minimum fields required to make a request.
--
-- * 'eQ' - The equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
-- * 'filterVariable' - Use one of the following variables to filter a list of @BatchPrediction@ :
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
-- * 'gE' - The greater than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
-- * 'gT' - The greater than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
-- * 'lE' - The less than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
-- * 'lT' - The less than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
-- * 'limit' - The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
-- * 'nE' - The not equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
-- * 'nextToken' - An ID of the page in the paginated results.
-- * 'prefix' - A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
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
-- * 'sortOrder' - A two-value parameter that determines the sequence of the resulting list of @MLModel@ s.
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
mkDescribeBatchPredictions ::
  DescribeBatchPredictions
mkDescribeBatchPredictions =
  DescribeBatchPredictions'
    { eQ = Lude.Nothing,
      gE = Lude.Nothing,
      prefix = Lude.Nothing,
      gT = Lude.Nothing,
      nE = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      limit = Lude.Nothing,
      lT = Lude.Nothing,
      filterVariable = Lude.Nothing,
      lE = Lude.Nothing
    }

-- | The equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- /Note:/ Consider using 'eQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpEQ :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpEQ = Lens.lens (eQ :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {eQ = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpEQ "Use generic-lens or generic-optics with 'eQ' instead." #-}

-- | The greater than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- /Note:/ Consider using 'gE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpGE :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpGE = Lens.lens (gE :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {gE = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpGE "Use generic-lens or generic-optics with 'gE' instead." #-}

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
dbpPrefix :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpPrefix = Lens.lens (prefix :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The greater than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- /Note:/ Consider using 'gT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpGT :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpGT = Lens.lens (gT :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {gT = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpGT "Use generic-lens or generic-optics with 'gT' instead." #-}

-- | The not equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- /Note:/ Consider using 'nE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpNE :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpNE = Lens.lens (nE :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {nE = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpNE "Use generic-lens or generic-optics with 'nE' instead." #-}

-- | An ID of the page in the paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpNextToken :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpNextToken = Lens.lens (nextToken :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
dbpSortOrder :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe SortOrder)
dbpSortOrder = Lens.lens (sortOrder :: DescribeBatchPredictions -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpLimit :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Natural)
dbpLimit = Lens.lens (limit :: DescribeBatchPredictions -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The less than operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- /Note:/ Consider using 'lT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpLT :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpLT = Lens.lens (lT :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {lT = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpLT "Use generic-lens or generic-optics with 'lT' instead." #-}

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
dbpFilterVariable :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe BatchPredictionFilterVariable)
dbpFilterVariable = Lens.lens (filterVariable :: DescribeBatchPredictions -> Lude.Maybe BatchPredictionFilterVariable) (\s a -> s {filterVariable = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpFilterVariable "Use generic-lens or generic-optics with 'filterVariable' instead." #-}

-- | The less than or equal to operator. The @BatchPrediction@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
--
-- /Note:/ Consider using 'lE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpLE :: Lens.Lens' DescribeBatchPredictions (Lude.Maybe Lude.Text)
dbpLE = Lens.lens (lE :: DescribeBatchPredictions -> Lude.Maybe Lude.Text) (\s a -> s {lE = a} :: DescribeBatchPredictions)
{-# DEPRECATED dbpLE "Use generic-lens or generic-optics with 'lE' instead." #-}

instance Page.AWSPager DescribeBatchPredictions where
  page rq rs
    | Page.stop (rs Lens.^. dbpsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbpsrsResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbpNextToken Lens..~ rs Lens.^. dbpsrsNextToken

instance Lude.AWSRequest DescribeBatchPredictions where
  type Rs DescribeBatchPredictions = DescribeBatchPredictionsResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBatchPredictionsResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBatchPredictions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DescribeBatchPredictions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBatchPredictions where
  toJSON DescribeBatchPredictions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EQ" Lude..=) Lude.<$> eQ,
            ("GE" Lude..=) Lude.<$> gE,
            ("Prefix" Lude..=) Lude.<$> prefix,
            ("GT" Lude..=) Lude.<$> gT,
            ("NE" Lude..=) Lude.<$> nE,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("Limit" Lude..=) Lude.<$> limit,
            ("LT" Lude..=) Lude.<$> lT,
            ("FilterVariable" Lude..=) Lude.<$> filterVariable,
            ("LE" Lude..=) Lude.<$> lE
          ]
      )

instance Lude.ToPath DescribeBatchPredictions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBatchPredictions where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DescribeBatchPredictions@ operation. The content is essentially a list of @BatchPrediction@ s.
--
-- /See:/ 'mkDescribeBatchPredictionsResponse' smart constructor.
data DescribeBatchPredictionsResponse = DescribeBatchPredictionsResponse'
  { results ::
      Lude.Maybe
        [BatchPrediction],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBatchPredictionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The ID of the next page in the paginated results that indicates at least one more page follows.
-- * 'responseStatus' - The response status code.
-- * 'results' - A list of @BatchPrediction@ objects that meet the search criteria.
mkDescribeBatchPredictionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBatchPredictionsResponse
mkDescribeBatchPredictionsResponse pResponseStatus_ =
  DescribeBatchPredictionsResponse'
    { results = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @BatchPrediction@ objects that meet the search criteria.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpsrsResults :: Lens.Lens' DescribeBatchPredictionsResponse (Lude.Maybe [BatchPrediction])
dbpsrsResults = Lens.lens (results :: DescribeBatchPredictionsResponse -> Lude.Maybe [BatchPrediction]) (\s a -> s {results = a} :: DescribeBatchPredictionsResponse)
{-# DEPRECATED dbpsrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpsrsNextToken :: Lens.Lens' DescribeBatchPredictionsResponse (Lude.Maybe Lude.Text)
dbpsrsNextToken = Lens.lens (nextToken :: DescribeBatchPredictionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBatchPredictionsResponse)
{-# DEPRECATED dbpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpsrsResponseStatus :: Lens.Lens' DescribeBatchPredictionsResponse Lude.Int
dbpsrsResponseStatus = Lens.lens (responseStatus :: DescribeBatchPredictionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBatchPredictionsResponse)
{-# DEPRECATED dbpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
