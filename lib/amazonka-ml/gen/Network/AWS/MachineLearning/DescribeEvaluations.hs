{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeEvaluations (..),
    mkDescribeEvaluations,

    -- ** Request lenses
    deEQ,
    deGE,
    dePrefix,
    deGT,
    deNE,
    deNextToken,
    deSortOrder,
    deLimit,
    deLT,
    deFilterVariable,
    deLE,

    -- * Destructuring the response
    DescribeEvaluationsResponse (..),
    mkDescribeEvaluationsResponse,

    -- ** Response lenses
    dersResults,
    dersNextToken,
    dersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEvaluations' smart constructor.
data DescribeEvaluations = DescribeEvaluations'
  { -- | The equal to operator. The @Evaluation@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
    eQ :: Lude.Maybe Lude.Text,
    -- | The greater than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
    gE :: Lude.Maybe Lude.Text,
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
    prefix :: Lude.Maybe Lude.Text,
    -- | The greater than operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
    gT :: Lude.Maybe Lude.Text,
    -- | The not equal to operator. The @Evaluation@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
    nE :: Lude.Maybe Lude.Text,
    -- | The ID of the page in the paginated results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A two-value parameter that determines the sequence of the resulting list of @Evaluation@ .
    --
    --
    --     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    --
    --     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | The maximum number of @Evaluation@ to include in the result.
    limit :: Lude.Maybe Lude.Natural,
    -- | The less than operator. The @Evaluation@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
    lT :: Lude.Maybe Lude.Text,
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
    filterVariable :: Lude.Maybe EvaluationFilterVariable,
    -- | The less than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
    lE :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvaluations' with the minimum fields required to make a request.
--
-- * 'eQ' - The equal to operator. The @Evaluation@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
-- * 'gE' - The greater than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
-- * 'prefix' - A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
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
-- * 'gT' - The greater than operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
-- * 'nE' - The not equal to operator. The @Evaluation@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
-- * 'nextToken' - The ID of the page in the paginated results.
-- * 'sortOrder' - A two-value parameter that determines the sequence of the resulting list of @Evaluation@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
-- * 'limit' - The maximum number of @Evaluation@ to include in the result.
-- * 'lT' - The less than operator. The @Evaluation@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
-- * 'filterVariable' - Use one of the following variable to filter a list of @Evaluation@ objects:
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
-- * 'lE' - The less than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
mkDescribeEvaluations ::
  DescribeEvaluations
mkDescribeEvaluations =
  DescribeEvaluations'
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

-- | The equal to operator. The @Evaluation@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- /Note:/ Consider using 'eQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEQ :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
deEQ = Lens.lens (eQ :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {eQ = a} :: DescribeEvaluations)
{-# DEPRECATED deEQ "Use generic-lens or generic-optics with 'eQ' instead." #-}

-- | The greater than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- /Note:/ Consider using 'gE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deGE :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
deGE = Lens.lens (gE :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {gE = a} :: DescribeEvaluations)
{-# DEPRECATED deGE "Use generic-lens or generic-optics with 'gE' instead." #-}

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
dePrefix :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
dePrefix = Lens.lens (prefix :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: DescribeEvaluations)
{-# DEPRECATED dePrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The greater than operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- /Note:/ Consider using 'gT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deGT :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
deGT = Lens.lens (gT :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {gT = a} :: DescribeEvaluations)
{-# DEPRECATED deGT "Use generic-lens or generic-optics with 'gT' instead." #-}

-- | The not equal to operator. The @Evaluation@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- /Note:/ Consider using 'nE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNE :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
deNE = Lens.lens (nE :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {nE = a} :: DescribeEvaluations)
{-# DEPRECATED deNE "Use generic-lens or generic-optics with 'nE' instead." #-}

-- | The ID of the page in the paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deNextToken :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
deNextToken = Lens.lens (nextToken :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEvaluations)
{-# DEPRECATED deNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
deSortOrder :: Lens.Lens' DescribeEvaluations (Lude.Maybe SortOrder)
deSortOrder = Lens.lens (sortOrder :: DescribeEvaluations -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: DescribeEvaluations)
{-# DEPRECATED deSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of @Evaluation@ to include in the result.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLimit :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Natural)
deLimit = Lens.lens (limit :: DescribeEvaluations -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeEvaluations)
{-# DEPRECATED deLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The less than operator. The @Evaluation@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- /Note:/ Consider using 'lT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLT :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
deLT = Lens.lens (lT :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {lT = a} :: DescribeEvaluations)
{-# DEPRECATED deLT "Use generic-lens or generic-optics with 'lT' instead." #-}

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
deFilterVariable :: Lens.Lens' DescribeEvaluations (Lude.Maybe EvaluationFilterVariable)
deFilterVariable = Lens.lens (filterVariable :: DescribeEvaluations -> Lude.Maybe EvaluationFilterVariable) (\s a -> s {filterVariable = a} :: DescribeEvaluations)
{-# DEPRECATED deFilterVariable "Use generic-lens or generic-optics with 'filterVariable' instead." #-}

-- | The less than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
--
-- /Note:/ Consider using 'lE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLE :: Lens.Lens' DescribeEvaluations (Lude.Maybe Lude.Text)
deLE = Lens.lens (lE :: DescribeEvaluations -> Lude.Maybe Lude.Text) (\s a -> s {lE = a} :: DescribeEvaluations)
{-# DEPRECATED deLE "Use generic-lens or generic-optics with 'lE' instead." #-}

instance Page.AWSPager DescribeEvaluations where
  page rq rs
    | Page.stop (rs Lens.^. dersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dersResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& deNextToken Lens..~ rs Lens.^. dersNextToken

instance Lude.AWSRequest DescribeEvaluations where
  type Rs DescribeEvaluations = DescribeEvaluationsResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEvaluationsResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEvaluations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DescribeEvaluations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEvaluations where
  toJSON DescribeEvaluations' {..} =
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

instance Lude.ToPath DescribeEvaluations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEvaluations where
  toQuery = Lude.const Lude.mempty

-- | Represents the query results from a @DescribeEvaluations@ operation. The content is essentially a list of @Evaluation@ .
--
-- /See:/ 'mkDescribeEvaluationsResponse' smart constructor.
data DescribeEvaluationsResponse = DescribeEvaluationsResponse'
  { -- | A list of @Evaluation@ that meet the search criteria.
    results :: Lude.Maybe [Evaluation],
    -- | The ID of the next page in the paginated results that indicates at least one more page follows.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvaluationsResponse' with the minimum fields required to make a request.
--
-- * 'results' - A list of @Evaluation@ that meet the search criteria.
-- * 'nextToken' - The ID of the next page in the paginated results that indicates at least one more page follows.
-- * 'responseStatus' - The response status code.
mkDescribeEvaluationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEvaluationsResponse
mkDescribeEvaluationsResponse pResponseStatus_ =
  DescribeEvaluationsResponse'
    { results = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @Evaluation@ that meet the search criteria.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResults :: Lens.Lens' DescribeEvaluationsResponse (Lude.Maybe [Evaluation])
dersResults = Lens.lens (results :: DescribeEvaluationsResponse -> Lude.Maybe [Evaluation]) (\s a -> s {results = a} :: DescribeEvaluationsResponse)
{-# DEPRECATED dersResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersNextToken :: Lens.Lens' DescribeEvaluationsResponse (Lude.Maybe Lude.Text)
dersNextToken = Lens.lens (nextToken :: DescribeEvaluationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeEvaluationsResponse)
{-# DEPRECATED dersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEvaluationsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEvaluationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEvaluationsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
