{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeDataSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DataSource@ that match the search criteria in the request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeDataSources
  ( -- * Creating a request
    DescribeDataSources (..),
    mkDescribeDataSources,

    -- ** Request lenses
    ddsEQ,
    ddsGE,
    ddsPrefix,
    ddsGT,
    ddsNE,
    ddsNextToken,
    ddsSortOrder,
    ddsLimit,
    ddsLT,
    ddsFilterVariable,
    ddsLE,

    -- * Destructuring the response
    DescribeDataSourcesResponse (..),
    mkDescribeDataSourcesResponse,

    -- ** Response lenses
    ddssrsResults,
    ddssrsNextToken,
    ddssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDataSources' smart constructor.
data DescribeDataSources = DescribeDataSources'
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
      Lude.Maybe DataSourceFilterVariable,
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

-- | Creates a value of 'DescribeDataSources' with the minimum fields required to make a request.
--
-- * 'eQ' - The equal to operator. The @DataSource@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
-- * 'filterVariable' - Use one of the following variables to filter a list of @DataSource@ :
--
--
--     * @CreatedAt@ - Sets the search criteria to @DataSource@ creation dates.
--
--     * @Status@ - Sets the search criteria to @DataSource@ statuses.
--
--     * @Name@ - Sets the search criteria to the contents of @DataSource@ ____ @Name@ .
--
--     * @DataUri@ - Sets the search criteria to the URI of data files used to create the @DataSource@ . The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @DataSource@ creation.
--
-- * 'gE' - The greater than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
-- * 'gT' - The greater than operator. The @DataSource@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
-- * 'lE' - The less than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
-- * 'lT' - The less than operator. The @DataSource@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
-- * 'limit' - The maximum number of @DataSource@ to include in the result.
-- * 'nE' - The not equal to operator. The @DataSource@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
-- * 'nextToken' - The ID of the page in the paginated results.
-- * 'prefix' - A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, a @DataSource@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @DataSource@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :
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
-- * 'sortOrder' - A two-value parameter that determines the sequence of the resulting list of @DataSource@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
mkDescribeDataSources ::
  DescribeDataSources
mkDescribeDataSources =
  DescribeDataSources'
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

-- | The equal to operator. The @DataSource@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- /Note:/ Consider using 'eQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsEQ :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsEQ = Lens.lens (eQ :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {eQ = a} :: DescribeDataSources)
{-# DEPRECATED ddsEQ "Use generic-lens or generic-optics with 'eQ' instead." #-}

-- | The greater than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- /Note:/ Consider using 'gE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsGE :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsGE = Lens.lens (gE :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {gE = a} :: DescribeDataSources)
{-# DEPRECATED ddsGE "Use generic-lens or generic-optics with 'gE' instead." #-}

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, a @DataSource@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @DataSource@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :
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
ddsPrefix :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsPrefix = Lens.lens (prefix :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: DescribeDataSources)
{-# DEPRECATED ddsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The greater than operator. The @DataSource@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- /Note:/ Consider using 'gT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsGT :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsGT = Lens.lens (gT :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {gT = a} :: DescribeDataSources)
{-# DEPRECATED ddsGT "Use generic-lens or generic-optics with 'gT' instead." #-}

-- | The not equal to operator. The @DataSource@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- /Note:/ Consider using 'nE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsNE :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsNE = Lens.lens (nE :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {nE = a} :: DescribeDataSources)
{-# DEPRECATED ddsNE "Use generic-lens or generic-optics with 'nE' instead." #-}

-- | The ID of the page in the paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsNextToken :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsNextToken = Lens.lens (nextToken :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDataSources)
{-# DEPRECATED ddsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A two-value parameter that determines the sequence of the resulting list of @DataSource@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsSortOrder :: Lens.Lens' DescribeDataSources (Lude.Maybe SortOrder)
ddsSortOrder = Lens.lens (sortOrder :: DescribeDataSources -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: DescribeDataSources)
{-# DEPRECATED ddsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of @DataSource@ to include in the result.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsLimit :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Natural)
ddsLimit = Lens.lens (limit :: DescribeDataSources -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeDataSources)
{-# DEPRECATED ddsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The less than operator. The @DataSource@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- /Note:/ Consider using 'lT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsLT :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsLT = Lens.lens (lT :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {lT = a} :: DescribeDataSources)
{-# DEPRECATED ddsLT "Use generic-lens or generic-optics with 'lT' instead." #-}

-- | Use one of the following variables to filter a list of @DataSource@ :
--
--
--     * @CreatedAt@ - Sets the search criteria to @DataSource@ creation dates.
--
--     * @Status@ - Sets the search criteria to @DataSource@ statuses.
--
--     * @Name@ - Sets the search criteria to the contents of @DataSource@ ____ @Name@ .
--
--     * @DataUri@ - Sets the search criteria to the URI of data files used to create the @DataSource@ . The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @DataSource@ creation.
--
--
-- /Note:/ Consider using 'filterVariable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsFilterVariable :: Lens.Lens' DescribeDataSources (Lude.Maybe DataSourceFilterVariable)
ddsFilterVariable = Lens.lens (filterVariable :: DescribeDataSources -> Lude.Maybe DataSourceFilterVariable) (\s a -> s {filterVariable = a} :: DescribeDataSources)
{-# DEPRECATED ddsFilterVariable "Use generic-lens or generic-optics with 'filterVariable' instead." #-}

-- | The less than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
--
-- /Note:/ Consider using 'lE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsLE :: Lens.Lens' DescribeDataSources (Lude.Maybe Lude.Text)
ddsLE = Lens.lens (lE :: DescribeDataSources -> Lude.Maybe Lude.Text) (\s a -> s {lE = a} :: DescribeDataSources)
{-# DEPRECATED ddsLE "Use generic-lens or generic-optics with 'lE' instead." #-}

instance Page.AWSPager DescribeDataSources where
  page rq rs
    | Page.stop (rs Lens.^. ddssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddssrsResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddsNextToken Lens..~ rs Lens.^. ddssrsNextToken

instance Lude.AWSRequest DescribeDataSources where
  type Rs DescribeDataSources = DescribeDataSourcesResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDataSourcesResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDataSources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DescribeDataSources" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDataSources where
  toJSON DescribeDataSources' {..} =
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

instance Lude.ToPath DescribeDataSources where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDataSources where
  toQuery = Lude.const Lude.mempty

-- | Represents the query results from a 'DescribeDataSources' operation. The content is essentially a list of @DataSource@ .
--
-- /See:/ 'mkDescribeDataSourcesResponse' smart constructor.
data DescribeDataSourcesResponse = DescribeDataSourcesResponse'
  { results ::
      Lude.Maybe [DataSource],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDataSourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An ID of the next page in the paginated results that indicates at least one more page follows.
-- * 'responseStatus' - The response status code.
-- * 'results' - A list of @DataSource@ that meet the search criteria.
mkDescribeDataSourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDataSourcesResponse
mkDescribeDataSourcesResponse pResponseStatus_ =
  DescribeDataSourcesResponse'
    { results = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @DataSource@ that meet the search criteria.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddssrsResults :: Lens.Lens' DescribeDataSourcesResponse (Lude.Maybe [DataSource])
ddssrsResults = Lens.lens (results :: DescribeDataSourcesResponse -> Lude.Maybe [DataSource]) (\s a -> s {results = a} :: DescribeDataSourcesResponse)
{-# DEPRECATED ddssrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | An ID of the next page in the paginated results that indicates at least one more page follows.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddssrsNextToken :: Lens.Lens' DescribeDataSourcesResponse (Lude.Maybe Lude.Text)
ddssrsNextToken = Lens.lens (nextToken :: DescribeDataSourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDataSourcesResponse)
{-# DEPRECATED ddssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddssrsResponseStatus :: Lens.Lens' DescribeDataSourcesResponse Lude.Int
ddssrsResponseStatus = Lens.lens (responseStatus :: DescribeDataSourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDataSourcesResponse)
{-# DEPRECATED ddssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
