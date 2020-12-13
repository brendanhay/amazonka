{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeMLModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @MLModel@ that match the search criteria in the request.
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeMLModels
  ( -- * Creating a request
    DescribeMLModels (..),
    mkDescribeMLModels,

    -- ** Request lenses
    dmlmEQ,
    dmlmGE,
    dmlmPrefix,
    dmlmGT,
    dmlmNE,
    dmlmNextToken,
    dmlmSortOrder,
    dmlmLimit,
    dmlmLT,
    dmlmFilterVariable,
    dmlmLE,

    -- * Destructuring the response
    DescribeMLModelsResponse (..),
    mkDescribeMLModelsResponse,

    -- ** Response lenses
    dmlmrsResults,
    dmlmrsNextToken,
    dmlmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMLModels' smart constructor.
data DescribeMLModels = DescribeMLModels'
  { -- | The equal to operator. The @MLModel@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
    eQ :: Lude.Maybe Lude.Text,
    -- | The greater than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
    gE :: Lude.Maybe Lude.Text,
    -- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
    --
    -- For example, an @MLModel@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @MLModel@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :
    --
    --     * 2014-09
    --
    --
    --     * 2014-09-09
    --
    --
    --     * 2014-09-09-Holiday
    prefix :: Lude.Maybe Lude.Text,
    -- | The greater than operator. The @MLModel@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
    gT :: Lude.Maybe Lude.Text,
    -- | The not equal to operator. The @MLModel@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
    nE :: Lude.Maybe Lude.Text,
    -- | The ID of the page in the paginated results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A two-value parameter that determines the sequence of the resulting list of @MLModel@ .
    --
    --
    --     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
    --
    --     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
    --
    -- Results are sorted by @FilterVariable@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
    limit :: Lude.Maybe Lude.Natural,
    -- | The less than operator. The @MLModel@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
    lT :: Lude.Maybe Lude.Text,
    -- | Use one of the following variables to filter a list of @MLModel@ :
    --
    --
    --     * @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
    --
    --     * @Status@ - Sets the search criteria to @MLModel@ status.
    --
    --     * @Name@ - Sets the search criteria to the contents of @MLModel@ ____ @Name@ .
    --
    --     * @IAMUser@ - Sets the search criteria to the user account that invoked the @MLModel@ creation.
    --
    --     * @TrainingDataSourceId@ - Sets the search criteria to the @DataSource@ used to train one or more @MLModel@ .
    --
    --     * @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@ real-time endpoint status.
    --
    --     * @MLModelType@ - Sets the search criteria to @MLModel@ type: binary, regression, or multi-class.
    --
    --     * @Algorithm@ - Sets the search criteria to the algorithm that the @MLModel@ uses.
    --
    --     * @TrainingDataURI@ - Sets the search criteria to the data file(s) used in training a @MLModel@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
    filterVariable :: Lude.Maybe MLModelFilterVariable,
    -- | The less than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
    lE :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMLModels' with the minimum fields required to make a request.
--
-- * 'eQ' - The equal to operator. The @MLModel@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
-- * 'gE' - The greater than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
-- * 'prefix' - A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, an @MLModel@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @MLModel@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :
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
-- * 'gT' - The greater than operator. The @MLModel@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
-- * 'nE' - The not equal to operator. The @MLModel@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
-- * 'nextToken' - The ID of the page in the paginated results.
-- * 'sortOrder' - A two-value parameter that determines the sequence of the resulting list of @MLModel@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
-- * 'limit' - The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
-- * 'lT' - The less than operator. The @MLModel@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
-- * 'filterVariable' - Use one of the following variables to filter a list of @MLModel@ :
--
--
--     * @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
--
--     * @Status@ - Sets the search criteria to @MLModel@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @MLModel@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @MLModel@ creation.
--
--     * @TrainingDataSourceId@ - Sets the search criteria to the @DataSource@ used to train one or more @MLModel@ .
--
--     * @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@ real-time endpoint status.
--
--     * @MLModelType@ - Sets the search criteria to @MLModel@ type: binary, regression, or multi-class.
--
--     * @Algorithm@ - Sets the search criteria to the algorithm that the @MLModel@ uses.
--
--     * @TrainingDataURI@ - Sets the search criteria to the data file(s) used in training a @MLModel@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
-- * 'lE' - The less than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
mkDescribeMLModels ::
  DescribeMLModels
mkDescribeMLModels =
  DescribeMLModels'
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

-- | The equal to operator. The @MLModel@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- /Note:/ Consider using 'eQ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmEQ :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmEQ = Lens.lens (eQ :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {eQ = a} :: DescribeMLModels)
{-# DEPRECATED dmlmEQ "Use generic-lens or generic-optics with 'eQ' instead." #-}

-- | The greater than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- /Note:/ Consider using 'gE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmGE :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmGE = Lens.lens (gE :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {gE = a} :: DescribeMLModels)
{-# DEPRECATED dmlmGE "Use generic-lens or generic-optics with 'gE' instead." #-}

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ .
--
-- For example, an @MLModel@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @MLModel@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :
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
dmlmPrefix :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmPrefix = Lens.lens (prefix :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: DescribeMLModels)
{-# DEPRECATED dmlmPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The greater than operator. The @MLModel@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- /Note:/ Consider using 'gT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmGT :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmGT = Lens.lens (gT :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {gT = a} :: DescribeMLModels)
{-# DEPRECATED dmlmGT "Use generic-lens or generic-optics with 'gT' instead." #-}

-- | The not equal to operator. The @MLModel@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- /Note:/ Consider using 'nE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmNE :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmNE = Lens.lens (nE :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {nE = a} :: DescribeMLModels)
{-# DEPRECATED dmlmNE "Use generic-lens or generic-optics with 'nE' instead." #-}

-- | The ID of the page in the paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmNextToken :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmNextToken = Lens.lens (nextToken :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMLModels)
{-# DEPRECATED dmlmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A two-value parameter that determines the sequence of the resulting list of @MLModel@ .
--
--
--     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).
--
--     * @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmSortOrder :: Lens.Lens' DescribeMLModels (Lude.Maybe SortOrder)
dmlmSortOrder = Lens.lens (sortOrder :: DescribeMLModels -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: DescribeMLModels)
{-# DEPRECATED dmlmSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmLimit :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Natural)
dmlmLimit = Lens.lens (limit :: DescribeMLModels -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeMLModels)
{-# DEPRECATED dmlmLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The less than operator. The @MLModel@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- /Note:/ Consider using 'lT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmLT :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmLT = Lens.lens (lT :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {lT = a} :: DescribeMLModels)
{-# DEPRECATED dmlmLT "Use generic-lens or generic-optics with 'lT' instead." #-}

-- | Use one of the following variables to filter a list of @MLModel@ :
--
--
--     * @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
--
--     * @Status@ - Sets the search criteria to @MLModel@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @MLModel@ ____ @Name@ .
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @MLModel@ creation.
--
--     * @TrainingDataSourceId@ - Sets the search criteria to the @DataSource@ used to train one or more @MLModel@ .
--
--     * @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@ real-time endpoint status.
--
--     * @MLModelType@ - Sets the search criteria to @MLModel@ type: binary, regression, or multi-class.
--
--     * @Algorithm@ - Sets the search criteria to the algorithm that the @MLModel@ uses.
--
--     * @TrainingDataURI@ - Sets the search criteria to the data file(s) used in training a @MLModel@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
--
-- /Note:/ Consider using 'filterVariable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmFilterVariable :: Lens.Lens' DescribeMLModels (Lude.Maybe MLModelFilterVariable)
dmlmFilterVariable = Lens.lens (filterVariable :: DescribeMLModels -> Lude.Maybe MLModelFilterVariable) (\s a -> s {filterVariable = a} :: DescribeMLModels)
{-# DEPRECATED dmlmFilterVariable "Use generic-lens or generic-optics with 'filterVariable' instead." #-}

-- | The less than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
--
-- /Note:/ Consider using 'lE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmLE :: Lens.Lens' DescribeMLModels (Lude.Maybe Lude.Text)
dmlmLE = Lens.lens (lE :: DescribeMLModels -> Lude.Maybe Lude.Text) (\s a -> s {lE = a} :: DescribeMLModels)
{-# DEPRECATED dmlmLE "Use generic-lens or generic-optics with 'lE' instead." #-}

instance Page.AWSPager DescribeMLModels where
  page rq rs
    | Page.stop (rs Lens.^. dmlmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dmlmrsResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dmlmNextToken Lens..~ rs Lens.^. dmlmrsNextToken

instance Lude.AWSRequest DescribeMLModels where
  type Rs DescribeMLModels = DescribeMLModelsResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMLModelsResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMLModels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DescribeMLModels" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMLModels where
  toJSON DescribeMLModels' {..} =
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

instance Lude.ToPath DescribeMLModels where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMLModels where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DescribeMLModels@ operation. The content is essentially a list of @MLModel@ .
--
-- /See:/ 'mkDescribeMLModelsResponse' smart constructor.
data DescribeMLModelsResponse = DescribeMLModelsResponse'
  { -- | A list of @MLModel@ that meet the search criteria.
    results :: Lude.Maybe [MLModel],
    -- | The ID of the next page in the paginated results that indicates at least one more page follows.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMLModelsResponse' with the minimum fields required to make a request.
--
-- * 'results' - A list of @MLModel@ that meet the search criteria.
-- * 'nextToken' - The ID of the next page in the paginated results that indicates at least one more page follows.
-- * 'responseStatus' - The response status code.
mkDescribeMLModelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMLModelsResponse
mkDescribeMLModelsResponse pResponseStatus_ =
  DescribeMLModelsResponse'
    { results = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @MLModel@ that meet the search criteria.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrsResults :: Lens.Lens' DescribeMLModelsResponse (Lude.Maybe [MLModel])
dmlmrsResults = Lens.lens (results :: DescribeMLModelsResponse -> Lude.Maybe [MLModel]) (\s a -> s {results = a} :: DescribeMLModelsResponse)
{-# DEPRECATED dmlmrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrsNextToken :: Lens.Lens' DescribeMLModelsResponse (Lude.Maybe Lude.Text)
dmlmrsNextToken = Lens.lens (nextToken :: DescribeMLModelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeMLModelsResponse)
{-# DEPRECATED dmlmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrsResponseStatus :: Lens.Lens' DescribeMLModelsResponse Lude.Int
dmlmrsResponseStatus = Lens.lens (responseStatus :: DescribeMLModelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMLModelsResponse)
{-# DEPRECATED dmlmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
