{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeEvaluations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DescribeEvaluations@ that match the search criteria in the request.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeEvaluations
    (
    -- * Creating a Request
      describeEvaluations
    , DescribeEvaluations
    -- * Request Lenses
    , deEQ
    , deGE
    , dePrefix
    , deGT
    , deNE
    , deNextToken
    , deSortOrder
    , deLimit
    , deLT
    , deFilterVariable
    , deLE

    -- * Destructuring the Response
    , describeEvaluationsResponse
    , DescribeEvaluationsResponse
    -- * Response Lenses
    , desrsResults
    , desrsNextToken
    , desrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEvaluations' smart constructor.
data DescribeEvaluations = DescribeEvaluations'
  { _deEQ             :: !(Maybe Text)
  , _deGE             :: !(Maybe Text)
  , _dePrefix         :: !(Maybe Text)
  , _deGT             :: !(Maybe Text)
  , _deNE             :: !(Maybe Text)
  , _deNextToken      :: !(Maybe Text)
  , _deSortOrder      :: !(Maybe SortOrder)
  , _deLimit          :: !(Maybe Nat)
  , _deLT             :: !(Maybe Text)
  , _deFilterVariable :: !(Maybe EvaluationFilterVariable)
  , _deLE             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvaluations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEQ' - The equal to operator. The @Evaluation@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- * 'deGE' - The greater than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- * 'dePrefix' - A string that is found at the beginning of a variable, such as @Name@ or @Id@ . For example, an @Evaluation@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @Evaluation@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :      * 2014-09     * 2014-09-09     * 2014-09-09-Holiday
--
-- * 'deGT' - The greater than operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- * 'deNE' - The not equal to operator. The @Evaluation@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- * 'deNextToken' - The ID of the page in the paginated results.
--
-- * 'deSortOrder' - A two-value parameter that determines the sequence of the resulting list of @Evaluation@ .     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).    * @dsc@ - Arranges the list in descending order (Z-A, 9-0). Results are sorted by @FilterVariable@ .
--
-- * 'deLimit' - The maximum number of @Evaluation@ to include in the result.
--
-- * 'deLT' - The less than operator. The @Evaluation@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- * 'deFilterVariable' - Use one of the following variable to filter a list of @Evaluation@ objects:     * @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation date.    * @Status@ - Sets the search criteria to the @Evaluation@ status.    * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked an @Evaluation@ .    * @MLModelId@ - Sets the search criteria to the @MLModel@ that was evaluated.    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in @Evaluation@ .    * @DataUri@ - Sets the search criteria to the data file(s) used in @Evaluation@ . The URL can identify either a file or an Amazon Simple Storage Solution (Amazon S3) bucket or directory.
--
-- * 'deLE' - The less than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
describeEvaluations
    :: DescribeEvaluations
describeEvaluations =
  DescribeEvaluations'
    { _deEQ = Nothing
    , _deGE = Nothing
    , _dePrefix = Nothing
    , _deGT = Nothing
    , _deNE = Nothing
    , _deNextToken = Nothing
    , _deSortOrder = Nothing
    , _deLimit = Nothing
    , _deLT = Nothing
    , _deFilterVariable = Nothing
    , _deLE = Nothing
    }


-- | The equal to operator. The @Evaluation@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
deEQ :: Lens' DescribeEvaluations (Maybe Text)
deEQ = lens _deEQ (\ s a -> s{_deEQ = a})

-- | The greater than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
deGE :: Lens' DescribeEvaluations (Maybe Text)
deGE = lens _deGE (\ s a -> s{_deGE = a})

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ . For example, an @Evaluation@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @Evaluation@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :      * 2014-09     * 2014-09-09     * 2014-09-09-Holiday
dePrefix :: Lens' DescribeEvaluations (Maybe Text)
dePrefix = lens _dePrefix (\ s a -> s{_dePrefix = a})

-- | The greater than operator. The @Evaluation@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
deGT :: Lens' DescribeEvaluations (Maybe Text)
deGT = lens _deGT (\ s a -> s{_deGT = a})

-- | The not equal to operator. The @Evaluation@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
deNE :: Lens' DescribeEvaluations (Maybe Text)
deNE = lens _deNE (\ s a -> s{_deNE = a})

-- | The ID of the page in the paginated results.
deNextToken :: Lens' DescribeEvaluations (Maybe Text)
deNextToken = lens _deNextToken (\ s a -> s{_deNextToken = a})

-- | A two-value parameter that determines the sequence of the resulting list of @Evaluation@ .     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).    * @dsc@ - Arranges the list in descending order (Z-A, 9-0). Results are sorted by @FilterVariable@ .
deSortOrder :: Lens' DescribeEvaluations (Maybe SortOrder)
deSortOrder = lens _deSortOrder (\ s a -> s{_deSortOrder = a})

-- | The maximum number of @Evaluation@ to include in the result.
deLimit :: Lens' DescribeEvaluations (Maybe Natural)
deLimit = lens _deLimit (\ s a -> s{_deLimit = a}) . mapping _Nat

-- | The less than operator. The @Evaluation@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
deLT :: Lens' DescribeEvaluations (Maybe Text)
deLT = lens _deLT (\ s a -> s{_deLT = a})

-- | Use one of the following variable to filter a list of @Evaluation@ objects:     * @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation date.    * @Status@ - Sets the search criteria to the @Evaluation@ status.    * @Name@ - Sets the search criteria to the contents of @Evaluation@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked an @Evaluation@ .    * @MLModelId@ - Sets the search criteria to the @MLModel@ that was evaluated.    * @DataSourceId@ - Sets the search criteria to the @DataSource@ used in @Evaluation@ .    * @DataUri@ - Sets the search criteria to the data file(s) used in @Evaluation@ . The URL can identify either a file or an Amazon Simple Storage Solution (Amazon S3) bucket or directory.
deFilterVariable :: Lens' DescribeEvaluations (Maybe EvaluationFilterVariable)
deFilterVariable = lens _deFilterVariable (\ s a -> s{_deFilterVariable = a})

-- | The less than or equal to operator. The @Evaluation@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
deLE :: Lens' DescribeEvaluations (Maybe Text)
deLE = lens _deLE (\ s a -> s{_deLE = a})

instance AWSPager DescribeEvaluations where
        page rq rs
          | stop (rs ^. desrsNextToken) = Nothing
          | stop (rs ^. desrsResults) = Nothing
          | otherwise =
            Just $ rq & deNextToken .~ rs ^. desrsNextToken

instance AWSRequest DescribeEvaluations where
        type Rs DescribeEvaluations =
             DescribeEvaluationsResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEvaluationsResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEvaluations where

instance NFData DescribeEvaluations where

instance ToHeaders DescribeEvaluations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DescribeEvaluations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEvaluations where
        toJSON DescribeEvaluations'{..}
          = object
              (catMaybes
                 [("EQ" .=) <$> _deEQ, ("GE" .=) <$> _deGE,
                  ("Prefix" .=) <$> _dePrefix, ("GT" .=) <$> _deGT,
                  ("NE" .=) <$> _deNE,
                  ("NextToken" .=) <$> _deNextToken,
                  ("SortOrder" .=) <$> _deSortOrder,
                  ("Limit" .=) <$> _deLimit, ("LT" .=) <$> _deLT,
                  ("FilterVariable" .=) <$> _deFilterVariable,
                  ("LE" .=) <$> _deLE])

instance ToPath DescribeEvaluations where
        toPath = const "/"

instance ToQuery DescribeEvaluations where
        toQuery = const mempty

-- | Represents the query results from a @DescribeEvaluations@ operation. The content is essentially a list of @Evaluation@ .
--
--
--
-- /See:/ 'describeEvaluationsResponse' smart constructor.
data DescribeEvaluationsResponse = DescribeEvaluationsResponse'
  { _desrsResults        :: !(Maybe [Evaluation])
  , _desrsNextToken      :: !(Maybe Text)
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvaluationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsResults' - A list of @Evaluation@ that meet the search criteria.
--
-- * 'desrsNextToken' - The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeEvaluationsResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeEvaluationsResponse
describeEvaluationsResponse pResponseStatus_ =
  DescribeEvaluationsResponse'
    { _desrsResults = Nothing
    , _desrsNextToken = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | A list of @Evaluation@ that meet the search criteria.
desrsResults :: Lens' DescribeEvaluationsResponse [Evaluation]
desrsResults = lens _desrsResults (\ s a -> s{_desrsResults = a}) . _Default . _Coerce

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
desrsNextToken :: Lens' DescribeEvaluationsResponse (Maybe Text)
desrsNextToken = lens _desrsNextToken (\ s a -> s{_desrsNextToken = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeEvaluationsResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeEvaluationsResponse where
