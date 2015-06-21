{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.MachineLearning.DescribeEvaluations
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of @DescribeEvaluations@ that match the search criteria
-- in the request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeEvaluations.html>
module Network.AWS.MachineLearning.DescribeEvaluations
    (
    -- * Request
      DescribeEvaluations
    -- ** Request constructor
    , describeEvaluations
    -- ** Request lenses
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

    -- * Response
    , DescribeEvaluationsResponse
    -- ** Response constructor
    , describeEvaluationsResponse
    -- ** Response lenses
    , derResults
    , derNextToken
    ) where

import Network.AWS.MachineLearning.Types
import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEvaluations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deEQ'
--
-- * 'deGE'
--
-- * 'dePrefix'
--
-- * 'deGT'
--
-- * 'deNE'
--
-- * 'deNextToken'
--
-- * 'deSortOrder'
--
-- * 'deLimit'
--
-- * 'deLT'
--
-- * 'deFilterVariable'
--
-- * 'deLE'
data DescribeEvaluations = DescribeEvaluations'{_deEQ :: Maybe Text, _deGE :: Maybe Text, _dePrefix :: Maybe Text, _deGT :: Maybe Text, _deNE :: Maybe Text, _deNextToken :: Maybe Text, _deSortOrder :: Maybe SortOrder, _deLimit :: Maybe Nat, _deLT :: Maybe Text, _deFilterVariable :: Maybe EvaluationFilterVariable, _deLE :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEvaluations' smart constructor.
describeEvaluations :: DescribeEvaluations
describeEvaluations = DescribeEvaluations'{_deEQ = Nothing, _deGE = Nothing, _dePrefix = Nothing, _deGT = Nothing, _deNE = Nothing, _deNextToken = Nothing, _deSortOrder = Nothing, _deLimit = Nothing, _deLT = Nothing, _deFilterVariable = Nothing, _deLE = Nothing};

-- | The equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
deEQ :: Lens' DescribeEvaluations (Maybe Text)
deEQ = lens _deEQ (\ s a -> s{_deEQ = a});

-- | The greater than or equal to operator. The @Evaluation@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
deGE :: Lens' DescribeEvaluations (Maybe Text)
deGE = lens _deGE (\ s a -> s{_deGE = a});

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, an @Evaluation@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @Evaluation@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
dePrefix :: Lens' DescribeEvaluations (Maybe Text)
dePrefix = lens _dePrefix (\ s a -> s{_dePrefix = a});

-- | The greater than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
deGT :: Lens' DescribeEvaluations (Maybe Text)
deGT = lens _deGT (\ s a -> s{_deGT = a});

-- | The not equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
deNE :: Lens' DescribeEvaluations (Maybe Text)
deNE = lens _deNE (\ s a -> s{_deNE = a});

-- | The ID of the page in the paginated results.
deNextToken :: Lens' DescribeEvaluations (Maybe Text)
deNextToken = lens _deNextToken (\ s a -> s{_deNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @Evaluation@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
deSortOrder :: Lens' DescribeEvaluations (Maybe SortOrder)
deSortOrder = lens _deSortOrder (\ s a -> s{_deSortOrder = a});

-- | The maximum number of @Evaluation@ to include in the result.
deLimit :: Lens' DescribeEvaluations (Maybe Natural)
deLimit = lens _deLimit (\ s a -> s{_deLimit = a}) . mapping _Nat;

-- | The less than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
deLT :: Lens' DescribeEvaluations (Maybe Text)
deLT = lens _deLT (\ s a -> s{_deLT = a});

-- | Use one of the following variable to filter a list of @Evaluation@
-- objects:
--
-- -   @CreatedAt@ - Sets the search criteria to the @Evaluation@ creation
--     date.
-- -   @Status@ - Sets the search criteria to the @Evaluation@ status.
-- -   @Name@ - Sets the search criteria to the contents of @Evaluation@
--     ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked an @Evaluation@.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ that was
--     evaluated.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in @Evaluation@.
-- -   @DataUri@ - Sets the search criteria to the data file(s) used in
--     @Evaluation@. The URL can identify either a file or an Amazon Simple
--     Storage Solution (Amazon S3) bucket or directory.
deFilterVariable :: Lens' DescribeEvaluations (Maybe EvaluationFilterVariable)
deFilterVariable = lens _deFilterVariable (\ s a -> s{_deFilterVariable = a});

-- | The less than or equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
deLE :: Lens' DescribeEvaluations (Maybe Text)
deLE = lens _deLE (\ s a -> s{_deLE = a});

instance AWSPager DescribeEvaluations where
        page rq rs
          | stop (rs ^. derNextToken) = Nothing
          | otherwise = rq & deNextToken ?~ rs ^. derNextToken

instance AWSRequest DescribeEvaluations where
        type Sv DescribeEvaluations = MachineLearning
        type Rs DescribeEvaluations =
             DescribeEvaluationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEvaluationsResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken"))

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
              ["EQ" .= _deEQ, "GE" .= _deGE, "Prefix" .= _dePrefix,
               "GT" .= _deGT, "NE" .= _deNE,
               "NextToken" .= _deNextToken,
               "SortOrder" .= _deSortOrder, "Limit" .= _deLimit,
               "LT" .= _deLT, "FilterVariable" .= _deFilterVariable,
               "LE" .= _deLE]

instance ToPath DescribeEvaluations where
        toPath = const "/"

instance ToQuery DescribeEvaluations where
        toQuery = const mempty

-- | /See:/ 'describeEvaluationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derResults'
--
-- * 'derNextToken'
data DescribeEvaluationsResponse = DescribeEvaluationsResponse'{_derResults :: Maybe [Evaluation], _derNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEvaluationsResponse' smart constructor.
describeEvaluationsResponse :: DescribeEvaluationsResponse
describeEvaluationsResponse = DescribeEvaluationsResponse'{_derResults = Nothing, _derNextToken = Nothing};

-- | A list of Evaluation that meet the search criteria.
derResults :: Lens' DescribeEvaluationsResponse [Evaluation]
derResults = lens _derResults (\ s a -> s{_derResults = a}) . _Default;

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
derNextToken :: Lens' DescribeEvaluationsResponse (Maybe Text)
derNextToken = lens _derNextToken (\ s a -> s{_derNextToken = a});
