{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.MachineLearning.DescribeBatchPredictions
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

-- | Returns a list of @BatchPrediction@ operations that match the search
-- criteria in the request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeBatchPredictions.html>
module Network.AWS.MachineLearning.DescribeBatchPredictions
    (
    -- * Request
      DescribeBatchPredictions
    -- ** Request constructor
    , describeBatchPredictions
    -- ** Request lenses
    , dbpEQ
    , dbpGE
    , dbpPrefix
    , dbpGT
    , dbpNE
    , dbpNextToken
    , dbpSortOrder
    , dbpLimit
    , dbpLT
    , dbpFilterVariable
    , dbpLE

    -- * Response
    , DescribeBatchPredictionsResponse
    -- ** Response constructor
    , describeBatchPredictionsResponse
    -- ** Response lenses
    , dbprResults
    , dbprNextToken
    ) where

import Network.AWS.MachineLearning.Types
import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBatchPredictions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpEQ'
--
-- * 'dbpGE'
--
-- * 'dbpPrefix'
--
-- * 'dbpGT'
--
-- * 'dbpNE'
--
-- * 'dbpNextToken'
--
-- * 'dbpSortOrder'
--
-- * 'dbpLimit'
--
-- * 'dbpLT'
--
-- * 'dbpFilterVariable'
--
-- * 'dbpLE'
data DescribeBatchPredictions = DescribeBatchPredictions'{_dbpEQ :: Maybe Text, _dbpGE :: Maybe Text, _dbpPrefix :: Maybe Text, _dbpGT :: Maybe Text, _dbpNE :: Maybe Text, _dbpNextToken :: Maybe Text, _dbpSortOrder :: Maybe SortOrder, _dbpLimit :: Maybe Nat, _dbpLT :: Maybe Text, _dbpFilterVariable :: Maybe BatchPredictionFilterVariable, _dbpLE :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeBatchPredictions' smart constructor.
describeBatchPredictions :: DescribeBatchPredictions
describeBatchPredictions = DescribeBatchPredictions'{_dbpEQ = Nothing, _dbpGE = Nothing, _dbpPrefix = Nothing, _dbpGT = Nothing, _dbpNE = Nothing, _dbpNextToken = Nothing, _dbpSortOrder = Nothing, _dbpLimit = Nothing, _dbpLT = Nothing, _dbpFilterVariable = Nothing, _dbpLE = Nothing};

-- | The equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
dbpEQ :: Lens' DescribeBatchPredictions (Maybe Text)
dbpEQ = lens _dbpEQ (\ s a -> s{_dbpEQ = a});

-- | The greater than or equal to operator. The @BatchPrediction@ results
-- will have @FilterVariable@ values that are greater than or equal to the
-- value specified with @GE@.
dbpGE :: Lens' DescribeBatchPredictions (Maybe Text)
dbpGE = lens _dbpGE (\ s a -> s{_dbpGE = a});

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, a @Batch Prediction@ operation could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @BatchPrediction@,
-- select @Name@ for the @FilterVariable@ and any of the following strings
-- for the @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
dbpPrefix :: Lens' DescribeBatchPredictions (Maybe Text)
dbpPrefix = lens _dbpPrefix (\ s a -> s{_dbpPrefix = a});

-- | The greater than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
dbpGT :: Lens' DescribeBatchPredictions (Maybe Text)
dbpGT = lens _dbpGT (\ s a -> s{_dbpGT = a});

-- | The not equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
dbpNE :: Lens' DescribeBatchPredictions (Maybe Text)
dbpNE = lens _dbpNE (\ s a -> s{_dbpNE = a});

-- | An ID of the page in the paginated results.
dbpNextToken :: Lens' DescribeBatchPredictions (Maybe Text)
dbpNextToken = lens _dbpNextToken (\ s a -> s{_dbpNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@s.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
dbpSortOrder :: Lens' DescribeBatchPredictions (Maybe SortOrder)
dbpSortOrder = lens _dbpSortOrder (\ s a -> s{_dbpSortOrder = a});

-- | The number of pages of information to include in the result. The range
-- of acceptable values is 1 through 100. The default value is 100.
dbpLimit :: Lens' DescribeBatchPredictions (Maybe Natural)
dbpLimit = lens _dbpLimit (\ s a -> s{_dbpLimit = a}) . mapping _Nat;

-- | The less than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
dbpLT :: Lens' DescribeBatchPredictions (Maybe Text)
dbpLT = lens _dbpLT (\ s a -> s{_dbpLT = a});

-- | Use one of the following variables to filter a list of
-- @BatchPrediction@:
--
-- -   @CreatedAt@ - Sets the search criteria to the @BatchPrediction@
--     creation date.
-- -   @Status@ - Sets the search criteria to the @BatchPrediction@ status.
-- -   @Name@ - Sets the search criteria to the contents of the
--     @BatchPrediction@ ____ @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @BatchPrediction@ creation.
-- -   @MLModelId@ - Sets the search criteria to the @MLModel@ used in the
--     @BatchPrediction@.
-- -   @DataSourceId@ - Sets the search criteria to the @DataSource@ used
--     in the @BatchPrediction@.
-- -   @DataURI@ - Sets the search criteria to the data file(s) used in the
--     @BatchPrediction@. The URL can identify either a file or an Amazon
--     Simple Storage Solution (Amazon S3) bucket or directory.
dbpFilterVariable :: Lens' DescribeBatchPredictions (Maybe BatchPredictionFilterVariable)
dbpFilterVariable = lens _dbpFilterVariable (\ s a -> s{_dbpFilterVariable = a});

-- | The less than or equal to operator. The @BatchPrediction@ results will
-- have @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
dbpLE :: Lens' DescribeBatchPredictions (Maybe Text)
dbpLE = lens _dbpLE (\ s a -> s{_dbpLE = a});

instance AWSPager DescribeBatchPredictions where
        page rq rs
          | stop (rs ^. dbprNextToken) = Nothing
          | otherwise =
            rq & dbpNextToken ?~ rs ^. dbprNextToken

instance AWSRequest DescribeBatchPredictions where
        type Sv DescribeBatchPredictions = MachineLearning
        type Rs DescribeBatchPredictions =
             DescribeBatchPredictionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBatchPredictionsResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken"))

instance ToHeaders DescribeBatchPredictions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DescribeBatchPredictions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBatchPredictions where
        toJSON DescribeBatchPredictions'{..}
          = object
              ["EQ" .= _dbpEQ, "GE" .= _dbpGE,
               "Prefix" .= _dbpPrefix, "GT" .= _dbpGT,
               "NE" .= _dbpNE, "NextToken" .= _dbpNextToken,
               "SortOrder" .= _dbpSortOrder, "Limit" .= _dbpLimit,
               "LT" .= _dbpLT,
               "FilterVariable" .= _dbpFilterVariable,
               "LE" .= _dbpLE]

instance ToPath DescribeBatchPredictions where
        toPath = const "/"

instance ToQuery DescribeBatchPredictions where
        toQuery = const mempty

-- | /See:/ 'describeBatchPredictionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbprResults'
--
-- * 'dbprNextToken'
data DescribeBatchPredictionsResponse = DescribeBatchPredictionsResponse'{_dbprResults :: Maybe [BatchPrediction], _dbprNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeBatchPredictionsResponse' smart constructor.
describeBatchPredictionsResponse :: DescribeBatchPredictionsResponse
describeBatchPredictionsResponse = DescribeBatchPredictionsResponse'{_dbprResults = Nothing, _dbprNextToken = Nothing};

-- | A list of BatchPrediction objects that meet the search criteria.
dbprResults :: Lens' DescribeBatchPredictionsResponse [BatchPrediction]
dbprResults = lens _dbprResults (\ s a -> s{_dbprResults = a}) . _Default;

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
dbprNextToken :: Lens' DescribeBatchPredictionsResponse (Maybe Text)
dbprNextToken = lens _dbprNextToken (\ s a -> s{_dbprNextToken = a});
