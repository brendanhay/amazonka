{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeBatchPredictions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @BatchPrediction@ operations that match the search
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
    , dbprqEQ
    , dbprqGE
    , dbprqPrefix
    , dbprqGT
    , dbprqNE
    , dbprqNextToken
    , dbprqSortOrder
    , dbprqLimit
    , dbprqLT
    , dbprqFilterVariable
    , dbprqLE

    -- * Response
    , DescribeBatchPredictionsResponse
    -- ** Response constructor
    , describeBatchPredictionsResponse
    -- ** Response lenses
    , drsResults
    , drsNextToken
    , drsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeBatchPredictions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbprqEQ'
--
-- * 'dbprqGE'
--
-- * 'dbprqPrefix'
--
-- * 'dbprqGT'
--
-- * 'dbprqNE'
--
-- * 'dbprqNextToken'
--
-- * 'dbprqSortOrder'
--
-- * 'dbprqLimit'
--
-- * 'dbprqLT'
--
-- * 'dbprqFilterVariable'
--
-- * 'dbprqLE'
data DescribeBatchPredictions = DescribeBatchPredictions'
    { _dbprqEQ             :: !(Maybe Text)
    , _dbprqGE             :: !(Maybe Text)
    , _dbprqPrefix         :: !(Maybe Text)
    , _dbprqGT             :: !(Maybe Text)
    , _dbprqNE             :: !(Maybe Text)
    , _dbprqNextToken      :: !(Maybe Text)
    , _dbprqSortOrder      :: !(Maybe SortOrder)
    , _dbprqLimit          :: !(Maybe Nat)
    , _dbprqLT             :: !(Maybe Text)
    , _dbprqFilterVariable :: !(Maybe BatchPredictionFilterVariable)
    , _dbprqLE             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeBatchPredictions' smart constructor.
describeBatchPredictions :: DescribeBatchPredictions
describeBatchPredictions =
    DescribeBatchPredictions'
    { _dbprqEQ = Nothing
    , _dbprqGE = Nothing
    , _dbprqPrefix = Nothing
    , _dbprqGT = Nothing
    , _dbprqNE = Nothing
    , _dbprqNextToken = Nothing
    , _dbprqSortOrder = Nothing
    , _dbprqLimit = Nothing
    , _dbprqLT = Nothing
    , _dbprqFilterVariable = Nothing
    , _dbprqLE = Nothing
    }

-- | The equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
dbprqEQ :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqEQ = lens _dbprqEQ (\ s a -> s{_dbprqEQ = a});

-- | The greater than or equal to operator. The @BatchPrediction@ results
-- will have @FilterVariable@ values that are greater than or equal to the
-- value specified with @GE@.
dbprqGE :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqGE = lens _dbprqGE (\ s a -> s{_dbprqGE = a});

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
dbprqPrefix :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqPrefix = lens _dbprqPrefix (\ s a -> s{_dbprqPrefix = a});

-- | The greater than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
dbprqGT :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqGT = lens _dbprqGT (\ s a -> s{_dbprqGT = a});

-- | The not equal to operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
dbprqNE :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqNE = lens _dbprqNE (\ s a -> s{_dbprqNE = a});

-- | An ID of the page in the paginated results.
dbprqNextToken :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqNextToken = lens _dbprqNextToken (\ s a -> s{_dbprqNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@s.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
dbprqSortOrder :: Lens' DescribeBatchPredictions (Maybe SortOrder)
dbprqSortOrder = lens _dbprqSortOrder (\ s a -> s{_dbprqSortOrder = a});

-- | The number of pages of information to include in the result. The range
-- of acceptable values is 1 through 100. The default value is 100.
dbprqLimit :: Lens' DescribeBatchPredictions (Maybe Natural)
dbprqLimit = lens _dbprqLimit (\ s a -> s{_dbprqLimit = a}) . mapping _Nat;

-- | The less than operator. The @BatchPrediction@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
dbprqLT :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqLT = lens _dbprqLT (\ s a -> s{_dbprqLT = a});

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
dbprqFilterVariable :: Lens' DescribeBatchPredictions (Maybe BatchPredictionFilterVariable)
dbprqFilterVariable = lens _dbprqFilterVariable (\ s a -> s{_dbprqFilterVariable = a});

-- | The less than or equal to operator. The @BatchPrediction@ results will
-- have @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
dbprqLE :: Lens' DescribeBatchPredictions (Maybe Text)
dbprqLE = lens _dbprqLE (\ s a -> s{_dbprqLE = a});

instance AWSPager DescribeBatchPredictions where
        page rq rs
          | stop (rs ^. drsNextToken) = Nothing
          | stop (rs ^. drsResults) = Nothing
          | otherwise =
            Just $ rq & dbprqNextToken .~ rs ^. drsNextToken

instance AWSRequest DescribeBatchPredictions where
        type Sv DescribeBatchPredictions = MachineLearning
        type Rs DescribeBatchPredictions =
             DescribeBatchPredictionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBatchPredictionsResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

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
              ["EQ" .= _dbprqEQ, "GE" .= _dbprqGE,
               "Prefix" .= _dbprqPrefix, "GT" .= _dbprqGT,
               "NE" .= _dbprqNE, "NextToken" .= _dbprqNextToken,
               "SortOrder" .= _dbprqSortOrder,
               "Limit" .= _dbprqLimit, "LT" .= _dbprqLT,
               "FilterVariable" .= _dbprqFilterVariable,
               "LE" .= _dbprqLE]

instance ToPath DescribeBatchPredictions where
        toPath = const "/"

instance ToQuery DescribeBatchPredictions where
        toQuery = const mempty

-- | Represents the output of a DescribeBatchPredictions operation. The
-- content is essentially a list of @BatchPrediction@s.
--
-- /See:/ 'describeBatchPredictionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsResults'
--
-- * 'drsNextToken'
--
-- * 'drsStatus'
data DescribeBatchPredictionsResponse = DescribeBatchPredictionsResponse'
    { _drsResults   :: !(Maybe [BatchPrediction])
    , _drsNextToken :: !(Maybe Text)
    , _drsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeBatchPredictionsResponse' smart constructor.
describeBatchPredictionsResponse :: Int -> DescribeBatchPredictionsResponse
describeBatchPredictionsResponse pStatus =
    DescribeBatchPredictionsResponse'
    { _drsResults = Nothing
    , _drsNextToken = Nothing
    , _drsStatus = pStatus
    }

-- | A list of BatchPrediction objects that meet the search criteria.
drsResults :: Lens' DescribeBatchPredictionsResponse [BatchPrediction]
drsResults = lens _drsResults (\ s a -> s{_drsResults = a}) . _Default;

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
drsNextToken :: Lens' DescribeBatchPredictionsResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a});

-- | FIXME: Undocumented member.
drsStatus :: Lens' DescribeBatchPredictionsResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
