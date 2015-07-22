{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeEvaluations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DescribeEvaluations@ that match the search criteria
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
    , derqEQ
    , derqGE
    , derqPrefix
    , derqGT
    , derqNE
    , derqNextToken
    , derqSortOrder
    , derqLimit
    , derqLT
    , derqFilterVariable
    , derqLE

    -- * Response
    , DescribeEvaluationsResponse
    -- ** Response constructor
    , describeEvaluationsResponse
    -- ** Response lenses
    , desrsResults
    , desrsNextToken
    , desrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeEvaluations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derqEQ'
--
-- * 'derqGE'
--
-- * 'derqPrefix'
--
-- * 'derqGT'
--
-- * 'derqNE'
--
-- * 'derqNextToken'
--
-- * 'derqSortOrder'
--
-- * 'derqLimit'
--
-- * 'derqLT'
--
-- * 'derqFilterVariable'
--
-- * 'derqLE'
data DescribeEvaluations = DescribeEvaluations'
    { _derqEQ             :: !(Maybe Text)
    , _derqGE             :: !(Maybe Text)
    , _derqPrefix         :: !(Maybe Text)
    , _derqGT             :: !(Maybe Text)
    , _derqNE             :: !(Maybe Text)
    , _derqNextToken      :: !(Maybe Text)
    , _derqSortOrder      :: !(Maybe SortOrder)
    , _derqLimit          :: !(Maybe Nat)
    , _derqLT             :: !(Maybe Text)
    , _derqFilterVariable :: !(Maybe EvaluationFilterVariable)
    , _derqLE             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEvaluations' smart constructor.
describeEvaluations :: DescribeEvaluations
describeEvaluations =
    DescribeEvaluations'
    { _derqEQ = Nothing
    , _derqGE = Nothing
    , _derqPrefix = Nothing
    , _derqGT = Nothing
    , _derqNE = Nothing
    , _derqNextToken = Nothing
    , _derqSortOrder = Nothing
    , _derqLimit = Nothing
    , _derqLT = Nothing
    , _derqFilterVariable = Nothing
    , _derqLE = Nothing
    }

-- | The equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
derqEQ :: Lens' DescribeEvaluations (Maybe Text)
derqEQ = lens _derqEQ (\ s a -> s{_derqEQ = a});

-- | The greater than or equal to operator. The @Evaluation@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
derqGE :: Lens' DescribeEvaluations (Maybe Text)
derqGE = lens _derqGE (\ s a -> s{_derqGE = a});

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
derqPrefix :: Lens' DescribeEvaluations (Maybe Text)
derqPrefix = lens _derqPrefix (\ s a -> s{_derqPrefix = a});

-- | The greater than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
derqGT :: Lens' DescribeEvaluations (Maybe Text)
derqGT = lens _derqGT (\ s a -> s{_derqGT = a});

-- | The not equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
derqNE :: Lens' DescribeEvaluations (Maybe Text)
derqNE = lens _derqNE (\ s a -> s{_derqNE = a});

-- | The ID of the page in the paginated results.
derqNextToken :: Lens' DescribeEvaluations (Maybe Text)
derqNextToken = lens _derqNextToken (\ s a -> s{_derqNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @Evaluation@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
derqSortOrder :: Lens' DescribeEvaluations (Maybe SortOrder)
derqSortOrder = lens _derqSortOrder (\ s a -> s{_derqSortOrder = a});

-- | The maximum number of @Evaluation@ to include in the result.
derqLimit :: Lens' DescribeEvaluations (Maybe Natural)
derqLimit = lens _derqLimit (\ s a -> s{_derqLimit = a}) . mapping _Nat;

-- | The less than operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
derqLT :: Lens' DescribeEvaluations (Maybe Text)
derqLT = lens _derqLT (\ s a -> s{_derqLT = a});

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
derqFilterVariable :: Lens' DescribeEvaluations (Maybe EvaluationFilterVariable)
derqFilterVariable = lens _derqFilterVariable (\ s a -> s{_derqFilterVariable = a});

-- | The less than or equal to operator. The @Evaluation@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
derqLE :: Lens' DescribeEvaluations (Maybe Text)
derqLE = lens _derqLE (\ s a -> s{_derqLE = a});

instance AWSPager DescribeEvaluations where
        page rq rs
          | stop (rs ^. desrsNextToken) = Nothing
          | stop (rs ^. desrsResults) = Nothing
          | otherwise =
            Just $ rq & derqNextToken .~ rs ^. desrsNextToken

instance AWSRequest DescribeEvaluations where
        type Sv DescribeEvaluations = MachineLearning
        type Rs DescribeEvaluations =
             DescribeEvaluationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEvaluationsResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

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
              ["EQ" .= _derqEQ, "GE" .= _derqGE,
               "Prefix" .= _derqPrefix, "GT" .= _derqGT,
               "NE" .= _derqNE, "NextToken" .= _derqNextToken,
               "SortOrder" .= _derqSortOrder, "Limit" .= _derqLimit,
               "LT" .= _derqLT,
               "FilterVariable" .= _derqFilterVariable,
               "LE" .= _derqLE]

instance ToPath DescribeEvaluations where
        toPath = const "/"

instance ToQuery DescribeEvaluations where
        toQuery = const mempty

-- | Represents the query results from a DescribeEvaluations operation. The
-- content is essentially a list of @Evaluation@.
--
-- /See:/ 'describeEvaluationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrsResults'
--
-- * 'desrsNextToken'
--
-- * 'desrsStatus'
data DescribeEvaluationsResponse = DescribeEvaluationsResponse'
    { _desrsResults   :: !(Maybe [Evaluation])
    , _desrsNextToken :: !(Maybe Text)
    , _desrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEvaluationsResponse' smart constructor.
describeEvaluationsResponse :: Int -> DescribeEvaluationsResponse
describeEvaluationsResponse pStatus =
    DescribeEvaluationsResponse'
    { _desrsResults = Nothing
    , _desrsNextToken = Nothing
    , _desrsStatus = pStatus
    }

-- | A list of Evaluation that meet the search criteria.
desrsResults :: Lens' DescribeEvaluationsResponse [Evaluation]
desrsResults = lens _desrsResults (\ s a -> s{_desrsResults = a}) . _Default;

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
desrsNextToken :: Lens' DescribeEvaluationsResponse (Maybe Text)
desrsNextToken = lens _desrsNextToken (\ s a -> s{_desrsNextToken = a});

-- | FIXME: Undocumented member.
desrsStatus :: Lens' DescribeEvaluationsResponse Int
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});
