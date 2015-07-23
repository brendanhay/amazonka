{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeMLModels
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @MLModel@ that match the search criteria in the
-- request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeMLModels.html>
module Network.AWS.MachineLearning.DescribeMLModels
    (
    -- * Request
      DescribeMLModels
    -- ** Request constructor
    , describeMLModels
    -- ** Request lenses
    , dmlmrqEQ
    , dmlmrqGE
    , dmlmrqPrefix
    , dmlmrqGT
    , dmlmrqNE
    , dmlmrqNextToken
    , dmlmrqSortOrder
    , dmlmrqLimit
    , dmlmrqLT
    , dmlmrqFilterVariable
    , dmlmrqLE

    -- * Response
    , DescribeMLModelsResponse
    -- ** Response constructor
    , describeMLModelsResponse
    -- ** Response lenses
    , dmlmsrsResults
    , dmlmsrsNextToken
    , dmlmsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMLModels' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmrqEQ'
--
-- * 'dmlmrqGE'
--
-- * 'dmlmrqPrefix'
--
-- * 'dmlmrqGT'
--
-- * 'dmlmrqNE'
--
-- * 'dmlmrqNextToken'
--
-- * 'dmlmrqSortOrder'
--
-- * 'dmlmrqLimit'
--
-- * 'dmlmrqLT'
--
-- * 'dmlmrqFilterVariable'
--
-- * 'dmlmrqLE'
data DescribeMLModels = DescribeMLModels'
    { _dmlmrqEQ             :: !(Maybe Text)
    , _dmlmrqGE             :: !(Maybe Text)
    , _dmlmrqPrefix         :: !(Maybe Text)
    , _dmlmrqGT             :: !(Maybe Text)
    , _dmlmrqNE             :: !(Maybe Text)
    , _dmlmrqNextToken      :: !(Maybe Text)
    , _dmlmrqSortOrder      :: !(Maybe SortOrder)
    , _dmlmrqLimit          :: !(Maybe Nat)
    , _dmlmrqLT             :: !(Maybe Text)
    , _dmlmrqFilterVariable :: !(Maybe MLModelFilterVariable)
    , _dmlmrqLE             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMLModels' smart constructor.
describeMLModels :: DescribeMLModels
describeMLModels =
    DescribeMLModels'
    { _dmlmrqEQ = Nothing
    , _dmlmrqGE = Nothing
    , _dmlmrqPrefix = Nothing
    , _dmlmrqGT = Nothing
    , _dmlmrqNE = Nothing
    , _dmlmrqNextToken = Nothing
    , _dmlmrqSortOrder = Nothing
    , _dmlmrqLimit = Nothing
    , _dmlmrqLT = Nothing
    , _dmlmrqFilterVariable = Nothing
    , _dmlmrqLE = Nothing
    }

-- | The equal to operator. The @MLModel@ results will have @FilterVariable@
-- values that exactly match the value specified with @EQ@.
dmlmrqEQ :: Lens' DescribeMLModels (Maybe Text)
dmlmrqEQ = lens _dmlmrqEQ (\ s a -> s{_dmlmrqEQ = a});

-- | The greater than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
dmlmrqGE :: Lens' DescribeMLModels (Maybe Text)
dmlmrqGE = lens _dmlmrqGE (\ s a -> s{_dmlmrqGE = a});

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, an @MLModel@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @MLModel@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
dmlmrqPrefix :: Lens' DescribeMLModels (Maybe Text)
dmlmrqPrefix = lens _dmlmrqPrefix (\ s a -> s{_dmlmrqPrefix = a});

-- | The greater than operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
dmlmrqGT :: Lens' DescribeMLModels (Maybe Text)
dmlmrqGT = lens _dmlmrqGT (\ s a -> s{_dmlmrqGT = a});

-- | The not equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
dmlmrqNE :: Lens' DescribeMLModels (Maybe Text)
dmlmrqNE = lens _dmlmrqNE (\ s a -> s{_dmlmrqNE = a});

-- | The ID of the page in the paginated results.
dmlmrqNextToken :: Lens' DescribeMLModels (Maybe Text)
dmlmrqNextToken = lens _dmlmrqNextToken (\ s a -> s{_dmlmrqNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
dmlmrqSortOrder :: Lens' DescribeMLModels (Maybe SortOrder)
dmlmrqSortOrder = lens _dmlmrqSortOrder (\ s a -> s{_dmlmrqSortOrder = a});

-- | The number of pages of information to include in the result. The range
-- of acceptable values is 1 through 100. The default value is 100.
dmlmrqLimit :: Lens' DescribeMLModels (Maybe Natural)
dmlmrqLimit = lens _dmlmrqLimit (\ s a -> s{_dmlmrqLimit = a}) . mapping _Nat;

-- | The less than operator. The @MLModel@ results will have @FilterVariable@
-- values that are less than the value specified with @LT@.
dmlmrqLT :: Lens' DescribeMLModels (Maybe Text)
dmlmrqLT = lens _dmlmrqLT (\ s a -> s{_dmlmrqLT = a});

-- | Use one of the following variables to filter a list of @MLModel@:
--
-- -   @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.
-- -   @Status@ - Sets the search criteria to @MLModel@ status.
-- -   @Name@ - Sets the search criteria to the contents of @MLModel@ ____
--     @Name@.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @MLModel@ creation.
-- -   @TrainingDataSourceId@ - Sets the search criteria to the
--     @DataSource@ used to train one or more @MLModel@.
-- -   @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@
--     real-time endpoint status.
-- -   @MLModelType@ - Sets the search criteria to @MLModel@ type: binary,
--     regression, or multi-class.
-- -   @Algorithm@ - Sets the search criteria to the algorithm that the
--     @MLModel@ uses.
-- -   @TrainingDataURI@ - Sets the search criteria to the data file(s)
--     used in training a @MLModel@. The URL can identify either a file or
--     an Amazon Simple Storage Service (Amazon S3) bucket or directory.
dmlmrqFilterVariable :: Lens' DescribeMLModels (Maybe MLModelFilterVariable)
dmlmrqFilterVariable = lens _dmlmrqFilterVariable (\ s a -> s{_dmlmrqFilterVariable = a});

-- | The less than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
dmlmrqLE :: Lens' DescribeMLModels (Maybe Text)
dmlmrqLE = lens _dmlmrqLE (\ s a -> s{_dmlmrqLE = a});

instance AWSPager DescribeMLModels where
        page rq rs
          | stop (rs ^. dmlmsrsNextToken) = Nothing
          | stop (rs ^. dmlmsrsResults) = Nothing
          | otherwise =
            Just $ rq & dmlmrqNextToken .~ rs ^. dmlmsrsNextToken

instance AWSRequest DescribeMLModels where
        type Sv DescribeMLModels = MachineLearning
        type Rs DescribeMLModels = DescribeMLModelsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMLModelsResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeMLModels where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DescribeMLModels" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMLModels where
        toJSON DescribeMLModels'{..}
          = object
              ["EQ" .= _dmlmrqEQ, "GE" .= _dmlmrqGE,
               "Prefix" .= _dmlmrqPrefix, "GT" .= _dmlmrqGT,
               "NE" .= _dmlmrqNE, "NextToken" .= _dmlmrqNextToken,
               "SortOrder" .= _dmlmrqSortOrder,
               "Limit" .= _dmlmrqLimit, "LT" .= _dmlmrqLT,
               "FilterVariable" .= _dmlmrqFilterVariable,
               "LE" .= _dmlmrqLE]

instance ToPath DescribeMLModels where
        toPath = const "/"

instance ToQuery DescribeMLModels where
        toQuery = const mempty

-- | Represents the output of a DescribeMLModels operation. The content is
-- essentially a list of @MLModel@.
--
-- /See:/ 'describeMLModelsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmsrsResults'
--
-- * 'dmlmsrsNextToken'
--
-- * 'dmlmsrsStatus'
data DescribeMLModelsResponse = DescribeMLModelsResponse'
    { _dmlmsrsResults   :: !(Maybe [MLModel])
    , _dmlmsrsNextToken :: !(Maybe Text)
    , _dmlmsrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMLModelsResponse' smart constructor.
describeMLModelsResponse :: Int -> DescribeMLModelsResponse
describeMLModelsResponse pStatus_ =
    DescribeMLModelsResponse'
    { _dmlmsrsResults = Nothing
    , _dmlmsrsNextToken = Nothing
    , _dmlmsrsStatus = pStatus_
    }

-- | A list of MLModel that meet the search criteria.
dmlmsrsResults :: Lens' DescribeMLModelsResponse [MLModel]
dmlmsrsResults = lens _dmlmsrsResults (\ s a -> s{_dmlmsrsResults = a}) . _Default;

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
dmlmsrsNextToken :: Lens' DescribeMLModelsResponse (Maybe Text)
dmlmsrsNextToken = lens _dmlmsrsNextToken (\ s a -> s{_dmlmsrsNextToken = a});

-- | FIXME: Undocumented member.
dmlmsrsStatus :: Lens' DescribeMLModelsResponse Int
dmlmsrsStatus = lens _dmlmsrsStatus (\ s a -> s{_dmlmsrsStatus = a});
