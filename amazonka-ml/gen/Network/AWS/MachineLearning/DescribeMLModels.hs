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
    , dmlmEQ
    , dmlmGE
    , dmlmPrefix
    , dmlmGT
    , dmlmNE
    , dmlmNextToken
    , dmlmSortOrder
    , dmlmLimit
    , dmlmLT
    , dmlmFilterVariable
    , dmlmLE

    -- * Response
    , DescribeMLModelsResponse
    -- ** Response constructor
    , describeMLModelsResponse
    -- ** Response lenses
    , descResults
    , descNextToken
    , descStatus
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
-- * 'dmlmEQ'
--
-- * 'dmlmGE'
--
-- * 'dmlmPrefix'
--
-- * 'dmlmGT'
--
-- * 'dmlmNE'
--
-- * 'dmlmNextToken'
--
-- * 'dmlmSortOrder'
--
-- * 'dmlmLimit'
--
-- * 'dmlmLT'
--
-- * 'dmlmFilterVariable'
--
-- * 'dmlmLE'
data DescribeMLModels = DescribeMLModels'
    { _dmlmEQ             :: !(Maybe Text)
    , _dmlmGE             :: !(Maybe Text)
    , _dmlmPrefix         :: !(Maybe Text)
    , _dmlmGT             :: !(Maybe Text)
    , _dmlmNE             :: !(Maybe Text)
    , _dmlmNextToken      :: !(Maybe Text)
    , _dmlmSortOrder      :: !(Maybe SortOrder)
    , _dmlmLimit          :: !(Maybe Nat)
    , _dmlmLT             :: !(Maybe Text)
    , _dmlmFilterVariable :: !(Maybe MLModelFilterVariable)
    , _dmlmLE             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMLModels' smart constructor.
describeMLModels :: DescribeMLModels
describeMLModels =
    DescribeMLModels'
    { _dmlmEQ = Nothing
    , _dmlmGE = Nothing
    , _dmlmPrefix = Nothing
    , _dmlmGT = Nothing
    , _dmlmNE = Nothing
    , _dmlmNextToken = Nothing
    , _dmlmSortOrder = Nothing
    , _dmlmLimit = Nothing
    , _dmlmLT = Nothing
    , _dmlmFilterVariable = Nothing
    , _dmlmLE = Nothing
    }

-- | The equal to operator. The @MLModel@ results will have @FilterVariable@
-- values that exactly match the value specified with @EQ@.
dmlmEQ :: Lens' DescribeMLModels (Maybe Text)
dmlmEQ = lens _dmlmEQ (\ s a -> s{_dmlmEQ = a});

-- | The greater than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
dmlmGE :: Lens' DescribeMLModels (Maybe Text)
dmlmGE = lens _dmlmGE (\ s a -> s{_dmlmGE = a});

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
dmlmPrefix :: Lens' DescribeMLModels (Maybe Text)
dmlmPrefix = lens _dmlmPrefix (\ s a -> s{_dmlmPrefix = a});

-- | The greater than operator. The @MLModel@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
dmlmGT :: Lens' DescribeMLModels (Maybe Text)
dmlmGT = lens _dmlmGT (\ s a -> s{_dmlmGT = a});

-- | The not equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
dmlmNE :: Lens' DescribeMLModels (Maybe Text)
dmlmNE = lens _dmlmNE (\ s a -> s{_dmlmNE = a});

-- | The ID of the page in the paginated results.
dmlmNextToken :: Lens' DescribeMLModels (Maybe Text)
dmlmNextToken = lens _dmlmNextToken (\ s a -> s{_dmlmNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @MLModel@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
dmlmSortOrder :: Lens' DescribeMLModels (Maybe SortOrder)
dmlmSortOrder = lens _dmlmSortOrder (\ s a -> s{_dmlmSortOrder = a});

-- | The number of pages of information to include in the result. The range
-- of acceptable values is 1 through 100. The default value is 100.
dmlmLimit :: Lens' DescribeMLModels (Maybe Natural)
dmlmLimit = lens _dmlmLimit (\ s a -> s{_dmlmLimit = a}) . mapping _Nat;

-- | The less than operator. The @MLModel@ results will have @FilterVariable@
-- values that are less than the value specified with @LT@.
dmlmLT :: Lens' DescribeMLModels (Maybe Text)
dmlmLT = lens _dmlmLT (\ s a -> s{_dmlmLT = a});

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
dmlmFilterVariable :: Lens' DescribeMLModels (Maybe MLModelFilterVariable)
dmlmFilterVariable = lens _dmlmFilterVariable (\ s a -> s{_dmlmFilterVariable = a});

-- | The less than or equal to operator. The @MLModel@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
dmlmLE :: Lens' DescribeMLModels (Maybe Text)
dmlmLE = lens _dmlmLE (\ s a -> s{_dmlmLE = a});

instance AWSPager DescribeMLModels where
        page rq rs
          | stop (rs ^. descNextToken) = Nothing
          | stop (rs ^. descResults) = Nothing
          | otherwise =
            Just $ rq & dmlmNextToken .~ rs ^. descNextToken

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
              ["EQ" .= _dmlmEQ, "GE" .= _dmlmGE,
               "Prefix" .= _dmlmPrefix, "GT" .= _dmlmGT,
               "NE" .= _dmlmNE, "NextToken" .= _dmlmNextToken,
               "SortOrder" .= _dmlmSortOrder, "Limit" .= _dmlmLimit,
               "LT" .= _dmlmLT,
               "FilterVariable" .= _dmlmFilterVariable,
               "LE" .= _dmlmLE]

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
-- * 'descResults'
--
-- * 'descNextToken'
--
-- * 'descStatus'
data DescribeMLModelsResponse = DescribeMLModelsResponse'
    { _descResults   :: !(Maybe [MLModel])
    , _descNextToken :: !(Maybe Text)
    , _descStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMLModelsResponse' smart constructor.
describeMLModelsResponse :: Int -> DescribeMLModelsResponse
describeMLModelsResponse pStatus =
    DescribeMLModelsResponse'
    { _descResults = Nothing
    , _descNextToken = Nothing
    , _descStatus = pStatus
    }

-- | A list of MLModel that meet the search criteria.
descResults :: Lens' DescribeMLModelsResponse [MLModel]
descResults = lens _descResults (\ s a -> s{_descResults = a}) . _Default;

-- | The ID of the next page in the paginated results that indicates at least
-- one more page follows.
descNextToken :: Lens' DescribeMLModelsResponse (Maybe Text)
descNextToken = lens _descNextToken (\ s a -> s{_descNextToken = a});

-- | FIXME: Undocumented member.
descStatus :: Lens' DescribeMLModelsResponse Int
descStatus = lens _descStatus (\ s a -> s{_descStatus = a});
