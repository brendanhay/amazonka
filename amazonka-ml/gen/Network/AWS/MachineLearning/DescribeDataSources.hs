{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DescribeDataSources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DataSource@ that match the search criteria in the
-- request.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DescribeDataSources.html>
module Network.AWS.MachineLearning.DescribeDataSources
    (
    -- * Request
      DescribeDataSources
    -- ** Request constructor
    , describeDataSources
    -- ** Request lenses
    , ddsrqEQ
    , ddsrqGE
    , ddsrqPrefix
    , ddsrqGT
    , ddsrqNE
    , ddsrqNextToken
    , ddsrqSortOrder
    , ddsrqLimit
    , ddsrqLT
    , ddsrqFilterVariable
    , ddsrqLE

    -- * Response
    , DescribeDataSourcesResponse
    -- ** Response constructor
    , describeDataSourcesResponse
    -- ** Response lenses
    , ddssrsResults
    , ddssrsNextToken
    , ddssrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDataSources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrqEQ'
--
-- * 'ddsrqGE'
--
-- * 'ddsrqPrefix'
--
-- * 'ddsrqGT'
--
-- * 'ddsrqNE'
--
-- * 'ddsrqNextToken'
--
-- * 'ddsrqSortOrder'
--
-- * 'ddsrqLimit'
--
-- * 'ddsrqLT'
--
-- * 'ddsrqFilterVariable'
--
-- * 'ddsrqLE'
data DescribeDataSources = DescribeDataSources'
    { _ddsrqEQ             :: !(Maybe Text)
    , _ddsrqGE             :: !(Maybe Text)
    , _ddsrqPrefix         :: !(Maybe Text)
    , _ddsrqGT             :: !(Maybe Text)
    , _ddsrqNE             :: !(Maybe Text)
    , _ddsrqNextToken      :: !(Maybe Text)
    , _ddsrqSortOrder      :: !(Maybe SortOrder)
    , _ddsrqLimit          :: !(Maybe Nat)
    , _ddsrqLT             :: !(Maybe Text)
    , _ddsrqFilterVariable :: !(Maybe DataSourceFilterVariable)
    , _ddsrqLE             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDataSources' smart constructor.
describeDataSources :: DescribeDataSources
describeDataSources =
    DescribeDataSources'
    { _ddsrqEQ = Nothing
    , _ddsrqGE = Nothing
    , _ddsrqPrefix = Nothing
    , _ddsrqGT = Nothing
    , _ddsrqNE = Nothing
    , _ddsrqNextToken = Nothing
    , _ddsrqSortOrder = Nothing
    , _ddsrqLimit = Nothing
    , _ddsrqLT = Nothing
    , _ddsrqFilterVariable = Nothing
    , _ddsrqLE = Nothing
    }

-- | The equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
ddsrqEQ :: Lens' DescribeDataSources (Maybe Text)
ddsrqEQ = lens _ddsrqEQ (\ s a -> s{_ddsrqEQ = a});

-- | The greater than or equal to operator. The @DataSource@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
ddsrqGE :: Lens' DescribeDataSources (Maybe Text)
ddsrqGE = lens _ddsrqGE (\ s a -> s{_ddsrqGE = a});

-- | A string that is found at the beginning of a variable, such as @Name@ or
-- @Id@.
--
-- For example, a @DataSource@ could have the @Name@
-- @2014-09-09-HolidayGiftMailer@. To search for this @DataSource@, select
-- @Name@ for the @FilterVariable@ and any of the following strings for the
-- @Prefix@:
--
-- -   2014-09
--
-- -   2014-09-09
--
-- -   2014-09-09-Holiday
--
ddsrqPrefix :: Lens' DescribeDataSources (Maybe Text)
ddsrqPrefix = lens _ddsrqPrefix (\ s a -> s{_ddsrqPrefix = a});

-- | The greater than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
ddsrqGT :: Lens' DescribeDataSources (Maybe Text)
ddsrqGT = lens _ddsrqGT (\ s a -> s{_ddsrqGT = a});

-- | The not equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
ddsrqNE :: Lens' DescribeDataSources (Maybe Text)
ddsrqNE = lens _ddsrqNE (\ s a -> s{_ddsrqNE = a});

-- | The ID of the page in the paginated results.
ddsrqNextToken :: Lens' DescribeDataSources (Maybe Text)
ddsrqNextToken = lens _ddsrqNextToken (\ s a -> s{_ddsrqNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @DataSource@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
ddsrqSortOrder :: Lens' DescribeDataSources (Maybe SortOrder)
ddsrqSortOrder = lens _ddsrqSortOrder (\ s a -> s{_ddsrqSortOrder = a});

-- | The maximum number of @DataSource@ to include in the result.
ddsrqLimit :: Lens' DescribeDataSources (Maybe Natural)
ddsrqLimit = lens _ddsrqLimit (\ s a -> s{_ddsrqLimit = a}) . mapping _Nat;

-- | The less than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
ddsrqLT :: Lens' DescribeDataSources (Maybe Text)
ddsrqLT = lens _ddsrqLT (\ s a -> s{_ddsrqLT = a});

-- | Use one of the following variables to filter a list of @DataSource@:
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     dates.
-- -   @Status@ - Sets the search criteria to @DataSource@ statuses.
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     ____ @Name@.
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
ddsrqFilterVariable :: Lens' DescribeDataSources (Maybe DataSourceFilterVariable)
ddsrqFilterVariable = lens _ddsrqFilterVariable (\ s a -> s{_ddsrqFilterVariable = a});

-- | The less than or equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
ddsrqLE :: Lens' DescribeDataSources (Maybe Text)
ddsrqLE = lens _ddsrqLE (\ s a -> s{_ddsrqLE = a});

instance AWSPager DescribeDataSources where
        page rq rs
          | stop (rs ^. ddssrsNextToken) = Nothing
          | stop (rs ^. ddssrsResults) = Nothing
          | otherwise =
            Just $ rq & ddsrqNextToken .~ rs ^. ddssrsNextToken

instance AWSRequest DescribeDataSources where
        type Sv DescribeDataSources = MachineLearning
        type Rs DescribeDataSources =
             DescribeDataSourcesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDataSourcesResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDataSources where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DescribeDataSources" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDataSources where
        toJSON DescribeDataSources'{..}
          = object
              ["EQ" .= _ddsrqEQ, "GE" .= _ddsrqGE,
               "Prefix" .= _ddsrqPrefix, "GT" .= _ddsrqGT,
               "NE" .= _ddsrqNE, "NextToken" .= _ddsrqNextToken,
               "SortOrder" .= _ddsrqSortOrder,
               "Limit" .= _ddsrqLimit, "LT" .= _ddsrqLT,
               "FilterVariable" .= _ddsrqFilterVariable,
               "LE" .= _ddsrqLE]

instance ToPath DescribeDataSources where
        toPath = const "/"

instance ToQuery DescribeDataSources where
        toQuery = const mempty

-- | Represents the query results from a DescribeDataSources operation. The
-- content is essentially a list of @DataSource@.
--
-- /See:/ 'describeDataSourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddssrsResults'
--
-- * 'ddssrsNextToken'
--
-- * 'ddssrsStatus'
data DescribeDataSourcesResponse = DescribeDataSourcesResponse'
    { _ddssrsResults   :: !(Maybe [DataSource])
    , _ddssrsNextToken :: !(Maybe Text)
    , _ddssrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDataSourcesResponse' smart constructor.
describeDataSourcesResponse :: Int -> DescribeDataSourcesResponse
describeDataSourcesResponse pStatus_ =
    DescribeDataSourcesResponse'
    { _ddssrsResults = Nothing
    , _ddssrsNextToken = Nothing
    , _ddssrsStatus = pStatus_
    }

-- | A list of @DataSource@ that meet the search criteria.
ddssrsResults :: Lens' DescribeDataSourcesResponse [DataSource]
ddssrsResults = lens _ddssrsResults (\ s a -> s{_ddssrsResults = a}) . _Default;

-- | An ID of the next page in the paginated results that indicates at least
-- one more page follows.
ddssrsNextToken :: Lens' DescribeDataSourcesResponse (Maybe Text)
ddssrsNextToken = lens _ddssrsNextToken (\ s a -> s{_ddssrsNextToken = a});

-- | FIXME: Undocumented member.
ddssrsStatus :: Lens' DescribeDataSourcesResponse Int
ddssrsStatus = lens _ddssrsStatus (\ s a -> s{_ddssrsStatus = a});
