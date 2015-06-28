{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.DescribeDataSources
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

-- | Returns a list of @DataSource@ that match the search criteria in the
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
    , ddsEQ
    , ddsGE
    , ddsPrefix
    , ddsGT
    , ddsNE
    , ddsNextToken
    , ddsSortOrder
    , ddsLimit
    , ddsLT
    , ddsFilterVariable
    , ddsLE

    -- * Response
    , DescribeDataSourcesResponse
    -- ** Response constructor
    , describeDataSourcesResponse
    -- ** Response lenses
    , dResults
    , dNextToken
    , dStatus
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
-- * 'ddsEQ'
--
-- * 'ddsGE'
--
-- * 'ddsPrefix'
--
-- * 'ddsGT'
--
-- * 'ddsNE'
--
-- * 'ddsNextToken'
--
-- * 'ddsSortOrder'
--
-- * 'ddsLimit'
--
-- * 'ddsLT'
--
-- * 'ddsFilterVariable'
--
-- * 'ddsLE'
data DescribeDataSources = DescribeDataSources'
    { _ddsEQ             :: !(Maybe Text)
    , _ddsGE             :: !(Maybe Text)
    , _ddsPrefix         :: !(Maybe Text)
    , _ddsGT             :: !(Maybe Text)
    , _ddsNE             :: !(Maybe Text)
    , _ddsNextToken      :: !(Maybe Text)
    , _ddsSortOrder      :: !(Maybe SortOrder)
    , _ddsLimit          :: !(Maybe Nat)
    , _ddsLT             :: !(Maybe Text)
    , _ddsFilterVariable :: !(Maybe DataSourceFilterVariable)
    , _ddsLE             :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeDataSources' smart constructor.
describeDataSources :: DescribeDataSources
describeDataSources =
    DescribeDataSources'
    { _ddsEQ = Nothing
    , _ddsGE = Nothing
    , _ddsPrefix = Nothing
    , _ddsGT = Nothing
    , _ddsNE = Nothing
    , _ddsNextToken = Nothing
    , _ddsSortOrder = Nothing
    , _ddsLimit = Nothing
    , _ddsLT = Nothing
    , _ddsFilterVariable = Nothing
    , _ddsLE = Nothing
    }

-- | The equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that exactly match the value specified with
-- @EQ@.
ddsEQ :: Lens' DescribeDataSources (Maybe Text)
ddsEQ = lens _ddsEQ (\ s a -> s{_ddsEQ = a});

-- | The greater than or equal to operator. The @DataSource@ results will
-- have @FilterVariable@ values that are greater than or equal to the value
-- specified with @GE@.
ddsGE :: Lens' DescribeDataSources (Maybe Text)
ddsGE = lens _ddsGE (\ s a -> s{_ddsGE = a});

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
ddsPrefix :: Lens' DescribeDataSources (Maybe Text)
ddsPrefix = lens _ddsPrefix (\ s a -> s{_ddsPrefix = a});

-- | The greater than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are greater than the value specified with
-- @GT@.
ddsGT :: Lens' DescribeDataSources (Maybe Text)
ddsGT = lens _ddsGT (\ s a -> s{_ddsGT = a});

-- | The not equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values not equal to the value specified with @NE@.
ddsNE :: Lens' DescribeDataSources (Maybe Text)
ddsNE = lens _ddsNE (\ s a -> s{_ddsNE = a});

-- | The ID of the page in the paginated results.
ddsNextToken :: Lens' DescribeDataSources (Maybe Text)
ddsNextToken = lens _ddsNextToken (\ s a -> s{_ddsNextToken = a});

-- | A two-value parameter that determines the sequence of the resulting list
-- of @DataSource@.
--
-- -   @asc@ - Arranges the list in ascending order (A-Z, 0-9).
-- -   @dsc@ - Arranges the list in descending order (Z-A, 9-0).
--
-- Results are sorted by @FilterVariable@.
ddsSortOrder :: Lens' DescribeDataSources (Maybe SortOrder)
ddsSortOrder = lens _ddsSortOrder (\ s a -> s{_ddsSortOrder = a});

-- | The maximum number of @DataSource@ to include in the result.
ddsLimit :: Lens' DescribeDataSources (Maybe Natural)
ddsLimit = lens _ddsLimit (\ s a -> s{_ddsLimit = a}) . mapping _Nat;

-- | The less than operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than the value specified with
-- @LT@.
ddsLT :: Lens' DescribeDataSources (Maybe Text)
ddsLT = lens _ddsLT (\ s a -> s{_ddsLT = a});

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
ddsFilterVariable :: Lens' DescribeDataSources (Maybe DataSourceFilterVariable)
ddsFilterVariable = lens _ddsFilterVariable (\ s a -> s{_ddsFilterVariable = a});

-- | The less than or equal to operator. The @DataSource@ results will have
-- @FilterVariable@ values that are less than or equal to the value
-- specified with @LE@.
ddsLE :: Lens' DescribeDataSources (Maybe Text)
ddsLE = lens _ddsLE (\ s a -> s{_ddsLE = a});

instance AWSPager DescribeDataSources where
        page rq rs
          | stop (rs ^. dNextToken) = Nothing
          | stop (rs ^. dResults) = Nothing
          | otherwise =
            Just $ rq & ddsNextToken .~ rs ^. dNextToken

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
                     <*> (pure s))

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
              ["EQ" .= _ddsEQ, "GE" .= _ddsGE,
               "Prefix" .= _ddsPrefix, "GT" .= _ddsGT,
               "NE" .= _ddsNE, "NextToken" .= _ddsNextToken,
               "SortOrder" .= _ddsSortOrder, "Limit" .= _ddsLimit,
               "LT" .= _ddsLT,
               "FilterVariable" .= _ddsFilterVariable,
               "LE" .= _ddsLE]

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
-- * 'dResults'
--
-- * 'dNextToken'
--
-- * 'dStatus'
data DescribeDataSourcesResponse = DescribeDataSourcesResponse'
    { _dResults   :: !(Maybe [DataSource])
    , _dNextToken :: !(Maybe Text)
    , _dStatus    :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeDataSourcesResponse' smart constructor.
describeDataSourcesResponse :: Status -> DescribeDataSourcesResponse
describeDataSourcesResponse pStatus =
    DescribeDataSourcesResponse'
    { _dResults = Nothing
    , _dNextToken = Nothing
    , _dStatus = pStatus
    }

-- | A list of @DataSource@ that meet the search criteria.
dResults :: Lens' DescribeDataSourcesResponse [DataSource]
dResults = lens _dResults (\ s a -> s{_dResults = a}) . _Default;

-- | An ID of the next page in the paginated results that indicates at least
-- one more page follows.
dNextToken :: Lens' DescribeDataSourcesResponse (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | FIXME: Undocumented member.
dStatus :: Lens' DescribeDataSourcesResponse Status
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
