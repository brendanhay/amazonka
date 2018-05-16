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
-- Module      : Network.AWS.MachineLearning.DescribeDataSources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DataSource@ that match the search criteria in the request.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeDataSources
    (
    -- * Creating a Request
      describeDataSources
    , DescribeDataSources
    -- * Request Lenses
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

    -- * Destructuring the Response
    , describeDataSourcesResponse
    , DescribeDataSourcesResponse
    -- * Response Lenses
    , ddssrsResults
    , ddssrsNextToken
    , ddssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDataSources' smart constructor.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDataSources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsEQ' - The equal to operator. The @DataSource@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- * 'ddsGE' - The greater than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- * 'ddsPrefix' - A string that is found at the beginning of a variable, such as @Name@ or @Id@ . For example, a @DataSource@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @DataSource@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :      * 2014-09     * 2014-09-09     * 2014-09-09-Holiday
--
-- * 'ddsGT' - The greater than operator. The @DataSource@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- * 'ddsNE' - The not equal to operator. The @DataSource@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- * 'ddsNextToken' - The ID of the page in the paginated results.
--
-- * 'ddsSortOrder' - A two-value parameter that determines the sequence of the resulting list of @DataSource@ .     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).    * @dsc@ - Arranges the list in descending order (Z-A, 9-0). Results are sorted by @FilterVariable@ .
--
-- * 'ddsLimit' - The maximum number of @DataSource@ to include in the result.
--
-- * 'ddsLT' - The less than operator. The @DataSource@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- * 'ddsFilterVariable' - Use one of the following variables to filter a list of @DataSource@ :     * @CreatedAt@ - Sets the search criteria to @DataSource@ creation dates.    * @Status@ - Sets the search criteria to @DataSource@ statuses.    * @Name@ - Sets the search criteria to the contents of @DataSource@ ____ @Name@ .    * @DataUri@ - Sets the search criteria to the URI of data files used to create the @DataSource@ . The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.    * @IAMUser@ - Sets the search criteria to the user account that invoked the @DataSource@ creation.
--
-- * 'ddsLE' - The less than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
describeDataSources
    :: DescribeDataSources
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


-- | The equal to operator. The @DataSource@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
ddsEQ :: Lens' DescribeDataSources (Maybe Text)
ddsEQ = lens _ddsEQ (\ s a -> s{_ddsEQ = a})

-- | The greater than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
ddsGE :: Lens' DescribeDataSources (Maybe Text)
ddsGE = lens _ddsGE (\ s a -> s{_ddsGE = a})

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ . For example, a @DataSource@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @DataSource@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :      * 2014-09     * 2014-09-09     * 2014-09-09-Holiday
ddsPrefix :: Lens' DescribeDataSources (Maybe Text)
ddsPrefix = lens _ddsPrefix (\ s a -> s{_ddsPrefix = a})

-- | The greater than operator. The @DataSource@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
ddsGT :: Lens' DescribeDataSources (Maybe Text)
ddsGT = lens _ddsGT (\ s a -> s{_ddsGT = a})

-- | The not equal to operator. The @DataSource@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
ddsNE :: Lens' DescribeDataSources (Maybe Text)
ddsNE = lens _ddsNE (\ s a -> s{_ddsNE = a})

-- | The ID of the page in the paginated results.
ddsNextToken :: Lens' DescribeDataSources (Maybe Text)
ddsNextToken = lens _ddsNextToken (\ s a -> s{_ddsNextToken = a})

-- | A two-value parameter that determines the sequence of the resulting list of @DataSource@ .     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).    * @dsc@ - Arranges the list in descending order (Z-A, 9-0). Results are sorted by @FilterVariable@ .
ddsSortOrder :: Lens' DescribeDataSources (Maybe SortOrder)
ddsSortOrder = lens _ddsSortOrder (\ s a -> s{_ddsSortOrder = a})

-- | The maximum number of @DataSource@ to include in the result.
ddsLimit :: Lens' DescribeDataSources (Maybe Natural)
ddsLimit = lens _ddsLimit (\ s a -> s{_ddsLimit = a}) . mapping _Nat

-- | The less than operator. The @DataSource@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
ddsLT :: Lens' DescribeDataSources (Maybe Text)
ddsLT = lens _ddsLT (\ s a -> s{_ddsLT = a})

-- | Use one of the following variables to filter a list of @DataSource@ :     * @CreatedAt@ - Sets the search criteria to @DataSource@ creation dates.    * @Status@ - Sets the search criteria to @DataSource@ statuses.    * @Name@ - Sets the search criteria to the contents of @DataSource@ ____ @Name@ .    * @DataUri@ - Sets the search criteria to the URI of data files used to create the @DataSource@ . The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.    * @IAMUser@ - Sets the search criteria to the user account that invoked the @DataSource@ creation.
ddsFilterVariable :: Lens' DescribeDataSources (Maybe DataSourceFilterVariable)
ddsFilterVariable = lens _ddsFilterVariable (\ s a -> s{_ddsFilterVariable = a})

-- | The less than or equal to operator. The @DataSource@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
ddsLE :: Lens' DescribeDataSources (Maybe Text)
ddsLE = lens _ddsLE (\ s a -> s{_ddsLE = a})

instance AWSPager DescribeDataSources where
        page rq rs
          | stop (rs ^. ddssrsNextToken) = Nothing
          | stop (rs ^. ddssrsResults) = Nothing
          | otherwise =
            Just $ rq & ddsNextToken .~ rs ^. ddssrsNextToken

instance AWSRequest DescribeDataSources where
        type Rs DescribeDataSources =
             DescribeDataSourcesResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDataSourcesResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDataSources where

instance NFData DescribeDataSources where

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
              (catMaybes
                 [("EQ" .=) <$> _ddsEQ, ("GE" .=) <$> _ddsGE,
                  ("Prefix" .=) <$> _ddsPrefix, ("GT" .=) <$> _ddsGT,
                  ("NE" .=) <$> _ddsNE,
                  ("NextToken" .=) <$> _ddsNextToken,
                  ("SortOrder" .=) <$> _ddsSortOrder,
                  ("Limit" .=) <$> _ddsLimit, ("LT" .=) <$> _ddsLT,
                  ("FilterVariable" .=) <$> _ddsFilterVariable,
                  ("LE" .=) <$> _ddsLE])

instance ToPath DescribeDataSources where
        toPath = const "/"

instance ToQuery DescribeDataSources where
        toQuery = const mempty

-- | Represents the query results from a 'DescribeDataSources' operation. The content is essentially a list of @DataSource@ .
--
--
--
-- /See:/ 'describeDataSourcesResponse' smart constructor.
data DescribeDataSourcesResponse = DescribeDataSourcesResponse'
  { _ddssrsResults        :: !(Maybe [DataSource])
  , _ddssrsNextToken      :: !(Maybe Text)
  , _ddssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDataSourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddssrsResults' - A list of @DataSource@ that meet the search criteria.
--
-- * 'ddssrsNextToken' - An ID of the next page in the paginated results that indicates at least one more page follows.
--
-- * 'ddssrsResponseStatus' - -- | The response status code.
describeDataSourcesResponse
    :: Int -- ^ 'ddssrsResponseStatus'
    -> DescribeDataSourcesResponse
describeDataSourcesResponse pResponseStatus_ =
  DescribeDataSourcesResponse'
    { _ddssrsResults = Nothing
    , _ddssrsNextToken = Nothing
    , _ddssrsResponseStatus = pResponseStatus_
    }


-- | A list of @DataSource@ that meet the search criteria.
ddssrsResults :: Lens' DescribeDataSourcesResponse [DataSource]
ddssrsResults = lens _ddssrsResults (\ s a -> s{_ddssrsResults = a}) . _Default . _Coerce

-- | An ID of the next page in the paginated results that indicates at least one more page follows.
ddssrsNextToken :: Lens' DescribeDataSourcesResponse (Maybe Text)
ddssrsNextToken = lens _ddssrsNextToken (\ s a -> s{_ddssrsNextToken = a})

-- | -- | The response status code.
ddssrsResponseStatus :: Lens' DescribeDataSourcesResponse Int
ddssrsResponseStatus = lens _ddssrsResponseStatus (\ s a -> s{_ddssrsResponseStatus = a})

instance NFData DescribeDataSourcesResponse where
