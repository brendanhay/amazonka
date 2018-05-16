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
-- Module      : Network.AWS.MachineLearning.DescribeMLModels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @MLModel@ that match the search criteria in the request.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MachineLearning.DescribeMLModels
    (
    -- * Creating a Request
      describeMLModels
    , DescribeMLModels
    -- * Request Lenses
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

    -- * Destructuring the Response
    , describeMLModelsResponse
    , DescribeMLModelsResponse
    -- * Response Lenses
    , dmlmsrsResults
    , dmlmsrsNextToken
    , dmlmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeMLModels' smart constructor.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMLModels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlmEQ' - The equal to operator. The @MLModel@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
--
-- * 'dmlmGE' - The greater than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
--
-- * 'dmlmPrefix' - A string that is found at the beginning of a variable, such as @Name@ or @Id@ . For example, an @MLModel@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @MLModel@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :      * 2014-09     * 2014-09-09     * 2014-09-09-Holiday
--
-- * 'dmlmGT' - The greater than operator. The @MLModel@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
--
-- * 'dmlmNE' - The not equal to operator. The @MLModel@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
--
-- * 'dmlmNextToken' - The ID of the page in the paginated results.
--
-- * 'dmlmSortOrder' - A two-value parameter that determines the sequence of the resulting list of @MLModel@ .     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).    * @dsc@ - Arranges the list in descending order (Z-A, 9-0). Results are sorted by @FilterVariable@ .
--
-- * 'dmlmLimit' - The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
--
-- * 'dmlmLT' - The less than operator. The @MLModel@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
--
-- * 'dmlmFilterVariable' - Use one of the following variables to filter a list of @MLModel@ :     * @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.    * @Status@ - Sets the search criteria to @MLModel@ status.    * @Name@ - Sets the search criteria to the contents of @MLModel@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked the @MLModel@ creation.    * @TrainingDataSourceId@ - Sets the search criteria to the @DataSource@ used to train one or more @MLModel@ .    * @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@ real-time endpoint status.    * @MLModelType@ - Sets the search criteria to @MLModel@ type: binary, regression, or multi-class.    * @Algorithm@ - Sets the search criteria to the algorithm that the @MLModel@ uses.    * @TrainingDataURI@ - Sets the search criteria to the data file(s) used in training a @MLModel@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
-- * 'dmlmLE' - The less than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
describeMLModels
    :: DescribeMLModels
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


-- | The equal to operator. The @MLModel@ results will have @FilterVariable@ values that exactly match the value specified with @EQ@ .
dmlmEQ :: Lens' DescribeMLModels (Maybe Text)
dmlmEQ = lens _dmlmEQ (\ s a -> s{_dmlmEQ = a})

-- | The greater than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are greater than or equal to the value specified with @GE@ .
dmlmGE :: Lens' DescribeMLModels (Maybe Text)
dmlmGE = lens _dmlmGE (\ s a -> s{_dmlmGE = a})

-- | A string that is found at the beginning of a variable, such as @Name@ or @Id@ . For example, an @MLModel@ could have the @Name@ @2014-09-09-HolidayGiftMailer@ . To search for this @MLModel@ , select @Name@ for the @FilterVariable@ and any of the following strings for the @Prefix@ :      * 2014-09     * 2014-09-09     * 2014-09-09-Holiday
dmlmPrefix :: Lens' DescribeMLModels (Maybe Text)
dmlmPrefix = lens _dmlmPrefix (\ s a -> s{_dmlmPrefix = a})

-- | The greater than operator. The @MLModel@ results will have @FilterVariable@ values that are greater than the value specified with @GT@ .
dmlmGT :: Lens' DescribeMLModels (Maybe Text)
dmlmGT = lens _dmlmGT (\ s a -> s{_dmlmGT = a})

-- | The not equal to operator. The @MLModel@ results will have @FilterVariable@ values not equal to the value specified with @NE@ .
dmlmNE :: Lens' DescribeMLModels (Maybe Text)
dmlmNE = lens _dmlmNE (\ s a -> s{_dmlmNE = a})

-- | The ID of the page in the paginated results.
dmlmNextToken :: Lens' DescribeMLModels (Maybe Text)
dmlmNextToken = lens _dmlmNextToken (\ s a -> s{_dmlmNextToken = a})

-- | A two-value parameter that determines the sequence of the resulting list of @MLModel@ .     * @asc@ - Arranges the list in ascending order (A-Z, 0-9).    * @dsc@ - Arranges the list in descending order (Z-A, 9-0). Results are sorted by @FilterVariable@ .
dmlmSortOrder :: Lens' DescribeMLModels (Maybe SortOrder)
dmlmSortOrder = lens _dmlmSortOrder (\ s a -> s{_dmlmSortOrder = a})

-- | The number of pages of information to include in the result. The range of acceptable values is @1@ through @100@ . The default value is @100@ .
dmlmLimit :: Lens' DescribeMLModels (Maybe Natural)
dmlmLimit = lens _dmlmLimit (\ s a -> s{_dmlmLimit = a}) . mapping _Nat

-- | The less than operator. The @MLModel@ results will have @FilterVariable@ values that are less than the value specified with @LT@ .
dmlmLT :: Lens' DescribeMLModels (Maybe Text)
dmlmLT = lens _dmlmLT (\ s a -> s{_dmlmLT = a})

-- | Use one of the following variables to filter a list of @MLModel@ :     * @CreatedAt@ - Sets the search criteria to @MLModel@ creation date.    * @Status@ - Sets the search criteria to @MLModel@ status.    * @Name@ - Sets the search criteria to the contents of @MLModel@ ____ @Name@ .    * @IAMUser@ - Sets the search criteria to the user account that invoked the @MLModel@ creation.    * @TrainingDataSourceId@ - Sets the search criteria to the @DataSource@ used to train one or more @MLModel@ .    * @RealtimeEndpointStatus@ - Sets the search criteria to the @MLModel@ real-time endpoint status.    * @MLModelType@ - Sets the search criteria to @MLModel@ type: binary, regression, or multi-class.    * @Algorithm@ - Sets the search criteria to the algorithm that the @MLModel@ uses.    * @TrainingDataURI@ - Sets the search criteria to the data file(s) used in training a @MLModel@ . The URL can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
dmlmFilterVariable :: Lens' DescribeMLModels (Maybe MLModelFilterVariable)
dmlmFilterVariable = lens _dmlmFilterVariable (\ s a -> s{_dmlmFilterVariable = a})

-- | The less than or equal to operator. The @MLModel@ results will have @FilterVariable@ values that are less than or equal to the value specified with @LE@ .
dmlmLE :: Lens' DescribeMLModels (Maybe Text)
dmlmLE = lens _dmlmLE (\ s a -> s{_dmlmLE = a})

instance AWSPager DescribeMLModels where
        page rq rs
          | stop (rs ^. dmlmsrsNextToken) = Nothing
          | stop (rs ^. dmlmsrsResults) = Nothing
          | otherwise =
            Just $ rq & dmlmNextToken .~ rs ^. dmlmsrsNextToken

instance AWSRequest DescribeMLModels where
        type Rs DescribeMLModels = DescribeMLModelsResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMLModelsResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMLModels where

instance NFData DescribeMLModels where

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
              (catMaybes
                 [("EQ" .=) <$> _dmlmEQ, ("GE" .=) <$> _dmlmGE,
                  ("Prefix" .=) <$> _dmlmPrefix, ("GT" .=) <$> _dmlmGT,
                  ("NE" .=) <$> _dmlmNE,
                  ("NextToken" .=) <$> _dmlmNextToken,
                  ("SortOrder" .=) <$> _dmlmSortOrder,
                  ("Limit" .=) <$> _dmlmLimit, ("LT" .=) <$> _dmlmLT,
                  ("FilterVariable" .=) <$> _dmlmFilterVariable,
                  ("LE" .=) <$> _dmlmLE])

instance ToPath DescribeMLModels where
        toPath = const "/"

instance ToQuery DescribeMLModels where
        toQuery = const mempty

-- | Represents the output of a @DescribeMLModels@ operation. The content is essentially a list of @MLModel@ .
--
--
--
-- /See:/ 'describeMLModelsResponse' smart constructor.
data DescribeMLModelsResponse = DescribeMLModelsResponse'
  { _dmlmsrsResults        :: !(Maybe [MLModel])
  , _dmlmsrsNextToken      :: !(Maybe Text)
  , _dmlmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMLModelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlmsrsResults' - A list of @MLModel@ that meet the search criteria.
--
-- * 'dmlmsrsNextToken' - The ID of the next page in the paginated results that indicates at least one more page follows.
--
-- * 'dmlmsrsResponseStatus' - -- | The response status code.
describeMLModelsResponse
    :: Int -- ^ 'dmlmsrsResponseStatus'
    -> DescribeMLModelsResponse
describeMLModelsResponse pResponseStatus_ =
  DescribeMLModelsResponse'
    { _dmlmsrsResults = Nothing
    , _dmlmsrsNextToken = Nothing
    , _dmlmsrsResponseStatus = pResponseStatus_
    }


-- | A list of @MLModel@ that meet the search criteria.
dmlmsrsResults :: Lens' DescribeMLModelsResponse [MLModel]
dmlmsrsResults = lens _dmlmsrsResults (\ s a -> s{_dmlmsrsResults = a}) . _Default . _Coerce

-- | The ID of the next page in the paginated results that indicates at least one more page follows.
dmlmsrsNextToken :: Lens' DescribeMLModelsResponse (Maybe Text)
dmlmsrsNextToken = lens _dmlmsrsNextToken (\ s a -> s{_dmlmsrsNextToken = a})

-- | -- | The response status code.
dmlmsrsResponseStatus :: Lens' DescribeMLModelsResponse Int
dmlmsrsResponseStatus = lens _dmlmsrsResponseStatus (\ s a -> s{_dmlmsrsResponseStatus = a})

instance NFData DescribeMLModelsResponse where
