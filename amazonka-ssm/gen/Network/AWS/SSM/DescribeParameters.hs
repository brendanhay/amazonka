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
-- Module      : Network.AWS.SSM.DescribeParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter.
--
--
-- Request results are returned on a best-effort basis. If you specify @MaxResults@ in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of @MaxResults@ . If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a @NextToken@ . You can specify the @NextToken@ in a subsequent call to get the next set of results.
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeParameters
    (
    -- * Creating a Request
      describeParameters
    , DescribeParameters
    -- * Request Lenses
    , dpParameterFilters
    , dpFilters
    , dpNextToken
    , dpMaxResults

    -- * Destructuring the Response
    , describeParametersResponse
    , DescribeParametersResponse
    -- * Response Lenses
    , dprsNextToken
    , dprsParameters
    , dprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { _dpParameterFilters :: !(Maybe [ParameterStringFilter])
  , _dpFilters          :: !(Maybe [ParametersFilter])
  , _dpNextToken        :: !(Maybe Text)
  , _dpMaxResults       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpParameterFilters' - Filters to limit the request results.
--
-- * 'dpFilters' - One or more filters. Use a filter to return a more specific list of results.
--
-- * 'dpNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dpMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
describeParameters
    :: DescribeParameters
describeParameters =
  DescribeParameters'
    { _dpParameterFilters = Nothing
    , _dpFilters = Nothing
    , _dpNextToken = Nothing
    , _dpMaxResults = Nothing
    }


-- | Filters to limit the request results.
dpParameterFilters :: Lens' DescribeParameters [ParameterStringFilter]
dpParameterFilters = lens _dpParameterFilters (\ s a -> s{_dpParameterFilters = a}) . _Default . _Coerce

-- | One or more filters. Use a filter to return a more specific list of results.
dpFilters :: Lens' DescribeParameters [ParametersFilter]
dpFilters = lens _dpFilters (\ s a -> s{_dpFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dpNextToken :: Lens' DescribeParameters (Maybe Text)
dpNextToken = lens _dpNextToken (\ s a -> s{_dpNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dpMaxResults :: Lens' DescribeParameters (Maybe Natural)
dpMaxResults = lens _dpMaxResults (\ s a -> s{_dpMaxResults = a}) . mapping _Nat

instance AWSPager DescribeParameters where
        page rq rs
          | stop (rs ^. dprsNextToken) = Nothing
          | stop (rs ^. dprsParameters) = Nothing
          | otherwise =
            Just $ rq & dpNextToken .~ rs ^. dprsNextToken

instance AWSRequest DescribeParameters where
        type Rs DescribeParameters =
             DescribeParametersResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeParametersResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Parameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeParameters where

instance NFData DescribeParameters where

instance ToHeaders DescribeParameters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeParameters" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeParameters where
        toJSON DescribeParameters'{..}
          = object
              (catMaybes
                 [("ParameterFilters" .=) <$> _dpParameterFilters,
                  ("Filters" .=) <$> _dpFilters,
                  ("NextToken" .=) <$> _dpNextToken,
                  ("MaxResults" .=) <$> _dpMaxResults])

instance ToPath DescribeParameters where
        toPath = const "/"

instance ToQuery DescribeParameters where
        toQuery = const mempty

-- | /See:/ 'describeParametersResponse' smart constructor.
data DescribeParametersResponse = DescribeParametersResponse'
  { _dprsNextToken      :: !(Maybe Text)
  , _dprsParameters     :: !(Maybe [ParameterMetadata])
  , _dprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dprsParameters' - Parameters returned by the request.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describeParametersResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribeParametersResponse
describeParametersResponse pResponseStatus_ =
  DescribeParametersResponse'
    { _dprsNextToken = Nothing
    , _dprsParameters = Nothing
    , _dprsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dprsNextToken :: Lens' DescribeParametersResponse (Maybe Text)
dprsNextToken = lens _dprsNextToken (\ s a -> s{_dprsNextToken = a})

-- | Parameters returned by the request.
dprsParameters :: Lens' DescribeParametersResponse [ParameterMetadata]
dprsParameters = lens _dprsParameters (\ s a -> s{_dprsParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribeParametersResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DescribeParametersResponse where
