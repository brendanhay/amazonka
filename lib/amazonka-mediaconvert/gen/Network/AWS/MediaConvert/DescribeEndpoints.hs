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
-- Module      : Network.AWS.MediaConvert.DescribeEndpoints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send an request with an empty body to the regional API endpoint to get your account API endpoint.
module Network.AWS.MediaConvert.DescribeEndpoints
    (
    -- * Creating a Request
      describeEndpoints
    , DescribeEndpoints
    -- * Request Lenses
    , deNextToken
    , deMaxResults

    -- * Destructuring the Response
    , describeEndpointsResponse
    , DescribeEndpointsResponse
    -- * Response Lenses
    , dersNextToken
    , dersEndpoints
    , dersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DescribeEndpointsRequest
--
-- /See:/ 'describeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { _deNextToken  :: !(Maybe Text)
  , _deMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deNextToken' - Use this string, provided with the response to a previous request, to request the next batch of endpoints.
--
-- * 'deMaxResults' - Optional. Max number of endpoints, up to twenty, that will be returned at one time.
describeEndpoints
    :: DescribeEndpoints
describeEndpoints =
  DescribeEndpoints' {_deNextToken = Nothing, _deMaxResults = Nothing}


-- | Use this string, provided with the response to a previous request, to request the next batch of endpoints.
deNextToken :: Lens' DescribeEndpoints (Maybe Text)
deNextToken = lens _deNextToken (\ s a -> s{_deNextToken = a})

-- | Optional. Max number of endpoints, up to twenty, that will be returned at one time.
deMaxResults :: Lens' DescribeEndpoints (Maybe Int)
deMaxResults = lens _deMaxResults (\ s a -> s{_deMaxResults = a})

instance AWSRequest DescribeEndpoints where
        type Rs DescribeEndpoints = DescribeEndpointsResponse
        request = postJSON mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEndpointsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "endpoints" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEndpoints where

instance NFData DescribeEndpoints where

instance ToHeaders DescribeEndpoints where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEndpoints where
        toJSON DescribeEndpoints'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _deNextToken,
                  ("maxResults" .=) <$> _deMaxResults])

instance ToPath DescribeEndpoints where
        toPath = const "/2017-08-29/endpoints"

instance ToQuery DescribeEndpoints where
        toQuery = const mempty

-- | /See:/ 'describeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { _dersNextToken      :: !(Maybe Text)
  , _dersEndpoints      :: !(Maybe [Endpoint])
  , _dersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersNextToken' - Use this string to request the next batch of endpoints.
--
-- * 'dersEndpoints' - List of endpoints
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEndpointsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEndpointsResponse
describeEndpointsResponse pResponseStatus_ =
  DescribeEndpointsResponse'
    { _dersNextToken = Nothing
    , _dersEndpoints = Nothing
    , _dersResponseStatus = pResponseStatus_
    }


-- | Use this string to request the next batch of endpoints.
dersNextToken :: Lens' DescribeEndpointsResponse (Maybe Text)
dersNextToken = lens _dersNextToken (\ s a -> s{_dersNextToken = a})

-- | List of endpoints
dersEndpoints :: Lens' DescribeEndpointsResponse [Endpoint]
dersEndpoints = lens _dersEndpoints (\ s a -> s{_dersEndpoints = a}) . _Default . _Coerce

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEndpointsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DescribeEndpointsResponse where
