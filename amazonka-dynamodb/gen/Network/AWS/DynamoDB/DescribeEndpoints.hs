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
-- Module      : Network.AWS.DynamoDB.DescribeEndpoints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the regional endpoint information.
--
--
module Network.AWS.DynamoDB.DescribeEndpoints
    (
    -- * Creating a Request
      describeEndpoints
    , DescribeEndpoints

    -- * Destructuring the Response
    , describeEndpointsResponse
    , DescribeEndpointsResponse
    -- * Response Lenses
    , dersResponseStatus
    , dersEndpoints
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEndpoints' smart constructor.
data DescribeEndpoints =
  DescribeEndpoints'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpoints' with the minimum fields required to make a request.
--
describeEndpoints
    :: DescribeEndpoints
describeEndpoints = DescribeEndpoints'


instance AWSRequest DescribeEndpoints where
        type Rs DescribeEndpoints = DescribeEndpointsResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEndpointsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "Endpoints" .!@ mempty))

instance Hashable DescribeEndpoints where

instance NFData DescribeEndpoints where

instance ToHeaders DescribeEndpoints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DescribeEndpoints" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeEndpoints where
        toJSON = const (Object mempty)

instance ToPath DescribeEndpoints where
        toPath = const "/"

instance ToQuery DescribeEndpoints where
        toQuery = const mempty

-- | /See:/ 'describeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { _dersResponseStatus :: !Int
  , _dersEndpoints      :: ![Endpoint]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersResponseStatus' - -- | The response status code.
--
-- * 'dersEndpoints' - List of endpoints.
describeEndpointsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEndpointsResponse
describeEndpointsResponse pResponseStatus_ =
  DescribeEndpointsResponse'
    {_dersResponseStatus = pResponseStatus_, _dersEndpoints = mempty}


-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEndpointsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

-- | List of endpoints.
dersEndpoints :: Lens' DescribeEndpointsResponse [Endpoint]
dersEndpoints = lens _dersEndpoints (\ s a -> s{_dersEndpoints = a}) . _Coerce

instance NFData DescribeEndpointsResponse where
