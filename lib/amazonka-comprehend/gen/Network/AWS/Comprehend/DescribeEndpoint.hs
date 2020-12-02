{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a specific endpoint. Use this operation to get the status of an endpoint.
module Network.AWS.Comprehend.DescribeEndpoint
  ( -- * Creating a Request
    describeEndpoint,
    DescribeEndpoint,

    -- * Request Lenses
    desEndpointARN,

    -- * Destructuring the Response
    describeEndpointResponse,
    DescribeEndpointResponse,

    -- * Response Lenses
    dersEndpointProperties,
    dersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { _desEndpointARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desEndpointARN' - The Amazon Resource Number (ARN) of the endpoint being described.
describeEndpoint ::
  -- | 'desEndpointARN'
  Text ->
  DescribeEndpoint
describeEndpoint pEndpointARN_ =
  DescribeEndpoint' {_desEndpointARN = pEndpointARN_}

-- | The Amazon Resource Number (ARN) of the endpoint being described.
desEndpointARN :: Lens' DescribeEndpoint Text
desEndpointARN = lens _desEndpointARN (\s a -> s {_desEndpointARN = a})

instance AWSRequest DescribeEndpoint where
  type Rs DescribeEndpoint = DescribeEndpointResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            <$> (x .?> "EndpointProperties") <*> (pure (fromEnum s))
      )

instance Hashable DescribeEndpoint

instance NFData DescribeEndpoint

instance ToHeaders DescribeEndpoint where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DescribeEndpoint" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    object (catMaybes [Just ("EndpointArn" .= _desEndpointARN)])

instance ToPath DescribeEndpoint where
  toPath = const "/"

instance ToQuery DescribeEndpoint where
  toQuery = const mempty

-- | /See:/ 'describeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { _dersEndpointProperties ::
      !(Maybe EndpointProperties),
    _dersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersEndpointProperties' - Describes information associated with the specific endpoint.
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEndpointResponse ::
  -- | 'dersResponseStatus'
  Int ->
  DescribeEndpointResponse
describeEndpointResponse pResponseStatus_ =
  DescribeEndpointResponse'
    { _dersEndpointProperties = Nothing,
      _dersResponseStatus = pResponseStatus_
    }

-- | Describes information associated with the specific endpoint.
dersEndpointProperties :: Lens' DescribeEndpointResponse (Maybe EndpointProperties)
dersEndpointProperties = lens _dersEndpointProperties (\s a -> s {_dersEndpointProperties = a})

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEndpointResponse Int
dersResponseStatus = lens _dersResponseStatus (\s a -> s {_dersResponseStatus = a})

instance NFData DescribeEndpointResponse
