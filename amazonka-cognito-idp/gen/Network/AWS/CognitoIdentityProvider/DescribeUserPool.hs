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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the configuration information and metadata of the specified user pool.
--
--
module Network.AWS.CognitoIdentityProvider.DescribeUserPool
    (
    -- * Creating a Request
      describeUserPool
    , DescribeUserPool
    -- * Request Lenses
    , dUserPoolId

    -- * Destructuring the Response
    , describeUserPoolResponse
    , DescribeUserPoolResponse
    -- * Response Lenses
    , duprsUserPool
    , duprsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to describe the user pool.
--
--
--
-- /See:/ 'describeUserPool' smart constructor.
newtype DescribeUserPool = DescribeUserPool'
  { _dUserPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dUserPoolId' - The user pool ID for the user pool you want to describe.
describeUserPool
    :: Text -- ^ 'dUserPoolId'
    -> DescribeUserPool
describeUserPool pUserPoolId_ = DescribeUserPool' {_dUserPoolId = pUserPoolId_}


-- | The user pool ID for the user pool you want to describe.
dUserPoolId :: Lens' DescribeUserPool Text
dUserPoolId = lens _dUserPoolId (\ s a -> s{_dUserPoolId = a})

instance AWSRequest DescribeUserPool where
        type Rs DescribeUserPool = DescribeUserPoolResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserPoolResponse' <$>
                   (x .?> "UserPool") <*> (pure (fromEnum s)))

instance Hashable DescribeUserPool where

instance NFData DescribeUserPool where

instance ToHeaders DescribeUserPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DescribeUserPool"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUserPool where
        toJSON DescribeUserPool'{..}
          = object
              (catMaybes [Just ("UserPoolId" .= _dUserPoolId)])

instance ToPath DescribeUserPool where
        toPath = const "/"

instance ToQuery DescribeUserPool where
        toQuery = const mempty

-- | Represents the response to describe the user pool.
--
--
--
-- /See:/ 'describeUserPoolResponse' smart constructor.
data DescribeUserPoolResponse = DescribeUserPoolResponse'
  { _duprsUserPool       :: !(Maybe UserPoolType)
  , _duprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserPoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duprsUserPool' - The container of metadata returned by the server to describe the pool.
--
-- * 'duprsResponseStatus' - -- | The response status code.
describeUserPoolResponse
    :: Int -- ^ 'duprsResponseStatus'
    -> DescribeUserPoolResponse
describeUserPoolResponse pResponseStatus_ =
  DescribeUserPoolResponse'
    {_duprsUserPool = Nothing, _duprsResponseStatus = pResponseStatus_}


-- | The container of metadata returned by the server to describe the pool.
duprsUserPool :: Lens' DescribeUserPoolResponse (Maybe UserPoolType)
duprsUserPool = lens _duprsUserPool (\ s a -> s{_duprsUserPool = a})

-- | -- | The response status code.
duprsResponseStatus :: Lens' DescribeUserPoolResponse Int
duprsResponseStatus = lens _duprsResponseStatus (\ s a -> s{_duprsResponseStatus = a})

instance NFData DescribeUserPoolResponse where
