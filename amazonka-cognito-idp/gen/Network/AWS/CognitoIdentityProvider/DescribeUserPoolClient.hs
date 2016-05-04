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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Client method for returning the configuration information and metadata
-- of the specified user pool client.
module Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
    (
    -- * Creating a Request
      describeUserPoolClient
    , DescribeUserPoolClient
    -- * Request Lenses
    , desUserPoolId
    , desClientId

    -- * Destructuring the Response
    , describeUserPoolClientResponse
    , DescribeUserPoolClientResponse
    -- * Response Lenses
    , dupcrsUserPoolClient
    , dupcrsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to describe a user pool client.
--
-- /See:/ 'describeUserPoolClient' smart constructor.
data DescribeUserPoolClient = DescribeUserPoolClient'
    { _desUserPoolId :: !Text
    , _desClientId   :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeUserPoolClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desUserPoolId'
--
-- * 'desClientId'
describeUserPoolClient
    :: Text -- ^ 'desUserPoolId'
    -> Text -- ^ 'desClientId'
    -> DescribeUserPoolClient
describeUserPoolClient pUserPoolId_ pClientId_ =
    DescribeUserPoolClient'
    { _desUserPoolId = pUserPoolId_
    , _desClientId = _Sensitive # pClientId_
    }

-- | The user pool ID for the user pool you want to describe.
desUserPoolId :: Lens' DescribeUserPoolClient Text
desUserPoolId = lens _desUserPoolId (\ s a -> s{_desUserPoolId = a});

-- | The ID of the client associated with the user pool.
desClientId :: Lens' DescribeUserPoolClient Text
desClientId = lens _desClientId (\ s a -> s{_desClientId = a}) . _Sensitive;

instance AWSRequest DescribeUserPoolClient where
        type Rs DescribeUserPoolClient =
             DescribeUserPoolClientResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserPoolClientResponse' <$>
                   (x .?> "UserPoolClient") <*> (pure (fromEnum s)))

instance Hashable DescribeUserPoolClient

instance NFData DescribeUserPoolClient

instance ToHeaders DescribeUserPoolClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DescribeUserPoolClient"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUserPoolClient where
        toJSON DescribeUserPoolClient'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _desUserPoolId),
                  Just ("ClientId" .= _desClientId)])

instance ToPath DescribeUserPoolClient where
        toPath = const "/"

instance ToQuery DescribeUserPoolClient where
        toQuery = const mempty

-- | Represents the response from the server from a request to describe the
-- user pool client.
--
-- /See:/ 'describeUserPoolClientResponse' smart constructor.
data DescribeUserPoolClientResponse = DescribeUserPoolClientResponse'
    { _dupcrsUserPoolClient :: !(Maybe UserPoolClientType)
    , _dupcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeUserPoolClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupcrsUserPoolClient'
--
-- * 'dupcrsResponseStatus'
describeUserPoolClientResponse
    :: Int -- ^ 'dupcrsResponseStatus'
    -> DescribeUserPoolClientResponse
describeUserPoolClientResponse pResponseStatus_ =
    DescribeUserPoolClientResponse'
    { _dupcrsUserPoolClient = Nothing
    , _dupcrsResponseStatus = pResponseStatus_
    }

-- | The user pool client from a server response to describe the user pool
-- client.
dupcrsUserPoolClient :: Lens' DescribeUserPoolClientResponse (Maybe UserPoolClientType)
dupcrsUserPoolClient = lens _dupcrsUserPoolClient (\ s a -> s{_dupcrsUserPoolClient = a});

-- | The response status code.
dupcrsResponseStatus :: Lens' DescribeUserPoolClientResponse Int
dupcrsResponseStatus = lens _dupcrsResponseStatus (\ s a -> s{_dupcrsResponseStatus = a});

instance NFData DescribeUserPoolClientResponse
