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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeResourceServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a resource server.
--
--
module Network.AWS.CognitoIdentityProvider.DescribeResourceServer
    (
    -- * Creating a Request
      describeResourceServer
    , DescribeResourceServer
    -- * Request Lenses
    , desUserPoolId
    , desIdentifier

    -- * Destructuring the Response
    , describeResourceServerResponse
    , DescribeResourceServerResponse
    -- * Response Lenses
    , drsrsResponseStatus
    , drsrsResourceServer
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeResourceServer' smart constructor.
data DescribeResourceServer = DescribeResourceServer'
  { _desUserPoolId :: !Text
  , _desIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourceServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desUserPoolId' - The user pool ID for the user pool that hosts the resource server.
--
-- * 'desIdentifier' - The identifier for the resource server
describeResourceServer
    :: Text -- ^ 'desUserPoolId'
    -> Text -- ^ 'desIdentifier'
    -> DescribeResourceServer
describeResourceServer pUserPoolId_ pIdentifier_ =
  DescribeResourceServer'
    {_desUserPoolId = pUserPoolId_, _desIdentifier = pIdentifier_}


-- | The user pool ID for the user pool that hosts the resource server.
desUserPoolId :: Lens' DescribeResourceServer Text
desUserPoolId = lens _desUserPoolId (\ s a -> s{_desUserPoolId = a})

-- | The identifier for the resource server
desIdentifier :: Lens' DescribeResourceServer Text
desIdentifier = lens _desIdentifier (\ s a -> s{_desIdentifier = a})

instance AWSRequest DescribeResourceServer where
        type Rs DescribeResourceServer =
             DescribeResourceServerResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 DescribeResourceServerResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ResourceServer"))

instance Hashable DescribeResourceServer where

instance NFData DescribeResourceServer where

instance ToHeaders DescribeResourceServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DescribeResourceServer"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeResourceServer where
        toJSON DescribeResourceServer'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _desUserPoolId),
                  Just ("Identifier" .= _desIdentifier)])

instance ToPath DescribeResourceServer where
        toPath = const "/"

instance ToQuery DescribeResourceServer where
        toQuery = const mempty

-- | /See:/ 'describeResourceServerResponse' smart constructor.
data DescribeResourceServerResponse = DescribeResourceServerResponse'
  { _drsrsResponseStatus :: !Int
  , _drsrsResourceServer :: !ResourceServerType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourceServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsrsResponseStatus' - -- | The response status code.
--
-- * 'drsrsResourceServer' - The resource server.
describeResourceServerResponse
    :: Int -- ^ 'drsrsResponseStatus'
    -> ResourceServerType -- ^ 'drsrsResourceServer'
    -> DescribeResourceServerResponse
describeResourceServerResponse pResponseStatus_ pResourceServer_ =
  DescribeResourceServerResponse'
    { _drsrsResponseStatus = pResponseStatus_
    , _drsrsResourceServer = pResourceServer_
    }


-- | -- | The response status code.
drsrsResponseStatus :: Lens' DescribeResourceServerResponse Int
drsrsResponseStatus = lens _drsrsResponseStatus (\ s a -> s{_drsrsResponseStatus = a})

-- | The resource server.
drsrsResourceServer :: Lens' DescribeResourceServerResponse ResourceServerType
drsrsResourceServer = lens _drsrsResourceServer (\ s a -> s{_drsrsResourceServer = a})

instance NFData DescribeResourceServerResponse where
