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
-- Module      : Network.AWS.IoT.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a custom authorization behavior by invoking a specified custom authorizer. Use this to test and debug the custom authorization behavior of devices that connect to the AWS IoT device gateway.
module Network.AWS.IoT.TestInvokeAuthorizer
  ( -- * Creating a Request
    testInvokeAuthorizer,
    TestInvokeAuthorizer,

    -- * Request Lenses
    tiaToken,
    tiaTlsContext,
    tiaTokenSignature,
    tiaHttpContext,
    tiaMqttContext,
    tiaAuthorizerName,

    -- * Destructuring the Response
    testInvokeAuthorizerResponse,
    TestInvokeAuthorizerResponse,

    -- * Response Lenses
    tiarsPolicyDocuments,
    tiarsPrincipalId,
    tiarsDisconnectAfterInSeconds,
    tiarsIsAuthenticated,
    tiarsRefreshAfterInSeconds,
    tiarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'testInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { _tiaToken ::
      !(Maybe Text),
    _tiaTlsContext :: !(Maybe TLSContext),
    _tiaTokenSignature :: !(Maybe Text),
    _tiaHttpContext :: !(Maybe HTTPContext),
    _tiaMqttContext :: !(Maybe MqttContext),
    _tiaAuthorizerName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestInvokeAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiaToken' - The token returned by your custom authentication service.
--
-- * 'tiaTlsContext' - Specifies a test TLS authorization request.
--
-- * 'tiaTokenSignature' - The signature made with the token and your custom authentication service's private key. This value must be Base-64-encoded.
--
-- * 'tiaHttpContext' - Specifies a test HTTP authorization request.
--
-- * 'tiaMqttContext' - Specifies a test MQTT authorization request.
--
-- * 'tiaAuthorizerName' - The custom authorizer name.
testInvokeAuthorizer ::
  -- | 'tiaAuthorizerName'
  Text ->
  TestInvokeAuthorizer
testInvokeAuthorizer pAuthorizerName_ =
  TestInvokeAuthorizer'
    { _tiaToken = Nothing,
      _tiaTlsContext = Nothing,
      _tiaTokenSignature = Nothing,
      _tiaHttpContext = Nothing,
      _tiaMqttContext = Nothing,
      _tiaAuthorizerName = pAuthorizerName_
    }

-- | The token returned by your custom authentication service.
tiaToken :: Lens' TestInvokeAuthorizer (Maybe Text)
tiaToken = lens _tiaToken (\s a -> s {_tiaToken = a})

-- | Specifies a test TLS authorization request.
tiaTlsContext :: Lens' TestInvokeAuthorizer (Maybe TLSContext)
tiaTlsContext = lens _tiaTlsContext (\s a -> s {_tiaTlsContext = a})

-- | The signature made with the token and your custom authentication service's private key. This value must be Base-64-encoded.
tiaTokenSignature :: Lens' TestInvokeAuthorizer (Maybe Text)
tiaTokenSignature = lens _tiaTokenSignature (\s a -> s {_tiaTokenSignature = a})

-- | Specifies a test HTTP authorization request.
tiaHttpContext :: Lens' TestInvokeAuthorizer (Maybe HTTPContext)
tiaHttpContext = lens _tiaHttpContext (\s a -> s {_tiaHttpContext = a})

-- | Specifies a test MQTT authorization request.
tiaMqttContext :: Lens' TestInvokeAuthorizer (Maybe MqttContext)
tiaMqttContext = lens _tiaMqttContext (\s a -> s {_tiaMqttContext = a})

-- | The custom authorizer name.
tiaAuthorizerName :: Lens' TestInvokeAuthorizer Text
tiaAuthorizerName = lens _tiaAuthorizerName (\s a -> s {_tiaAuthorizerName = a})

instance AWSRequest TestInvokeAuthorizer where
  type Rs TestInvokeAuthorizer = TestInvokeAuthorizerResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          TestInvokeAuthorizerResponse'
            <$> (x .?> "policyDocuments" .!@ mempty)
            <*> (x .?> "principalId")
            <*> (x .?> "disconnectAfterInSeconds")
            <*> (x .?> "isAuthenticated")
            <*> (x .?> "refreshAfterInSeconds")
            <*> (pure (fromEnum s))
      )

instance Hashable TestInvokeAuthorizer

instance NFData TestInvokeAuthorizer

instance ToHeaders TestInvokeAuthorizer where
  toHeaders = const mempty

instance ToJSON TestInvokeAuthorizer where
  toJSON TestInvokeAuthorizer' {..} =
    object
      ( catMaybes
          [ ("token" .=) <$> _tiaToken,
            ("tlsContext" .=) <$> _tiaTlsContext,
            ("tokenSignature" .=) <$> _tiaTokenSignature,
            ("httpContext" .=) <$> _tiaHttpContext,
            ("mqttContext" .=) <$> _tiaMqttContext
          ]
      )

instance ToPath TestInvokeAuthorizer where
  toPath TestInvokeAuthorizer' {..} =
    mconcat ["/authorizer/", toBS _tiaAuthorizerName, "/test"]

instance ToQuery TestInvokeAuthorizer where
  toQuery = const mempty

-- | /See:/ 'testInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { _tiarsPolicyDocuments ::
      !(Maybe [Text]),
    _tiarsPrincipalId ::
      !(Maybe Text),
    _tiarsDisconnectAfterInSeconds ::
      !(Maybe Int),
    _tiarsIsAuthenticated ::
      !(Maybe Bool),
    _tiarsRefreshAfterInSeconds ::
      !(Maybe Int),
    _tiarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestInvokeAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiarsPolicyDocuments' - IAM policy documents.
--
-- * 'tiarsPrincipalId' - The principal ID.
--
-- * 'tiarsDisconnectAfterInSeconds' - The number of seconds after which the connection is terminated.
--
-- * 'tiarsIsAuthenticated' - True if the token is authenticated, otherwise false.
--
-- * 'tiarsRefreshAfterInSeconds' - The number of seconds after which the temporary credentials are refreshed.
--
-- * 'tiarsResponseStatus' - -- | The response status code.
testInvokeAuthorizerResponse ::
  -- | 'tiarsResponseStatus'
  Int ->
  TestInvokeAuthorizerResponse
testInvokeAuthorizerResponse pResponseStatus_ =
  TestInvokeAuthorizerResponse'
    { _tiarsPolicyDocuments = Nothing,
      _tiarsPrincipalId = Nothing,
      _tiarsDisconnectAfterInSeconds = Nothing,
      _tiarsIsAuthenticated = Nothing,
      _tiarsRefreshAfterInSeconds = Nothing,
      _tiarsResponseStatus = pResponseStatus_
    }

-- | IAM policy documents.
tiarsPolicyDocuments :: Lens' TestInvokeAuthorizerResponse [Text]
tiarsPolicyDocuments = lens _tiarsPolicyDocuments (\s a -> s {_tiarsPolicyDocuments = a}) . _Default . _Coerce

-- | The principal ID.
tiarsPrincipalId :: Lens' TestInvokeAuthorizerResponse (Maybe Text)
tiarsPrincipalId = lens _tiarsPrincipalId (\s a -> s {_tiarsPrincipalId = a})

-- | The number of seconds after which the connection is terminated.
tiarsDisconnectAfterInSeconds :: Lens' TestInvokeAuthorizerResponse (Maybe Int)
tiarsDisconnectAfterInSeconds = lens _tiarsDisconnectAfterInSeconds (\s a -> s {_tiarsDisconnectAfterInSeconds = a})

-- | True if the token is authenticated, otherwise false.
tiarsIsAuthenticated :: Lens' TestInvokeAuthorizerResponse (Maybe Bool)
tiarsIsAuthenticated = lens _tiarsIsAuthenticated (\s a -> s {_tiarsIsAuthenticated = a})

-- | The number of seconds after which the temporary credentials are refreshed.
tiarsRefreshAfterInSeconds :: Lens' TestInvokeAuthorizerResponse (Maybe Int)
tiarsRefreshAfterInSeconds = lens _tiarsRefreshAfterInSeconds (\s a -> s {_tiarsRefreshAfterInSeconds = a})

-- | -- | The response status code.
tiarsResponseStatus :: Lens' TestInvokeAuthorizerResponse Int
tiarsResponseStatus = lens _tiarsResponseStatus (\s a -> s {_tiarsResponseStatus = a})

instance NFData TestInvokeAuthorizerResponse
