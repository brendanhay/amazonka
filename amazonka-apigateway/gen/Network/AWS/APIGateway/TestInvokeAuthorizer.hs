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
-- Module      : Network.AWS.APIGateway.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of an 'Authorizer' in your 'RestApi' with headers, parameters, and an incoming request body.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html Enable custom authorizers>
module Network.AWS.APIGateway.TestInvokeAuthorizer
    (
    -- * Creating a Request
      testInvokeAuthorizer
    , TestInvokeAuthorizer
    -- * Request Lenses
    , tiaPathWithQueryString
    , tiaBody
    , tiaAdditionalContext
    , tiaStageVariables
    , tiaHeaders
    , tiaRestAPIId
    , tiaAuthorizerId

    -- * Destructuring the Response
    , testInvokeAuthorizerResponse
    , TestInvokeAuthorizerResponse
    -- * Response Lenses
    , tiarsLog
    , tiarsPrincipalId
    , tiarsLatency
    , tiarsAuthorization
    , tiarsClaims
    , tiarsClientStatus
    , tiarsPolicy
    , tiarsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Make a request to simulate the execution of an 'Authorizer' .
--
--
--
-- /See:/ 'testInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { _tiaPathWithQueryString :: !(Maybe Text)
  , _tiaBody                :: !(Maybe Text)
  , _tiaAdditionalContext   :: !(Maybe (Map Text Text))
  , _tiaStageVariables      :: !(Maybe (Map Text Text))
  , _tiaHeaders             :: !(Maybe (Map Text Text))
  , _tiaRestAPIId           :: !Text
  , _tiaAuthorizerId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestInvokeAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiaPathWithQueryString' - [Optional] The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
--
-- * 'tiaBody' - [Optional] The simulated request body of an incoming invocation request.
--
-- * 'tiaAdditionalContext' - [Optional] A key-value map of additional context variables.
--
-- * 'tiaStageVariables' - A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
--
-- * 'tiaHeaders' - [Required] A key-value map of headers to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, should be specified.
--
-- * 'tiaRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'tiaAuthorizerId' - [Required] Specifies a test invoke authorizer request's 'Authorizer' ID.
testInvokeAuthorizer
    :: Text -- ^ 'tiaRestAPIId'
    -> Text -- ^ 'tiaAuthorizerId'
    -> TestInvokeAuthorizer
testInvokeAuthorizer pRestAPIId_ pAuthorizerId_ =
  TestInvokeAuthorizer'
    { _tiaPathWithQueryString = Nothing
    , _tiaBody = Nothing
    , _tiaAdditionalContext = Nothing
    , _tiaStageVariables = Nothing
    , _tiaHeaders = Nothing
    , _tiaRestAPIId = pRestAPIId_
    , _tiaAuthorizerId = pAuthorizerId_
    }


-- | [Optional] The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
tiaPathWithQueryString :: Lens' TestInvokeAuthorizer (Maybe Text)
tiaPathWithQueryString = lens _tiaPathWithQueryString (\ s a -> s{_tiaPathWithQueryString = a})

-- | [Optional] The simulated request body of an incoming invocation request.
tiaBody :: Lens' TestInvokeAuthorizer (Maybe Text)
tiaBody = lens _tiaBody (\ s a -> s{_tiaBody = a})

-- | [Optional] A key-value map of additional context variables.
tiaAdditionalContext :: Lens' TestInvokeAuthorizer (HashMap Text Text)
tiaAdditionalContext = lens _tiaAdditionalContext (\ s a -> s{_tiaAdditionalContext = a}) . _Default . _Map

-- | A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
tiaStageVariables :: Lens' TestInvokeAuthorizer (HashMap Text Text)
tiaStageVariables = lens _tiaStageVariables (\ s a -> s{_tiaStageVariables = a}) . _Default . _Map

-- | [Required] A key-value map of headers to simulate an incoming invocation request. This is where the incoming authorization token, or identity source, should be specified.
tiaHeaders :: Lens' TestInvokeAuthorizer (HashMap Text Text)
tiaHeaders = lens _tiaHeaders (\ s a -> s{_tiaHeaders = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
tiaRestAPIId :: Lens' TestInvokeAuthorizer Text
tiaRestAPIId = lens _tiaRestAPIId (\ s a -> s{_tiaRestAPIId = a})

-- | [Required] Specifies a test invoke authorizer request's 'Authorizer' ID.
tiaAuthorizerId :: Lens' TestInvokeAuthorizer Text
tiaAuthorizerId = lens _tiaAuthorizerId (\ s a -> s{_tiaAuthorizerId = a})

instance AWSRequest TestInvokeAuthorizer where
        type Rs TestInvokeAuthorizer =
             TestInvokeAuthorizerResponse
        request = postJSON apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 TestInvokeAuthorizerResponse' <$>
                   (x .?> "log") <*> (x .?> "principalId") <*>
                     (x .?> "latency")
                     <*> (x .?> "authorization" .!@ mempty)
                     <*> (x .?> "claims" .!@ mempty)
                     <*> (x .?> "clientStatus")
                     <*> (x .?> "policy")
                     <*> (pure (fromEnum s)))

instance Hashable TestInvokeAuthorizer where

instance NFData TestInvokeAuthorizer where

instance ToHeaders TestInvokeAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON TestInvokeAuthorizer where
        toJSON TestInvokeAuthorizer'{..}
          = object
              (catMaybes
                 [("pathWithQueryString" .=) <$>
                    _tiaPathWithQueryString,
                  ("body" .=) <$> _tiaBody,
                  ("additionalContext" .=) <$> _tiaAdditionalContext,
                  ("stageVariables" .=) <$> _tiaStageVariables,
                  ("headers" .=) <$> _tiaHeaders])

instance ToPath TestInvokeAuthorizer where
        toPath TestInvokeAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _tiaRestAPIId, "/authorizers/",
               toBS _tiaAuthorizerId]

instance ToQuery TestInvokeAuthorizer where
        toQuery = const mempty

-- | Represents the response of the test invoke request for a custom 'Authorizer'
--
--
--
-- /See:/ 'testInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { _tiarsLog            :: !(Maybe Text)
  , _tiarsPrincipalId    :: !(Maybe Text)
  , _tiarsLatency        :: !(Maybe Integer)
  , _tiarsAuthorization  :: !(Maybe (Map Text [Text]))
  , _tiarsClaims         :: !(Maybe (Map Text Text))
  , _tiarsClientStatus   :: !(Maybe Int)
  , _tiarsPolicy         :: !(Maybe Text)
  , _tiarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestInvokeAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiarsLog' - The API Gateway execution log for the test authorizer request.
--
-- * 'tiarsPrincipalId' - The principal identity returned by the 'Authorizer'
--
-- * 'tiarsLatency' - The execution latency of the test authorizer request.
--
-- * 'tiarsAuthorization' - Undocumented member.
--
-- * 'tiarsClaims' - The <http://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims> , with any supported custom attributes, returned from the Cognito Your User Pool configured for the API.
--
-- * 'tiarsClientStatus' - The HTTP status code that the client would have received. Value is 0 if the authorizer succeeded.
--
-- * 'tiarsPolicy' - The JSON policy document returned by the 'Authorizer'
--
-- * 'tiarsResponseStatus' - -- | The response status code.
testInvokeAuthorizerResponse
    :: Int -- ^ 'tiarsResponseStatus'
    -> TestInvokeAuthorizerResponse
testInvokeAuthorizerResponse pResponseStatus_ =
  TestInvokeAuthorizerResponse'
    { _tiarsLog = Nothing
    , _tiarsPrincipalId = Nothing
    , _tiarsLatency = Nothing
    , _tiarsAuthorization = Nothing
    , _tiarsClaims = Nothing
    , _tiarsClientStatus = Nothing
    , _tiarsPolicy = Nothing
    , _tiarsResponseStatus = pResponseStatus_
    }


-- | The API Gateway execution log for the test authorizer request.
tiarsLog :: Lens' TestInvokeAuthorizerResponse (Maybe Text)
tiarsLog = lens _tiarsLog (\ s a -> s{_tiarsLog = a})

-- | The principal identity returned by the 'Authorizer'
tiarsPrincipalId :: Lens' TestInvokeAuthorizerResponse (Maybe Text)
tiarsPrincipalId = lens _tiarsPrincipalId (\ s a -> s{_tiarsPrincipalId = a})

-- | The execution latency of the test authorizer request.
tiarsLatency :: Lens' TestInvokeAuthorizerResponse (Maybe Integer)
tiarsLatency = lens _tiarsLatency (\ s a -> s{_tiarsLatency = a})

-- | Undocumented member.
tiarsAuthorization :: Lens' TestInvokeAuthorizerResponse (HashMap Text [Text])
tiarsAuthorization = lens _tiarsAuthorization (\ s a -> s{_tiarsAuthorization = a}) . _Default . _Map

-- | The <http://openid.net/specs/openid-connect-core-1_0.html#StandardClaims open identity claims> , with any supported custom attributes, returned from the Cognito Your User Pool configured for the API.
tiarsClaims :: Lens' TestInvokeAuthorizerResponse (HashMap Text Text)
tiarsClaims = lens _tiarsClaims (\ s a -> s{_tiarsClaims = a}) . _Default . _Map

-- | The HTTP status code that the client would have received. Value is 0 if the authorizer succeeded.
tiarsClientStatus :: Lens' TestInvokeAuthorizerResponse (Maybe Int)
tiarsClientStatus = lens _tiarsClientStatus (\ s a -> s{_tiarsClientStatus = a})

-- | The JSON policy document returned by the 'Authorizer'
tiarsPolicy :: Lens' TestInvokeAuthorizerResponse (Maybe Text)
tiarsPolicy = lens _tiarsPolicy (\ s a -> s{_tiarsPolicy = a})

-- | -- | The response status code.
tiarsResponseStatus :: Lens' TestInvokeAuthorizerResponse Int
tiarsResponseStatus = lens _tiarsResponseStatus (\ s a -> s{_tiarsResponseStatus = a})

instance NFData TestInvokeAuthorizerResponse where
