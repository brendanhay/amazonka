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
-- Module      : Network.AWS.APIGateway.TestInvokeMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the execution of a 'Method' in your 'RestApi' with headers, parameters, and an incoming request body.
--
--
module Network.AWS.APIGateway.TestInvokeMethod
    (
    -- * Creating a Request
      testInvokeMethod
    , TestInvokeMethod
    -- * Request Lenses
    , timPathWithQueryString
    , timBody
    , timClientCertificateId
    , timStageVariables
    , timHeaders
    , timRestAPIId
    , timResourceId
    , timHttpMethod

    -- * Destructuring the Response
    , testInvokeMethodResponse
    , TestInvokeMethodResponse
    -- * Response Lenses
    , timrsLog
    , timrsStatus
    , timrsBody
    , timrsLatency
    , timrsHeaders
    , timrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Make a request to simulate the execution of a 'Method' .
--
--
--
-- /See:/ 'testInvokeMethod' smart constructor.
data TestInvokeMethod = TestInvokeMethod'
  { _timPathWithQueryString :: !(Maybe Text)
  , _timBody                :: !(Maybe Text)
  , _timClientCertificateId :: !(Maybe Text)
  , _timStageVariables      :: !(Maybe (Map Text Text))
  , _timHeaders             :: !(Maybe (Map Text Text))
  , _timRestAPIId           :: !Text
  , _timResourceId          :: !Text
  , _timHttpMethod          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestInvokeMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'timPathWithQueryString' - The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
--
-- * 'timBody' - The simulated request body of an incoming invocation request.
--
-- * 'timClientCertificateId' - A 'ClientCertificate' identifier to use in the test invocation. API Gateway will use the certificate when making the HTTPS request to the defined back-end endpoint.
--
-- * 'timStageVariables' - A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
--
-- * 'timHeaders' - A key-value map of headers to simulate an incoming invocation request.
--
-- * 'timRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'timResourceId' - [Required] Specifies a test invoke method request's resource ID.
--
-- * 'timHttpMethod' - [Required] Specifies a test invoke method request's HTTP method.
testInvokeMethod
    :: Text -- ^ 'timRestAPIId'
    -> Text -- ^ 'timResourceId'
    -> Text -- ^ 'timHttpMethod'
    -> TestInvokeMethod
testInvokeMethod pRestAPIId_ pResourceId_ pHttpMethod_ =
  TestInvokeMethod'
    { _timPathWithQueryString = Nothing
    , _timBody = Nothing
    , _timClientCertificateId = Nothing
    , _timStageVariables = Nothing
    , _timHeaders = Nothing
    , _timRestAPIId = pRestAPIId_
    , _timResourceId = pResourceId_
    , _timHttpMethod = pHttpMethod_
    }


-- | The URI path, including query string, of the simulated invocation request. Use this to specify path parameters and query string parameters.
timPathWithQueryString :: Lens' TestInvokeMethod (Maybe Text)
timPathWithQueryString = lens _timPathWithQueryString (\ s a -> s{_timPathWithQueryString = a})

-- | The simulated request body of an incoming invocation request.
timBody :: Lens' TestInvokeMethod (Maybe Text)
timBody = lens _timBody (\ s a -> s{_timBody = a})

-- | A 'ClientCertificate' identifier to use in the test invocation. API Gateway will use the certificate when making the HTTPS request to the defined back-end endpoint.
timClientCertificateId :: Lens' TestInvokeMethod (Maybe Text)
timClientCertificateId = lens _timClientCertificateId (\ s a -> s{_timClientCertificateId = a})

-- | A key-value map of stage variables to simulate an invocation on a deployed 'Stage' .
timStageVariables :: Lens' TestInvokeMethod (HashMap Text Text)
timStageVariables = lens _timStageVariables (\ s a -> s{_timStageVariables = a}) . _Default . _Map

-- | A key-value map of headers to simulate an incoming invocation request.
timHeaders :: Lens' TestInvokeMethod (HashMap Text Text)
timHeaders = lens _timHeaders (\ s a -> s{_timHeaders = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
timRestAPIId :: Lens' TestInvokeMethod Text
timRestAPIId = lens _timRestAPIId (\ s a -> s{_timRestAPIId = a})

-- | [Required] Specifies a test invoke method request's resource ID.
timResourceId :: Lens' TestInvokeMethod Text
timResourceId = lens _timResourceId (\ s a -> s{_timResourceId = a})

-- | [Required] Specifies a test invoke method request's HTTP method.
timHttpMethod :: Lens' TestInvokeMethod Text
timHttpMethod = lens _timHttpMethod (\ s a -> s{_timHttpMethod = a})

instance AWSRequest TestInvokeMethod where
        type Rs TestInvokeMethod = TestInvokeMethodResponse
        request = postJSON apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 TestInvokeMethodResponse' <$>
                   (x .?> "log") <*> (x .?> "status") <*> (x .?> "body")
                     <*> (x .?> "latency")
                     <*> (x .?> "headers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable TestInvokeMethod where

instance NFData TestInvokeMethod where

instance ToHeaders TestInvokeMethod where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON TestInvokeMethod where
        toJSON TestInvokeMethod'{..}
          = object
              (catMaybes
                 [("pathWithQueryString" .=) <$>
                    _timPathWithQueryString,
                  ("body" .=) <$> _timBody,
                  ("clientCertificateId" .=) <$>
                    _timClientCertificateId,
                  ("stageVariables" .=) <$> _timStageVariables,
                  ("headers" .=) <$> _timHeaders])

instance ToPath TestInvokeMethod where
        toPath TestInvokeMethod'{..}
          = mconcat
              ["/restapis/", toBS _timRestAPIId, "/resources/",
               toBS _timResourceId, "/methods/",
               toBS _timHttpMethod]

instance ToQuery TestInvokeMethod where
        toQuery = const mempty

-- | Represents the response of the test invoke request in the HTTP method.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-test-method.html#how-to-test-method-console Test API using the API Gateway console>
--
-- /See:/ 'testInvokeMethodResponse' smart constructor.
data TestInvokeMethodResponse = TestInvokeMethodResponse'
  { _timrsLog            :: !(Maybe Text)
  , _timrsStatus         :: !(Maybe Int)
  , _timrsBody           :: !(Maybe Text)
  , _timrsLatency        :: !(Maybe Integer)
  , _timrsHeaders        :: !(Maybe (Map Text Text))
  , _timrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestInvokeMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'timrsLog' - The API Gateway execution log for the test invoke request.
--
-- * 'timrsStatus' - The HTTP status code.
--
-- * 'timrsBody' - The body of the HTTP response.
--
-- * 'timrsLatency' - The execution latency of the test invoke request.
--
-- * 'timrsHeaders' - The headers of the HTTP response.
--
-- * 'timrsResponseStatus' - -- | The response status code.
testInvokeMethodResponse
    :: Int -- ^ 'timrsResponseStatus'
    -> TestInvokeMethodResponse
testInvokeMethodResponse pResponseStatus_ =
  TestInvokeMethodResponse'
    { _timrsLog = Nothing
    , _timrsStatus = Nothing
    , _timrsBody = Nothing
    , _timrsLatency = Nothing
    , _timrsHeaders = Nothing
    , _timrsResponseStatus = pResponseStatus_
    }


-- | The API Gateway execution log for the test invoke request.
timrsLog :: Lens' TestInvokeMethodResponse (Maybe Text)
timrsLog = lens _timrsLog (\ s a -> s{_timrsLog = a})

-- | The HTTP status code.
timrsStatus :: Lens' TestInvokeMethodResponse (Maybe Int)
timrsStatus = lens _timrsStatus (\ s a -> s{_timrsStatus = a})

-- | The body of the HTTP response.
timrsBody :: Lens' TestInvokeMethodResponse (Maybe Text)
timrsBody = lens _timrsBody (\ s a -> s{_timrsBody = a})

-- | The execution latency of the test invoke request.
timrsLatency :: Lens' TestInvokeMethodResponse (Maybe Integer)
timrsLatency = lens _timrsLatency (\ s a -> s{_timrsLatency = a})

-- | The headers of the HTTP response.
timrsHeaders :: Lens' TestInvokeMethodResponse (HashMap Text Text)
timrsHeaders = lens _timrsHeaders (\ s a -> s{_timrsHeaders = a}) . _Default . _Map

-- | -- | The response status code.
timrsResponseStatus :: Lens' TestInvokeMethodResponse Int
timrsResponseStatus = lens _timrsResponseStatus (\ s a -> s{_timrsResponseStatus = a})

instance NFData TestInvokeMethodResponse where
