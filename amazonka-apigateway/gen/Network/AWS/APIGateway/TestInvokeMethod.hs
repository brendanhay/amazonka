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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/TestInvokeMethod.html AWS API Reference> for TestInvokeMethod.
module Network.AWS.APIGateway.TestInvokeMethod
    (
    -- * Creating a Request
      testInvokeMethod
    , TestInvokeMethod
    -- * Request Lenses
    , timPathWithQueryString
    , timBody
    , timClientCertificateId
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

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'testInvokeMethod' smart constructor.
data TestInvokeMethod = TestInvokeMethod'
    { _timPathWithQueryString :: !(Maybe Text)
    , _timBody                :: !(Maybe Text)
    , _timClientCertificateId :: !(Maybe Text)
    , _timHeaders             :: !(Maybe (Map Text Text))
    , _timRestAPIId           :: !Text
    , _timResourceId          :: !Text
    , _timHttpMethod          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestInvokeMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'timPathWithQueryString'
--
-- * 'timBody'
--
-- * 'timClientCertificateId'
--
-- * 'timHeaders'
--
-- * 'timRestAPIId'
--
-- * 'timResourceId'
--
-- * 'timHttpMethod'
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
    , _timHeaders = Nothing
    , _timRestAPIId = pRestAPIId_
    , _timResourceId = pResourceId_
    , _timHttpMethod = pHttpMethod_
    }

-- | Undocumented member.
timPathWithQueryString :: Lens' TestInvokeMethod (Maybe Text)
timPathWithQueryString = lens _timPathWithQueryString (\ s a -> s{_timPathWithQueryString = a});

-- | Undocumented member.
timBody :: Lens' TestInvokeMethod (Maybe Text)
timBody = lens _timBody (\ s a -> s{_timBody = a});

-- | Undocumented member.
timClientCertificateId :: Lens' TestInvokeMethod (Maybe Text)
timClientCertificateId = lens _timClientCertificateId (\ s a -> s{_timClientCertificateId = a});

-- | Undocumented member.
timHeaders :: Lens' TestInvokeMethod (HashMap Text Text)
timHeaders = lens _timHeaders (\ s a -> s{_timHeaders = a}) . _Default . _Map;

-- | Undocumented member.
timRestAPIId :: Lens' TestInvokeMethod Text
timRestAPIId = lens _timRestAPIId (\ s a -> s{_timRestAPIId = a});

-- | Undocumented member.
timResourceId :: Lens' TestInvokeMethod Text
timResourceId = lens _timResourceId (\ s a -> s{_timResourceId = a});

-- | Undocumented member.
timHttpMethod :: Lens' TestInvokeMethod Text
timHttpMethod = lens _timHttpMethod (\ s a -> s{_timHttpMethod = a});

instance AWSRequest TestInvokeMethod where
        type Rs TestInvokeMethod = TestInvokeMethodResponse
        request = postJSON aPIGateway
        response
          = receiveJSON
              (\ s h x ->
                 TestInvokeMethodResponse' <$>
                   (x .?> "log") <*> (x .?> "status") <*> (x .?> "body")
                     <*> (x .?> "latency")
                     <*> (x .?> "headers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders TestInvokeMethod where
        toHeaders = const mempty

instance ToJSON TestInvokeMethod where
        toJSON TestInvokeMethod'{..}
          = object
              (catMaybes
                 [("pathWithQueryString" .=) <$>
                    _timPathWithQueryString,
                  ("body" .=) <$> _timBody,
                  ("clientCertificateId" .=) <$>
                    _timClientCertificateId,
                  ("headers" .=) <$> _timHeaders])

instance ToPath TestInvokeMethod where
        toPath TestInvokeMethod'{..}
          = mconcat
              ["/restapis/", toBS _timRestAPIId, "/resources/",
               toBS _timResourceId, "/methods/",
               toBS _timHttpMethod]

instance ToQuery TestInvokeMethod where
        toQuery = const mempty

-- | Represents the response of the test invoke request in HTTP method.
--
-- /See:/ 'testInvokeMethodResponse' smart constructor.
data TestInvokeMethodResponse = TestInvokeMethodResponse'
    { _timrsLog            :: !(Maybe Text)
    , _timrsStatus         :: !(Maybe Int)
    , _timrsBody           :: !(Maybe Text)
    , _timrsLatency        :: !(Maybe Integer)
    , _timrsHeaders        :: !(Maybe (Map Text Text))
    , _timrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TestInvokeMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'timrsLog'
--
-- * 'timrsStatus'
--
-- * 'timrsBody'
--
-- * 'timrsLatency'
--
-- * 'timrsHeaders'
--
-- * 'timrsResponseStatus'
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

-- | The Amazon API Gateway execution log for the test invoke request.
timrsLog :: Lens' TestInvokeMethodResponse (Maybe Text)
timrsLog = lens _timrsLog (\ s a -> s{_timrsLog = a});

-- | The HTTP status code.
timrsStatus :: Lens' TestInvokeMethodResponse (Maybe Int)
timrsStatus = lens _timrsStatus (\ s a -> s{_timrsStatus = a});

-- | The body of HTTP response.
timrsBody :: Lens' TestInvokeMethodResponse (Maybe Text)
timrsBody = lens _timrsBody (\ s a -> s{_timrsBody = a});

-- | The execution latency of the test invoke request.
timrsLatency :: Lens' TestInvokeMethodResponse (Maybe Integer)
timrsLatency = lens _timrsLatency (\ s a -> s{_timrsLatency = a});

-- | The headers of HTTP response.
timrsHeaders :: Lens' TestInvokeMethodResponse (HashMap Text Text)
timrsHeaders = lens _timrsHeaders (\ s a -> s{_timrsHeaders = a}) . _Default . _Map;

-- | The response status code.
timrsResponseStatus :: Lens' TestInvokeMethodResponse Int
timrsResponseStatus = lens _timrsResponseStatus (\ s a -> s{_timrsResponseStatus = a});
