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
-- Module      : Network.AWS.APIGateway.PutIntegrationResponse
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a put integration.
module Network.AWS.APIGateway.PutIntegrationResponse
    (
    -- * Creating a Request
      putIntegrationResponse
    , PutIntegrationResponse
    -- * Request Lenses
    , piResponseTemplates
    , piSelectionPattern
    , piResponseParameters
    , piRestAPIId
    , piResourceId
    , piHttpMethod
    , piStatusCode

    -- * Destructuring the Response
    , integrationResponse
    , IntegrationResponse
    -- * Response Lenses
    , iResponseTemplates
    , iSelectionPattern
    , iStatusCode
    , iResponseParameters
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a put integration response request.
--
-- /See:/ 'putIntegrationResponse' smart constructor.
data PutIntegrationResponse = PutIntegrationResponse'
    { _piResponseTemplates  :: !(Maybe (Map Text Text))
    , _piSelectionPattern   :: !(Maybe Text)
    , _piResponseParameters :: !(Maybe (Map Text Text))
    , _piRestAPIId          :: !Text
    , _piResourceId         :: !Text
    , _piHttpMethod         :: !Text
    , _piStatusCode         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piResponseTemplates'
--
-- * 'piSelectionPattern'
--
-- * 'piResponseParameters'
--
-- * 'piRestAPIId'
--
-- * 'piResourceId'
--
-- * 'piHttpMethod'
--
-- * 'piStatusCode'
putIntegrationResponse
    :: Text -- ^ 'piRestAPIId'
    -> Text -- ^ 'piResourceId'
    -> Text -- ^ 'piHttpMethod'
    -> Text -- ^ 'piStatusCode'
    -> PutIntegrationResponse
putIntegrationResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
    PutIntegrationResponse'
    { _piResponseTemplates = Nothing
    , _piSelectionPattern = Nothing
    , _piResponseParameters = Nothing
    , _piRestAPIId = pRestAPIId_
    , _piResourceId = pResourceId_
    , _piHttpMethod = pHttpMethod_
    , _piStatusCode = pStatusCode_
    }

-- | Specifies a put integration response\'s templates.
piResponseTemplates :: Lens' PutIntegrationResponse (HashMap Text Text)
piResponseTemplates = lens _piResponseTemplates (\ s a -> s{_piResponseTemplates = a}) . _Default . _Map;

-- | Specifies the selection pattern of a put integration response.
piSelectionPattern :: Lens' PutIntegrationResponse (Maybe Text)
piSelectionPattern = lens _piSelectionPattern (\ s a -> s{_piSelectionPattern = a});

-- | Represents response parameters that can be read from the backend
-- response. Response parameters are represented as a key\/value map, with
-- a destination as the key and a source as the value. A destination must
-- match an existing response parameter in the < Method>. The source can be
-- a header from the backend response, or a static value. Static values are
-- specified using enclosing single quotes, and backend response headers
-- can be read using the pattern 'integration.response.header.{name}'.
piResponseParameters :: Lens' PutIntegrationResponse (HashMap Text Text)
piResponseParameters = lens _piResponseParameters (\ s a -> s{_piResponseParameters = a}) . _Default . _Map;

-- | Specifies a put integration response request\'s API identifier.
piRestAPIId :: Lens' PutIntegrationResponse Text
piRestAPIId = lens _piRestAPIId (\ s a -> s{_piRestAPIId = a});

-- | Specifies a put integration response request\'s resource identifier.
piResourceId :: Lens' PutIntegrationResponse Text
piResourceId = lens _piResourceId (\ s a -> s{_piResourceId = a});

-- | Specifies a put integration response request\'s HTTP method.
piHttpMethod :: Lens' PutIntegrationResponse Text
piHttpMethod = lens _piHttpMethod (\ s a -> s{_piHttpMethod = a});

-- | Specifies the status code that is used to map the integration response
-- to an existing < MethodResponse>.
piStatusCode :: Lens' PutIntegrationResponse Text
piStatusCode = lens _piStatusCode (\ s a -> s{_piStatusCode = a});

instance AWSRequest PutIntegrationResponse where
        type Rs PutIntegrationResponse = IntegrationResponse
        request = putJSON aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutIntegrationResponse

instance ToHeaders PutIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON PutIntegrationResponse where
        toJSON PutIntegrationResponse'{..}
          = object
              (catMaybes
                 [("responseTemplates" .=) <$> _piResponseTemplates,
                  ("selectionPattern" .=) <$> _piSelectionPattern,
                  ("responseParameters" .=) <$> _piResponseParameters])

instance ToPath PutIntegrationResponse where
        toPath PutIntegrationResponse'{..}
          = mconcat
              ["/restapis/", toBS _piRestAPIId, "/resources/",
               toBS _piResourceId, "/methods/", toBS _piHttpMethod,
               "/integration/responses/", toBS _piStatusCode]

instance ToQuery PutIntegrationResponse where
        toQuery = const mempty
