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
-- Module      : Network.AWS.APIGateway.PutGatewayResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customization of a 'GatewayResponse' of a specified response type and status code on the given 'RestApi' .
--
--
module Network.AWS.APIGateway.PutGatewayResponse
    (
    -- * Creating a Request
      putGatewayResponse
    , PutGatewayResponse
    -- * Request Lenses
    , pgResponseTemplates
    , pgStatusCode
    , pgResponseParameters
    , pgRestAPIId
    , pgResponseType

    -- * Destructuring the Response
    , gatewayResponse
    , GatewayResponse
    -- * Response Lenses
    , gDefaultResponse
    , gResponseTemplates
    , gResponseType
    , gStatusCode
    , gResponseParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Creates a customization of a 'GatewayResponse' of a specified response type and status code on the given 'RestApi' .
--
--
--
-- /See:/ 'putGatewayResponse' smart constructor.
data PutGatewayResponse = PutGatewayResponse'
  { _pgResponseTemplates  :: !(Maybe (Map Text Text))
  , _pgStatusCode         :: !(Maybe Text)
  , _pgResponseParameters :: !(Maybe (Map Text Text))
  , _pgRestAPIId          :: !Text
  , _pgResponseType       :: !GatewayResponseType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgResponseTemplates' - Response templates of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
-- * 'pgStatusCode' - 'GatewayResponse'
--
-- * 'pgResponseParameters' - Response parameters (paths, query strings and headers) of the 'GatewayResponse' as a string-to-string map of key-value pairs.
--
-- * 'pgRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'pgResponseType' - [Required] The response type of the associated 'GatewayResponse' . Valid values are     * ACCESS_DENIED    * API_CONFIGURATION_ERROR    * AUTHORIZER_FAILURE    * AUTHORIZER_CONFIGURATION_ERROR    * BAD_REQUEST_PARAMETERS    * BAD_REQUEST_BODY    * DEFAULT_4XX    * DEFAULT_5XX    * EXPIRED_TOKEN    * INVALID_SIGNATURE    * INTEGRATION_FAILURE    * INTEGRATION_TIMEOUT    * INVALID_API_KEY    * MISSING_AUTHENTICATION_TOKEN    * QUOTA_EXCEEDED    * REQUEST_TOO_LARGE    * RESOURCE_NOT_FOUND    * THROTTLED    * UNAUTHORIZED    * UNSUPPORTED_MEDIA_TYPE
putGatewayResponse
    :: Text -- ^ 'pgRestAPIId'
    -> GatewayResponseType -- ^ 'pgResponseType'
    -> PutGatewayResponse
putGatewayResponse pRestAPIId_ pResponseType_ =
  PutGatewayResponse'
    { _pgResponseTemplates = Nothing
    , _pgStatusCode = Nothing
    , _pgResponseParameters = Nothing
    , _pgRestAPIId = pRestAPIId_
    , _pgResponseType = pResponseType_
    }


-- | Response templates of the 'GatewayResponse' as a string-to-string map of key-value pairs.
pgResponseTemplates :: Lens' PutGatewayResponse (HashMap Text Text)
pgResponseTemplates = lens _pgResponseTemplates (\ s a -> s{_pgResponseTemplates = a}) . _Default . _Map

-- | 'GatewayResponse'
pgStatusCode :: Lens' PutGatewayResponse (Maybe Text)
pgStatusCode = lens _pgStatusCode (\ s a -> s{_pgStatusCode = a})

-- | Response parameters (paths, query strings and headers) of the 'GatewayResponse' as a string-to-string map of key-value pairs.
pgResponseParameters :: Lens' PutGatewayResponse (HashMap Text Text)
pgResponseParameters = lens _pgResponseParameters (\ s a -> s{_pgResponseParameters = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
pgRestAPIId :: Lens' PutGatewayResponse Text
pgRestAPIId = lens _pgRestAPIId (\ s a -> s{_pgRestAPIId = a})

-- | [Required] The response type of the associated 'GatewayResponse' . Valid values are     * ACCESS_DENIED    * API_CONFIGURATION_ERROR    * AUTHORIZER_FAILURE    * AUTHORIZER_CONFIGURATION_ERROR    * BAD_REQUEST_PARAMETERS    * BAD_REQUEST_BODY    * DEFAULT_4XX    * DEFAULT_5XX    * EXPIRED_TOKEN    * INVALID_SIGNATURE    * INTEGRATION_FAILURE    * INTEGRATION_TIMEOUT    * INVALID_API_KEY    * MISSING_AUTHENTICATION_TOKEN    * QUOTA_EXCEEDED    * REQUEST_TOO_LARGE    * RESOURCE_NOT_FOUND    * THROTTLED    * UNAUTHORIZED    * UNSUPPORTED_MEDIA_TYPE
pgResponseType :: Lens' PutGatewayResponse GatewayResponseType
pgResponseType = lens _pgResponseType (\ s a -> s{_pgResponseType = a})

instance AWSRequest PutGatewayResponse where
        type Rs PutGatewayResponse = GatewayResponse
        request = putJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutGatewayResponse where

instance NFData PutGatewayResponse where

instance ToHeaders PutGatewayResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON PutGatewayResponse where
        toJSON PutGatewayResponse'{..}
          = object
              (catMaybes
                 [("responseTemplates" .=) <$> _pgResponseTemplates,
                  ("statusCode" .=) <$> _pgStatusCode,
                  ("responseParameters" .=) <$> _pgResponseParameters])

instance ToPath PutGatewayResponse where
        toPath PutGatewayResponse'{..}
          = mconcat
              ["/restapis/", toBS _pgRestAPIId,
               "/gatewayresponses/", toBS _pgResponseType]

instance ToQuery PutGatewayResponse where
        toQuery = const mempty
