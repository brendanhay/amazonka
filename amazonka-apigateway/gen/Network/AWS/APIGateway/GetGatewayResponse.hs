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
-- Module      : Network.AWS.APIGateway.GetGatewayResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a 'GatewayResponse' of a specified response type on the given 'RestApi' .
--
--
module Network.AWS.APIGateway.GetGatewayResponse
    (
    -- * Creating a Request
      getGatewayResponse
    , GetGatewayResponse
    -- * Request Lenses
    , gggRestAPIId
    , gggResponseType

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

-- | Gets a 'GatewayResponse' of a specified response type on the given 'RestApi' .
--
--
--
-- /See:/ 'getGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { _gggRestAPIId    :: !Text
  , _gggResponseType :: !GatewayResponseType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gggRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gggResponseType' - [Required] The response type of the associated 'GatewayResponse' . Valid values are     * ACCESS_DENIED    * API_CONFIGURATION_ERROR    * AUTHORIZER_FAILURE    * AUTHORIZER_CONFIGURATION_ERROR    * BAD_REQUEST_PARAMETERS    * BAD_REQUEST_BODY    * DEFAULT_4XX    * DEFAULT_5XX    * EXPIRED_TOKEN    * INVALID_SIGNATURE    * INTEGRATION_FAILURE    * INTEGRATION_TIMEOUT    * INVALID_API_KEY    * MISSING_AUTHENTICATION_TOKEN    * QUOTA_EXCEEDED    * REQUEST_TOO_LARGE    * RESOURCE_NOT_FOUND    * THROTTLED    * UNAUTHORIZED    * UNSUPPORTED_MEDIA_TYPE
getGatewayResponse
    :: Text -- ^ 'gggRestAPIId'
    -> GatewayResponseType -- ^ 'gggResponseType'
    -> GetGatewayResponse
getGatewayResponse pRestAPIId_ pResponseType_ =
  GetGatewayResponse'
    {_gggRestAPIId = pRestAPIId_, _gggResponseType = pResponseType_}


-- | [Required] The string identifier of the associated 'RestApi' .
gggRestAPIId :: Lens' GetGatewayResponse Text
gggRestAPIId = lens _gggRestAPIId (\ s a -> s{_gggRestAPIId = a})

-- | [Required] The response type of the associated 'GatewayResponse' . Valid values are     * ACCESS_DENIED    * API_CONFIGURATION_ERROR    * AUTHORIZER_FAILURE    * AUTHORIZER_CONFIGURATION_ERROR    * BAD_REQUEST_PARAMETERS    * BAD_REQUEST_BODY    * DEFAULT_4XX    * DEFAULT_5XX    * EXPIRED_TOKEN    * INVALID_SIGNATURE    * INTEGRATION_FAILURE    * INTEGRATION_TIMEOUT    * INVALID_API_KEY    * MISSING_AUTHENTICATION_TOKEN    * QUOTA_EXCEEDED    * REQUEST_TOO_LARGE    * RESOURCE_NOT_FOUND    * THROTTLED    * UNAUTHORIZED    * UNSUPPORTED_MEDIA_TYPE
gggResponseType :: Lens' GetGatewayResponse GatewayResponseType
gggResponseType = lens _gggResponseType (\ s a -> s{_gggResponseType = a})

instance AWSRequest GetGatewayResponse where
        type Rs GetGatewayResponse = GatewayResponse
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetGatewayResponse where

instance NFData GetGatewayResponse where

instance ToHeaders GetGatewayResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetGatewayResponse where
        toPath GetGatewayResponse'{..}
          = mconcat
              ["/restapis/", toBS _gggRestAPIId,
               "/gatewayresponses/", toBS _gggResponseType]

instance ToQuery GetGatewayResponse where
        toQuery = const mempty
