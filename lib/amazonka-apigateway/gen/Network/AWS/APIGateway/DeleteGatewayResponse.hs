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
-- Module      : Network.AWS.APIGateway.DeleteGatewayResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears any customization of a 'GatewayResponse' of a specified response type on the given 'RestApi' and resets it with the default settings.
--
--
module Network.AWS.APIGateway.DeleteGatewayResponse
    (
    -- * Creating a Request
      deleteGatewayResponse
    , DeleteGatewayResponse
    -- * Request Lenses
    , dgRestAPIId
    , dgResponseType

    -- * Destructuring the Response
    , deleteGatewayResponseResponse
    , DeleteGatewayResponseResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Clears any customization of a 'GatewayResponse' of a specified response type on the given 'RestApi' and resets it with the default settings.
--
--
--
-- /See:/ 'deleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { _dgRestAPIId    :: !Text
  , _dgResponseType :: !GatewayResponseType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'dgResponseType' - [Required] The response type of the associated 'GatewayResponse' . Valid values are     * ACCESS_DENIED    * API_CONFIGURATION_ERROR    * AUTHORIZER_FAILURE    * AUTHORIZER_CONFIGURATION_ERROR    * BAD_REQUEST_PARAMETERS    * BAD_REQUEST_BODY    * DEFAULT_4XX    * DEFAULT_5XX    * EXPIRED_TOKEN    * INVALID_SIGNATURE    * INTEGRATION_FAILURE    * INTEGRATION_TIMEOUT    * INVALID_API_KEY    * MISSING_AUTHENTICATION_TOKEN    * QUOTA_EXCEEDED    * REQUEST_TOO_LARGE    * RESOURCE_NOT_FOUND    * THROTTLED    * UNAUTHORIZED    * UNSUPPORTED_MEDIA_TYPE
deleteGatewayResponse
    :: Text -- ^ 'dgRestAPIId'
    -> GatewayResponseType -- ^ 'dgResponseType'
    -> DeleteGatewayResponse
deleteGatewayResponse pRestAPIId_ pResponseType_ =
  DeleteGatewayResponse'
    {_dgRestAPIId = pRestAPIId_, _dgResponseType = pResponseType_}


-- | [Required] The string identifier of the associated 'RestApi' .
dgRestAPIId :: Lens' DeleteGatewayResponse Text
dgRestAPIId = lens _dgRestAPIId (\ s a -> s{_dgRestAPIId = a})

-- | [Required] The response type of the associated 'GatewayResponse' . Valid values are     * ACCESS_DENIED    * API_CONFIGURATION_ERROR    * AUTHORIZER_FAILURE    * AUTHORIZER_CONFIGURATION_ERROR    * BAD_REQUEST_PARAMETERS    * BAD_REQUEST_BODY    * DEFAULT_4XX    * DEFAULT_5XX    * EXPIRED_TOKEN    * INVALID_SIGNATURE    * INTEGRATION_FAILURE    * INTEGRATION_TIMEOUT    * INVALID_API_KEY    * MISSING_AUTHENTICATION_TOKEN    * QUOTA_EXCEEDED    * REQUEST_TOO_LARGE    * RESOURCE_NOT_FOUND    * THROTTLED    * UNAUTHORIZED    * UNSUPPORTED_MEDIA_TYPE
dgResponseType :: Lens' DeleteGatewayResponse GatewayResponseType
dgResponseType = lens _dgResponseType (\ s a -> s{_dgResponseType = a})

instance AWSRequest DeleteGatewayResponse where
        type Rs DeleteGatewayResponse =
             DeleteGatewayResponseResponse
        request = delete apiGateway
        response = receiveNull DeleteGatewayResponseResponse'

instance Hashable DeleteGatewayResponse where

instance NFData DeleteGatewayResponse where

instance ToHeaders DeleteGatewayResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteGatewayResponse where
        toPath DeleteGatewayResponse'{..}
          = mconcat
              ["/restapis/", toBS _dgRestAPIId,
               "/gatewayresponses/", toBS _dgResponseType]

instance ToQuery DeleteGatewayResponse where
        toQuery = const mempty

-- | /See:/ 'deleteGatewayResponseResponse' smart constructor.
data DeleteGatewayResponseResponse =
  DeleteGatewayResponseResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGatewayResponseResponse' with the minimum fields required to make a request.
--
deleteGatewayResponseResponse
    :: DeleteGatewayResponseResponse
deleteGatewayResponseResponse = DeleteGatewayResponseResponse'


instance NFData DeleteGatewayResponseResponse where
