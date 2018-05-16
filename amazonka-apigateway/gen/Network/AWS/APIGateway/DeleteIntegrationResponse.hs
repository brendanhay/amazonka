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
-- Module      : Network.AWS.APIGateway.DeleteIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a delete integration response.
--
--
module Network.AWS.APIGateway.DeleteIntegrationResponse
    (
    -- * Creating a Request
      deleteIntegrationResponse
    , DeleteIntegrationResponse
    -- * Request Lenses
    , diRestAPIId
    , diResourceId
    , diHttpMethod
    , diStatusCode

    -- * Destructuring the Response
    , deleteIntegrationResponseResponse
    , DeleteIntegrationResponseResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a delete integration response request.
--
--
--
-- /See:/ 'deleteIntegrationResponse' smart constructor.
data DeleteIntegrationResponse = DeleteIntegrationResponse'
  { _diRestAPIId  :: !Text
  , _diResourceId :: !Text
  , _diHttpMethod :: !Text
  , _diStatusCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'diResourceId' - [Required] Specifies a delete integration response request's resource identifier.
--
-- * 'diHttpMethod' - [Required] Specifies a delete integration response request's HTTP method.
--
-- * 'diStatusCode' - [Required] Specifies a delete integration response request's status code.
deleteIntegrationResponse
    :: Text -- ^ 'diRestAPIId'
    -> Text -- ^ 'diResourceId'
    -> Text -- ^ 'diHttpMethod'
    -> Text -- ^ 'diStatusCode'
    -> DeleteIntegrationResponse
deleteIntegrationResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
  DeleteIntegrationResponse'
    { _diRestAPIId = pRestAPIId_
    , _diResourceId = pResourceId_
    , _diHttpMethod = pHttpMethod_
    , _diStatusCode = pStatusCode_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
diRestAPIId :: Lens' DeleteIntegrationResponse Text
diRestAPIId = lens _diRestAPIId (\ s a -> s{_diRestAPIId = a})

-- | [Required] Specifies a delete integration response request's resource identifier.
diResourceId :: Lens' DeleteIntegrationResponse Text
diResourceId = lens _diResourceId (\ s a -> s{_diResourceId = a})

-- | [Required] Specifies a delete integration response request's HTTP method.
diHttpMethod :: Lens' DeleteIntegrationResponse Text
diHttpMethod = lens _diHttpMethod (\ s a -> s{_diHttpMethod = a})

-- | [Required] Specifies a delete integration response request's status code.
diStatusCode :: Lens' DeleteIntegrationResponse Text
diStatusCode = lens _diStatusCode (\ s a -> s{_diStatusCode = a})

instance AWSRequest DeleteIntegrationResponse where
        type Rs DeleteIntegrationResponse =
             DeleteIntegrationResponseResponse
        request = delete apiGateway
        response
          = receiveNull DeleteIntegrationResponseResponse'

instance Hashable DeleteIntegrationResponse where

instance NFData DeleteIntegrationResponse where

instance ToHeaders DeleteIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteIntegrationResponse where
        toPath DeleteIntegrationResponse'{..}
          = mconcat
              ["/restapis/", toBS _diRestAPIId, "/resources/",
               toBS _diResourceId, "/methods/", toBS _diHttpMethod,
               "/integration/responses/", toBS _diStatusCode]

instance ToQuery DeleteIntegrationResponse where
        toQuery = const mempty

-- | /See:/ 'deleteIntegrationResponseResponse' smart constructor.
data DeleteIntegrationResponseResponse =
  DeleteIntegrationResponseResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIntegrationResponseResponse' with the minimum fields required to make a request.
--
deleteIntegrationResponseResponse
    :: DeleteIntegrationResponseResponse
deleteIntegrationResponseResponse = DeleteIntegrationResponseResponse'


instance NFData DeleteIntegrationResponseResponse
         where
