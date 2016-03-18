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
-- Module      : Network.AWS.APIGateway.UpdateIntegrationResponse
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration response.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/UpdateIntegrationResponse.html AWS API Reference> for UpdateIntegrationResponse.
module Network.AWS.APIGateway.UpdateIntegrationResponse
    (
    -- * Creating a Request
      updateIntegrationResponse
    , UpdateIntegrationResponse
    -- * Request Lenses
    , uiPatchOperations
    , uiRestAPIId
    , uiResourceId
    , uiHttpMethod
    , uiStatusCode

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

-- | Represents an update integration response request.
--
-- /See:/ 'updateIntegrationResponse' smart constructor.
data UpdateIntegrationResponse = UpdateIntegrationResponse'
    { _uiPatchOperations :: !(Maybe [PatchOperation])
    , _uiRestAPIId       :: !Text
    , _uiResourceId      :: !Text
    , _uiHttpMethod      :: !Text
    , _uiStatusCode      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiPatchOperations'
--
-- * 'uiRestAPIId'
--
-- * 'uiResourceId'
--
-- * 'uiHttpMethod'
--
-- * 'uiStatusCode'
updateIntegrationResponse
    :: Text -- ^ 'uiRestAPIId'
    -> Text -- ^ 'uiResourceId'
    -> Text -- ^ 'uiHttpMethod'
    -> Text -- ^ 'uiStatusCode'
    -> UpdateIntegrationResponse
updateIntegrationResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
    UpdateIntegrationResponse'
    { _uiPatchOperations = Nothing
    , _uiRestAPIId = pRestAPIId_
    , _uiResourceId = pResourceId_
    , _uiHttpMethod = pHttpMethod_
    , _uiStatusCode = pStatusCode_
    }

-- | A list of operations describing the updates to apply to the specified
-- resource. The patches are applied in the order specified in the list.
uiPatchOperations :: Lens' UpdateIntegrationResponse [PatchOperation]
uiPatchOperations = lens _uiPatchOperations (\ s a -> s{_uiPatchOperations = a}) . _Default . _Coerce;

-- | Specifies an update integration response request\'s API identifier.
uiRestAPIId :: Lens' UpdateIntegrationResponse Text
uiRestAPIId = lens _uiRestAPIId (\ s a -> s{_uiRestAPIId = a});

-- | Specifies an update integration response request\'s resource identifier.
uiResourceId :: Lens' UpdateIntegrationResponse Text
uiResourceId = lens _uiResourceId (\ s a -> s{_uiResourceId = a});

-- | Specifies an update integration response request\'s HTTP method.
uiHttpMethod :: Lens' UpdateIntegrationResponse Text
uiHttpMethod = lens _uiHttpMethod (\ s a -> s{_uiHttpMethod = a});

-- | Specifies an update integration response request\'s status code.
uiStatusCode :: Lens' UpdateIntegrationResponse Text
uiStatusCode = lens _uiStatusCode (\ s a -> s{_uiStatusCode = a});

instance AWSRequest UpdateIntegrationResponse where
        type Rs UpdateIntegrationResponse =
             IntegrationResponse
        request = patchJSON aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders UpdateIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateIntegrationResponse where
        toJSON UpdateIntegrationResponse'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uiPatchOperations])

instance ToPath UpdateIntegrationResponse where
        toPath UpdateIntegrationResponse'{..}
          = mconcat
              ["/restapis/", toBS _uiRestAPIId, "/resources/",
               toBS _uiResourceId, "/methods/", toBS _uiHttpMethod,
               "/integration/responses/", toBS _uiStatusCode]

instance ToQuery UpdateIntegrationResponse where
        toQuery = const mempty
