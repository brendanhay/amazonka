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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration response.
--
--
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
    , intContentHandling
    , intResponseTemplates
    , intSelectionPattern
    , intStatusCode
    , intResponseParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents an update integration response request.
--
--
--
-- /See:/ 'updateIntegrationResponse' smart constructor.
data UpdateIntegrationResponse = UpdateIntegrationResponse'
  { _uiPatchOperations :: !(Maybe [PatchOperation])
  , _uiRestAPIId       :: !Text
  , _uiResourceId      :: !Text
  , _uiHttpMethod      :: !Text
  , _uiStatusCode      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uiRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'uiResourceId' - [Required] Specifies an update integration response request's resource identifier.
--
-- * 'uiHttpMethod' - [Required] Specifies an update integration response request's HTTP method.
--
-- * 'uiStatusCode' - [Required] Specifies an update integration response request's status code.
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


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uiPatchOperations :: Lens' UpdateIntegrationResponse [PatchOperation]
uiPatchOperations = lens _uiPatchOperations (\ s a -> s{_uiPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
uiRestAPIId :: Lens' UpdateIntegrationResponse Text
uiRestAPIId = lens _uiRestAPIId (\ s a -> s{_uiRestAPIId = a})

-- | [Required] Specifies an update integration response request's resource identifier.
uiResourceId :: Lens' UpdateIntegrationResponse Text
uiResourceId = lens _uiResourceId (\ s a -> s{_uiResourceId = a})

-- | [Required] Specifies an update integration response request's HTTP method.
uiHttpMethod :: Lens' UpdateIntegrationResponse Text
uiHttpMethod = lens _uiHttpMethod (\ s a -> s{_uiHttpMethod = a})

-- | [Required] Specifies an update integration response request's status code.
uiStatusCode :: Lens' UpdateIntegrationResponse Text
uiStatusCode = lens _uiStatusCode (\ s a -> s{_uiStatusCode = a})

instance AWSRequest UpdateIntegrationResponse where
        type Rs UpdateIntegrationResponse =
             IntegrationResponse
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateIntegrationResponse where

instance NFData UpdateIntegrationResponse where

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
