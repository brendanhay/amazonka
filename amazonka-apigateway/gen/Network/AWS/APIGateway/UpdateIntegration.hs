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
-- Module      : Network.AWS.APIGateway.UpdateIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration.
--
--
module Network.AWS.APIGateway.UpdateIntegration
    (
    -- * Creating a Request
      updateIntegration
    , UpdateIntegration
    -- * Request Lenses
    , updPatchOperations
    , updRestAPIId
    , updResourceId
    , updHttpMethod

    -- * Destructuring the Response
    , integration
    , Integration
    -- * Response Lenses
    , iHttpMethod
    , iRequestTemplates
    , iCredentials
    , iConnectionId
    , iRequestParameters
    , iContentHandling
    , iPassthroughBehavior
    , iUri
    , iIntegrationResponses
    , iCacheNamespace
    , iTimeoutInMillis
    , iType
    , iConnectionType
    , iCacheKeyParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents an update integration request.
--
--
--
-- /See:/ 'updateIntegration' smart constructor.
data UpdateIntegration = UpdateIntegration'
  { _updPatchOperations :: !(Maybe [PatchOperation])
  , _updRestAPIId       :: !Text
  , _updResourceId      :: !Text
  , _updHttpMethod      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'updRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'updResourceId' - [Required] Represents an update integration request's resource identifier.
--
-- * 'updHttpMethod' - [Required] Represents an update integration request's HTTP method.
updateIntegration
    :: Text -- ^ 'updRestAPIId'
    -> Text -- ^ 'updResourceId'
    -> Text -- ^ 'updHttpMethod'
    -> UpdateIntegration
updateIntegration pRestAPIId_ pResourceId_ pHttpMethod_ =
  UpdateIntegration'
    { _updPatchOperations = Nothing
    , _updRestAPIId = pRestAPIId_
    , _updResourceId = pResourceId_
    , _updHttpMethod = pHttpMethod_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
updPatchOperations :: Lens' UpdateIntegration [PatchOperation]
updPatchOperations = lens _updPatchOperations (\ s a -> s{_updPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
updRestAPIId :: Lens' UpdateIntegration Text
updRestAPIId = lens _updRestAPIId (\ s a -> s{_updRestAPIId = a})

-- | [Required] Represents an update integration request's resource identifier.
updResourceId :: Lens' UpdateIntegration Text
updResourceId = lens _updResourceId (\ s a -> s{_updResourceId = a})

-- | [Required] Represents an update integration request's HTTP method.
updHttpMethod :: Lens' UpdateIntegration Text
updHttpMethod = lens _updHttpMethod (\ s a -> s{_updHttpMethod = a})

instance AWSRequest UpdateIntegration where
        type Rs UpdateIntegration = Integration
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateIntegration where

instance NFData UpdateIntegration where

instance ToHeaders UpdateIntegration where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateIntegration where
        toJSON UpdateIntegration'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _updPatchOperations])

instance ToPath UpdateIntegration where
        toPath UpdateIntegration'{..}
          = mconcat
              ["/restapis/", toBS _updRestAPIId, "/resources/",
               toBS _updResourceId, "/methods/",
               toBS _updHttpMethod, "/integration"]

instance ToQuery UpdateIntegration where
        toQuery = const mempty
