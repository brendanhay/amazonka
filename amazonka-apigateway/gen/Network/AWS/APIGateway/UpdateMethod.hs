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
-- Module      : Network.AWS.APIGateway.UpdateMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'Method' resource.
--
--
module Network.AWS.APIGateway.UpdateMethod
    (
    -- * Creating a Request
      updateMethod
    , UpdateMethod
    -- * Request Lenses
    , ummPatchOperations
    , ummRestAPIId
    , ummResourceId
    , ummHttpMethod

    -- * Destructuring the Response
    , method
    , Method
    -- * Response Lenses
    , mMethodResponses
    , mHttpMethod
    , mAuthorizationScopes
    , mRequestValidatorId
    , mRequestModels
    , mRequestParameters
    , mAuthorizerId
    , mOperationName
    , mAuthorizationType
    , mApiKeyRequired
    , mMethodIntegration
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update an existing 'Method' resource.
--
--
--
-- /See:/ 'updateMethod' smart constructor.
data UpdateMethod = UpdateMethod'
  { _ummPatchOperations :: !(Maybe [PatchOperation])
  , _ummRestAPIId       :: !Text
  , _ummResourceId      :: !Text
  , _ummHttpMethod      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ummPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'ummRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'ummResourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
--
-- * 'ummHttpMethod' - [Required] The HTTP verb of the 'Method' resource.
updateMethod
    :: Text -- ^ 'ummRestAPIId'
    -> Text -- ^ 'ummResourceId'
    -> Text -- ^ 'ummHttpMethod'
    -> UpdateMethod
updateMethod pRestAPIId_ pResourceId_ pHttpMethod_ =
  UpdateMethod'
    { _ummPatchOperations = Nothing
    , _ummRestAPIId = pRestAPIId_
    , _ummResourceId = pResourceId_
    , _ummHttpMethod = pHttpMethod_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
ummPatchOperations :: Lens' UpdateMethod [PatchOperation]
ummPatchOperations = lens _ummPatchOperations (\ s a -> s{_ummPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
ummRestAPIId :: Lens' UpdateMethod Text
ummRestAPIId = lens _ummRestAPIId (\ s a -> s{_ummRestAPIId = a})

-- | [Required] The 'Resource' identifier for the 'Method' resource.
ummResourceId :: Lens' UpdateMethod Text
ummResourceId = lens _ummResourceId (\ s a -> s{_ummResourceId = a})

-- | [Required] The HTTP verb of the 'Method' resource.
ummHttpMethod :: Lens' UpdateMethod Text
ummHttpMethod = lens _ummHttpMethod (\ s a -> s{_ummHttpMethod = a})

instance AWSRequest UpdateMethod where
        type Rs UpdateMethod = Method
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateMethod where

instance NFData UpdateMethod where

instance ToHeaders UpdateMethod where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateMethod where
        toJSON UpdateMethod'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _ummPatchOperations])

instance ToPath UpdateMethod where
        toPath UpdateMethod'{..}
          = mconcat
              ["/restapis/", toBS _ummRestAPIId, "/resources/",
               toBS _ummResourceId, "/methods/",
               toBS _ummHttpMethod]

instance ToQuery UpdateMethod where
        toQuery = const mempty
