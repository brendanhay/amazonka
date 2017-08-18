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
-- Module      : Network.AWS.APIGateway.UpdateMethodResponse
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'MethodResponse' resource.
--
--
module Network.AWS.APIGateway.UpdateMethodResponse
    (
    -- * Creating a Request
      updateMethodResponse
    , UpdateMethodResponse
    -- * Request Lenses
    , umPatchOperations
    , umRestAPIId
    , umResourceId
    , umHttpMethod
    , umStatusCode

    -- * Destructuring the Response
    , methodResponse
    , MethodResponse
    -- * Response Lenses
    , mResponseModels
    , mStatusCode
    , mResponseParameters
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to update an existing 'MethodResponse' resource.
--
--
--
-- /See:/ 'updateMethodResponse' smart constructor.
data UpdateMethodResponse = UpdateMethodResponse'
    { _umPatchOperations :: !(Maybe [PatchOperation])
    , _umRestAPIId       :: !Text
    , _umResourceId      :: !Text
    , _umHttpMethod      :: !Text
    , _umStatusCode      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'umRestAPIId' - The string identifier of the associated 'RestApi' .
--
-- * 'umResourceId' - The 'Resource' identifier for the 'MethodResponse' resource.
--
-- * 'umHttpMethod' - The HTTP verb of the 'Method' resource.
--
-- * 'umStatusCode' - The status code for the 'MethodResponse' resource.
updateMethodResponse
    :: Text -- ^ 'umRestAPIId'
    -> Text -- ^ 'umResourceId'
    -> Text -- ^ 'umHttpMethod'
    -> Text -- ^ 'umStatusCode'
    -> UpdateMethodResponse
updateMethodResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
    UpdateMethodResponse'
    { _umPatchOperations = Nothing
    , _umRestAPIId = pRestAPIId_
    , _umResourceId = pResourceId_
    , _umHttpMethod = pHttpMethod_
    , _umStatusCode = pStatusCode_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
umPatchOperations :: Lens' UpdateMethodResponse [PatchOperation]
umPatchOperations = lens _umPatchOperations (\ s a -> s{_umPatchOperations = a}) . _Default . _Coerce;

-- | The string identifier of the associated 'RestApi' .
umRestAPIId :: Lens' UpdateMethodResponse Text
umRestAPIId = lens _umRestAPIId (\ s a -> s{_umRestAPIId = a});

-- | The 'Resource' identifier for the 'MethodResponse' resource.
umResourceId :: Lens' UpdateMethodResponse Text
umResourceId = lens _umResourceId (\ s a -> s{_umResourceId = a});

-- | The HTTP verb of the 'Method' resource.
umHttpMethod :: Lens' UpdateMethodResponse Text
umHttpMethod = lens _umHttpMethod (\ s a -> s{_umHttpMethod = a});

-- | The status code for the 'MethodResponse' resource.
umStatusCode :: Lens' UpdateMethodResponse Text
umStatusCode = lens _umStatusCode (\ s a -> s{_umStatusCode = a});

instance AWSRequest UpdateMethodResponse where
        type Rs UpdateMethodResponse = MethodResponse
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateMethodResponse

instance NFData UpdateMethodResponse

instance ToHeaders UpdateMethodResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateMethodResponse where
        toJSON UpdateMethodResponse'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _umPatchOperations])

instance ToPath UpdateMethodResponse where
        toPath UpdateMethodResponse'{..}
          = mconcat
              ["/restapis/", toBS _umRestAPIId, "/resources/",
               toBS _umResourceId, "/methods/", toBS _umHttpMethod,
               "/responses/", toBS _umStatusCode]

instance ToQuery UpdateMethodResponse where
        toQuery = const mempty
