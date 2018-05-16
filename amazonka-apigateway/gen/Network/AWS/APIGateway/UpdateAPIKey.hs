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
-- Module      : Network.AWS.APIGateway.UpdateAPIKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an 'ApiKey' resource.
--
--
module Network.AWS.APIGateway.UpdateAPIKey
    (
    -- * Creating a Request
      updateAPIKey
    , UpdateAPIKey
    -- * Request Lenses
    , uakPatchOperations
    , uakApiKey

    -- * Destructuring the Response
    , apiKey
    , APIKey
    -- * Response Lenses
    , akEnabled
    , akValue
    , akCustomerId
    , akCreatedDate
    , akName
    , akId
    , akStageKeys
    , akLastUpdatedDate
    , akDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to change information about an 'ApiKey' resource.
--
--
--
-- /See:/ 'updateAPIKey' smart constructor.
data UpdateAPIKey = UpdateAPIKey'
  { _uakPatchOperations :: !(Maybe [PatchOperation])
  , _uakApiKey          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uakPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uakApiKey' - [Required] The identifier of the 'ApiKey' resource to be updated.
updateAPIKey
    :: Text -- ^ 'uakApiKey'
    -> UpdateAPIKey
updateAPIKey pApiKey_ =
  UpdateAPIKey' {_uakPatchOperations = Nothing, _uakApiKey = pApiKey_}


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uakPatchOperations :: Lens' UpdateAPIKey [PatchOperation]
uakPatchOperations = lens _uakPatchOperations (\ s a -> s{_uakPatchOperations = a}) . _Default . _Coerce

-- | [Required] The identifier of the 'ApiKey' resource to be updated.
uakApiKey :: Lens' UpdateAPIKey Text
uakApiKey = lens _uakApiKey (\ s a -> s{_uakApiKey = a})

instance AWSRequest UpdateAPIKey where
        type Rs UpdateAPIKey = APIKey
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateAPIKey where

instance NFData UpdateAPIKey where

instance ToHeaders UpdateAPIKey where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateAPIKey where
        toJSON UpdateAPIKey'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uakPatchOperations])

instance ToPath UpdateAPIKey where
        toPath UpdateAPIKey'{..}
          = mconcat ["/apikeys/", toBS _uakApiKey]

instance ToQuery UpdateAPIKey where
        toQuery = const mempty
