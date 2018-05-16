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
-- Module      : Network.AWS.APIGateway.UpdateRestAPI
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the specified API.
--
--
module Network.AWS.APIGateway.UpdateRestAPI
    (
    -- * Creating a Request
      updateRestAPI
    , UpdateRestAPI
    -- * Request Lenses
    , uraPatchOperations
    , uraRestAPIId

    -- * Destructuring the Response
    , restAPI
    , RestAPI
    -- * Response Lenses
    , raMinimumCompressionSize
    , raBinaryMediaTypes
    , raWarnings
    , raCreatedDate
    , raName
    , raVersion
    , raApiKeySource
    , raId
    , raPolicy
    , raEndpointConfiguration
    , raDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update an existing 'RestApi' resource in your collection.
--
--
--
-- /See:/ 'updateRestAPI' smart constructor.
data UpdateRestAPI = UpdateRestAPI'
  { _uraPatchOperations :: !(Maybe [PatchOperation])
  , _uraRestAPIId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uraPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uraRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
updateRestAPI
    :: Text -- ^ 'uraRestAPIId'
    -> UpdateRestAPI
updateRestAPI pRestAPIId_ =
  UpdateRestAPI' {_uraPatchOperations = Nothing, _uraRestAPIId = pRestAPIId_}


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uraPatchOperations :: Lens' UpdateRestAPI [PatchOperation]
uraPatchOperations = lens _uraPatchOperations (\ s a -> s{_uraPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
uraRestAPIId :: Lens' UpdateRestAPI Text
uraRestAPIId = lens _uraRestAPIId (\ s a -> s{_uraRestAPIId = a})

instance AWSRequest UpdateRestAPI where
        type Rs UpdateRestAPI = RestAPI
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateRestAPI where

instance NFData UpdateRestAPI where

instance ToHeaders UpdateRestAPI where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateRestAPI where
        toJSON UpdateRestAPI'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uraPatchOperations])

instance ToPath UpdateRestAPI where
        toPath UpdateRestAPI'{..}
          = mconcat ["/restapis/", toBS _uraRestAPIId]

instance ToQuery UpdateRestAPI where
        toQuery = const mempty
