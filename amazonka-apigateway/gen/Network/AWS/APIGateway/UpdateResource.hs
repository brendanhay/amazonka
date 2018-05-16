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
-- Module      : Network.AWS.APIGateway.UpdateResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Resource' resource.
--
--
module Network.AWS.APIGateway.UpdateResource
    (
    -- * Creating a Request
      updateResource
    , UpdateResource
    -- * Request Lenses
    , urPatchOperations
    , urRestAPIId
    , urResourceId

    -- * Destructuring the Response
    , resource
    , Resource
    -- * Response Lenses
    , rPathPart
    , rPath
    , rId
    , rResourceMethods
    , rParentId
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to change information about a 'Resource' resource.
--
--
--
-- /See:/ 'updateResource' smart constructor.
data UpdateResource = UpdateResource'
  { _urPatchOperations :: !(Maybe [PatchOperation])
  , _urRestAPIId       :: !Text
  , _urResourceId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'urRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'urResourceId' - [Required] The identifier of the 'Resource' resource.
updateResource
    :: Text -- ^ 'urRestAPIId'
    -> Text -- ^ 'urResourceId'
    -> UpdateResource
updateResource pRestAPIId_ pResourceId_ =
  UpdateResource'
    { _urPatchOperations = Nothing
    , _urRestAPIId = pRestAPIId_
    , _urResourceId = pResourceId_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
urPatchOperations :: Lens' UpdateResource [PatchOperation]
urPatchOperations = lens _urPatchOperations (\ s a -> s{_urPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
urRestAPIId :: Lens' UpdateResource Text
urRestAPIId = lens _urRestAPIId (\ s a -> s{_urRestAPIId = a})

-- | [Required] The identifier of the 'Resource' resource.
urResourceId :: Lens' UpdateResource Text
urResourceId = lens _urResourceId (\ s a -> s{_urResourceId = a})

instance AWSRequest UpdateResource where
        type Rs UpdateResource = Resource
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateResource where

instance NFData UpdateResource where

instance ToHeaders UpdateResource where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateResource where
        toJSON UpdateResource'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _urPatchOperations])

instance ToPath UpdateResource where
        toPath UpdateResource'{..}
          = mconcat
              ["/restapis/", toBS _urRestAPIId, "/resources/",
               toBS _urResourceId]

instance ToQuery UpdateResource where
        toQuery = const mempty
