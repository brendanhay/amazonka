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
-- Module      : Network.AWS.APIGateway.UpdateModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a model.
--
--
module Network.AWS.APIGateway.UpdateModel
    (
    -- * Creating a Request
      updateModel
    , UpdateModel
    -- * Request Lenses
    , uPatchOperations
    , uRestAPIId
    , uModelName

    -- * Destructuring the Response
    , model
    , Model
    -- * Response Lenses
    , mSchema
    , mName
    , mId
    , mDescription
    , mContentType
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update an existing model in an existing 'RestApi' resource.
--
--
--
-- /See:/ 'updateModel' smart constructor.
data UpdateModel = UpdateModel'
  { _uPatchOperations :: !(Maybe [PatchOperation])
  , _uRestAPIId       :: !Text
  , _uModelName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'uModelName' - [Required] The name of the model to update.
updateModel
    :: Text -- ^ 'uRestAPIId'
    -> Text -- ^ 'uModelName'
    -> UpdateModel
updateModel pRestAPIId_ pModelName_ =
  UpdateModel'
    { _uPatchOperations = Nothing
    , _uRestAPIId = pRestAPIId_
    , _uModelName = pModelName_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uPatchOperations :: Lens' UpdateModel [PatchOperation]
uPatchOperations = lens _uPatchOperations (\ s a -> s{_uPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
uRestAPIId :: Lens' UpdateModel Text
uRestAPIId = lens _uRestAPIId (\ s a -> s{_uRestAPIId = a})

-- | [Required] The name of the model to update.
uModelName :: Lens' UpdateModel Text
uModelName = lens _uModelName (\ s a -> s{_uModelName = a})

instance AWSRequest UpdateModel where
        type Rs UpdateModel = Model
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateModel where

instance NFData UpdateModel where

instance ToHeaders UpdateModel where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateModel where
        toJSON UpdateModel'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uPatchOperations])

instance ToPath UpdateModel where
        toPath UpdateModel'{..}
          = mconcat
              ["/restapis/", toBS _uRestAPIId, "/models/",
               toBS _uModelName]

instance ToQuery UpdateModel where
        toQuery = const mempty
