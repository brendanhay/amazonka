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
-- Module      : Network.AWS.APIGateway.UpdateRequestValidator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a 'RequestValidator' of a given 'RestApi' .
--
--
module Network.AWS.APIGateway.UpdateRequestValidator
    (
    -- * Creating a Request
      updateRequestValidator
    , UpdateRequestValidator
    -- * Request Lenses
    , urvPatchOperations
    , urvRestAPIId
    , urvRequestValidatorId

    -- * Destructuring the Response
    , requestValidator
    , RequestValidator
    -- * Response Lenses
    , rvValidateRequestParameters
    , rvName
    , rvValidateRequestBody
    , rvId
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates a 'RequestValidator' of a given 'RestApi' .
--
--
--
-- /See:/ 'updateRequestValidator' smart constructor.
data UpdateRequestValidator = UpdateRequestValidator'
  { _urvPatchOperations    :: !(Maybe [PatchOperation])
  , _urvRestAPIId          :: !Text
  , _urvRequestValidatorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRequestValidator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urvPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'urvRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'urvRequestValidatorId' - [Required] The identifier of 'RequestValidator' to be updated.
updateRequestValidator
    :: Text -- ^ 'urvRestAPIId'
    -> Text -- ^ 'urvRequestValidatorId'
    -> UpdateRequestValidator
updateRequestValidator pRestAPIId_ pRequestValidatorId_ =
  UpdateRequestValidator'
    { _urvPatchOperations = Nothing
    , _urvRestAPIId = pRestAPIId_
    , _urvRequestValidatorId = pRequestValidatorId_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
urvPatchOperations :: Lens' UpdateRequestValidator [PatchOperation]
urvPatchOperations = lens _urvPatchOperations (\ s a -> s{_urvPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
urvRestAPIId :: Lens' UpdateRequestValidator Text
urvRestAPIId = lens _urvRestAPIId (\ s a -> s{_urvRestAPIId = a})

-- | [Required] The identifier of 'RequestValidator' to be updated.
urvRequestValidatorId :: Lens' UpdateRequestValidator Text
urvRequestValidatorId = lens _urvRequestValidatorId (\ s a -> s{_urvRequestValidatorId = a})

instance AWSRequest UpdateRequestValidator where
        type Rs UpdateRequestValidator = RequestValidator
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateRequestValidator where

instance NFData UpdateRequestValidator where

instance ToHeaders UpdateRequestValidator where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateRequestValidator where
        toJSON UpdateRequestValidator'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _urvPatchOperations])

instance ToPath UpdateRequestValidator where
        toPath UpdateRequestValidator'{..}
          = mconcat
              ["/restapis/", toBS _urvRestAPIId,
               "/requestvalidators/", toBS _urvRequestValidatorId]

instance ToQuery UpdateRequestValidator where
        toQuery = const mempty
