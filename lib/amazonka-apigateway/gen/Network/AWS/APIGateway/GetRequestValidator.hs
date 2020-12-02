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
-- Module      : Network.AWS.APIGateway.GetRequestValidator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a 'RequestValidator' of a given 'RestApi' .
--
--
module Network.AWS.APIGateway.GetRequestValidator
    (
    -- * Creating a Request
      getRequestValidator
    , GetRequestValidator
    -- * Request Lenses
    , grvrRestAPIId
    , grvrRequestValidatorId

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

-- | Gets a 'RequestValidator' of a given 'RestApi' .
--
--
--
-- /See:/ 'getRequestValidator' smart constructor.
data GetRequestValidator = GetRequestValidator'
  { _grvrRestAPIId          :: !Text
  , _grvrRequestValidatorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRequestValidator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grvrRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'grvrRequestValidatorId' - [Required] The identifier of the 'RequestValidator' to be retrieved.
getRequestValidator
    :: Text -- ^ 'grvrRestAPIId'
    -> Text -- ^ 'grvrRequestValidatorId'
    -> GetRequestValidator
getRequestValidator pRestAPIId_ pRequestValidatorId_ =
  GetRequestValidator'
    { _grvrRestAPIId = pRestAPIId_
    , _grvrRequestValidatorId = pRequestValidatorId_
    }


-- | [Required] The string identifier of the associated 'RestApi' .
grvrRestAPIId :: Lens' GetRequestValidator Text
grvrRestAPIId = lens _grvrRestAPIId (\ s a -> s{_grvrRestAPIId = a})

-- | [Required] The identifier of the 'RequestValidator' to be retrieved.
grvrRequestValidatorId :: Lens' GetRequestValidator Text
grvrRequestValidatorId = lens _grvrRequestValidatorId (\ s a -> s{_grvrRequestValidatorId = a})

instance AWSRequest GetRequestValidator where
        type Rs GetRequestValidator = RequestValidator
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetRequestValidator where

instance NFData GetRequestValidator where

instance ToHeaders GetRequestValidator where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetRequestValidator where
        toPath GetRequestValidator'{..}
          = mconcat
              ["/restapis/", toBS _grvrRestAPIId,
               "/requestvalidators/", toBS _grvrRequestValidatorId]

instance ToQuery GetRequestValidator where
        toQuery = const mempty
