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
-- Module      : Network.AWS.APIGateway.CreateRequestValidator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'ReqeustValidator' of a given 'RestApi' .
--
--
module Network.AWS.APIGateway.CreateRequestValidator
    (
    -- * Creating a Request
      createRequestValidator
    , CreateRequestValidator
    -- * Request Lenses
    , crvValidateRequestParameters
    , crvName
    , crvValidateRequestBody
    , crvRestAPIId

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

-- | Creates a 'RequestValidator' of a given 'RestApi' .
--
--
--
-- /See:/ 'createRequestValidator' smart constructor.
data CreateRequestValidator = CreateRequestValidator'
  { _crvValidateRequestParameters :: !(Maybe Bool)
  , _crvName                      :: !(Maybe Text)
  , _crvValidateRequestBody       :: !(Maybe Bool)
  , _crvRestAPIId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRequestValidator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crvValidateRequestParameters' - A Boolean flag to indicate whether to validate request parameters, @true@ , or not @false@ .
--
-- * 'crvName' - The name of the to-be-created 'RequestValidator' .
--
-- * 'crvValidateRequestBody' - A Boolean flag to indicate whether to validate request body according to the configured model schema for the method (@true@ ) or not (@false@ ).
--
-- * 'crvRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
createRequestValidator
    :: Text -- ^ 'crvRestAPIId'
    -> CreateRequestValidator
createRequestValidator pRestAPIId_ =
  CreateRequestValidator'
    { _crvValidateRequestParameters = Nothing
    , _crvName = Nothing
    , _crvValidateRequestBody = Nothing
    , _crvRestAPIId = pRestAPIId_
    }


-- | A Boolean flag to indicate whether to validate request parameters, @true@ , or not @false@ .
crvValidateRequestParameters :: Lens' CreateRequestValidator (Maybe Bool)
crvValidateRequestParameters = lens _crvValidateRequestParameters (\ s a -> s{_crvValidateRequestParameters = a})

-- | The name of the to-be-created 'RequestValidator' .
crvName :: Lens' CreateRequestValidator (Maybe Text)
crvName = lens _crvName (\ s a -> s{_crvName = a})

-- | A Boolean flag to indicate whether to validate request body according to the configured model schema for the method (@true@ ) or not (@false@ ).
crvValidateRequestBody :: Lens' CreateRequestValidator (Maybe Bool)
crvValidateRequestBody = lens _crvValidateRequestBody (\ s a -> s{_crvValidateRequestBody = a})

-- | [Required] The string identifier of the associated 'RestApi' .
crvRestAPIId :: Lens' CreateRequestValidator Text
crvRestAPIId = lens _crvRestAPIId (\ s a -> s{_crvRestAPIId = a})

instance AWSRequest CreateRequestValidator where
        type Rs CreateRequestValidator = RequestValidator
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateRequestValidator where

instance NFData CreateRequestValidator where

instance ToHeaders CreateRequestValidator where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateRequestValidator where
        toJSON CreateRequestValidator'{..}
          = object
              (catMaybes
                 [("validateRequestParameters" .=) <$>
                    _crvValidateRequestParameters,
                  ("name" .=) <$> _crvName,
                  ("validateRequestBody" .=) <$>
                    _crvValidateRequestBody])

instance ToPath CreateRequestValidator where
        toPath CreateRequestValidator'{..}
          = mconcat
              ["/restapis/", toBS _crvRestAPIId,
               "/requestvalidators"]

instance ToQuery CreateRequestValidator where
        toQuery = const mempty
