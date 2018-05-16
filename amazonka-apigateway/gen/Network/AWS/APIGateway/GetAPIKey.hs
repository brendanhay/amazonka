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
-- Module      : Network.AWS.APIGateway.GetAPIKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ApiKey' resource.
--
--
module Network.AWS.APIGateway.GetAPIKey
    (
    -- * Creating a Request
      getAPIKey
    , GetAPIKey
    -- * Request Lenses
    , gakIncludeValue
    , gakApiKey

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

-- | A request to get information about the current 'ApiKey' resource.
--
--
--
-- /See:/ 'getAPIKey' smart constructor.
data GetAPIKey = GetAPIKey'
  { _gakIncludeValue :: !(Maybe Bool)
  , _gakApiKey       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gakIncludeValue' - A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains the key value.
--
-- * 'gakApiKey' - [Required] The identifier of the 'ApiKey' resource.
getAPIKey
    :: Text -- ^ 'gakApiKey'
    -> GetAPIKey
getAPIKey pApiKey_ =
  GetAPIKey' {_gakIncludeValue = Nothing, _gakApiKey = pApiKey_}


-- | A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains the key value.
gakIncludeValue :: Lens' GetAPIKey (Maybe Bool)
gakIncludeValue = lens _gakIncludeValue (\ s a -> s{_gakIncludeValue = a})

-- | [Required] The identifier of the 'ApiKey' resource.
gakApiKey :: Lens' GetAPIKey Text
gakApiKey = lens _gakApiKey (\ s a -> s{_gakApiKey = a})

instance AWSRequest GetAPIKey where
        type Rs GetAPIKey = APIKey
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetAPIKey where

instance NFData GetAPIKey where

instance ToHeaders GetAPIKey where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetAPIKey where
        toPath GetAPIKey'{..}
          = mconcat ["/apikeys/", toBS _gakApiKey]

instance ToQuery GetAPIKey where
        toQuery GetAPIKey'{..}
          = mconcat ["includeValue" =: _gakIncludeValue]
