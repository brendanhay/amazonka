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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current ApiKey resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/GetAPIKey.html AWS API Reference> for GetAPIKey.
module Network.AWS.APIGateway.GetAPIKey
    (
    -- * Creating a Request
      getAPIKey
    , GetAPIKey
    -- * Request Lenses
    , gakApiKey

    -- * Destructuring the Response
    , apiKey
    , APIKey
    -- * Response Lenses
    , akEnabled
    , akCreatedDate
    , akName
    , akId
    , akStageKeys
    , akLastUpdatedDate
    , akDescription
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to get information about the current ApiKey resource.
--
-- /See:/ 'getAPIKey' smart constructor.
newtype GetAPIKey = GetAPIKey'
    { _gakApiKey :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gakApiKey'
getAPIKey
    :: Text -- ^ 'gakApiKey'
    -> GetAPIKey
getAPIKey pApiKey_ =
    GetAPIKey'
    { _gakApiKey = pApiKey_
    }

-- | The identifier of the ApiKey resource.
gakApiKey :: Lens' GetAPIKey Text
gakApiKey = lens _gakApiKey (\ s a -> s{_gakApiKey = a});

instance AWSRequest GetAPIKey where
        type Rs GetAPIKey = APIKey
        request = get aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders GetAPIKey where
        toHeaders = const mempty

instance ToPath GetAPIKey where
        toPath GetAPIKey'{..}
          = mconcat ["/apikeys/", toBS _gakApiKey]

instance ToQuery GetAPIKey where
        toQuery = const mempty
