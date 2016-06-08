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
-- Module      : Network.AWS.APIGateway.GetRestAPI
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the < RestApi> resource in the collection.
module Network.AWS.APIGateway.GetRestAPI
    (
    -- * Creating a Request
      getRestAPI
    , GetRestAPI
    -- * Request Lenses
    , graRestAPIId

    -- * Destructuring the Response
    , restAPI
    , RestAPI
    -- * Response Lenses
    , raWarnings
    , raCreatedDate
    , raName
    , raId
    , raDescription
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The GET request to list an existing < RestApi> defined for your collection.
--
-- /See:/ 'getRestAPI' smart constructor.
newtype GetRestAPI = GetRestAPI'
    { _graRestAPIId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetRestAPI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'graRestAPIId'
getRestAPI
    :: Text -- ^ 'graRestAPIId'
    -> GetRestAPI
getRestAPI pRestAPIId_ =
    GetRestAPI'
    { _graRestAPIId = pRestAPIId_
    }

-- | The identifier of the < RestApi> resource.
graRestAPIId :: Lens' GetRestAPI Text
graRestAPIId = lens _graRestAPIId (\ s a -> s{_graRestAPIId = a});

instance AWSRequest GetRestAPI where
        type Rs GetRestAPI = RestAPI
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetRestAPI

instance NFData GetRestAPI

instance ToHeaders GetRestAPI where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetRestAPI where
        toPath GetRestAPI'{..}
          = mconcat ["/restapis/", toBS _graRestAPIId]

instance ToQuery GetRestAPI where
        toQuery = const mempty
