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
-- Module      : Network.AWS.Pinpoint.GetApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an app.
module Network.AWS.Pinpoint.GetApp
    (
    -- * Creating a Request
      getApp
    , GetApp
    -- * Request Lenses
    , gaApplicationId

    -- * Destructuring the Response
    , getAppResponse
    , GetAppResponse
    -- * Response Lenses
    , garsResponseStatus
    , garsApplicationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getApp' smart constructor.
newtype GetApp = GetApp'
  { _gaApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaApplicationId' - Undocumented member.
getApp
    :: Text -- ^ 'gaApplicationId'
    -> GetApp
getApp pApplicationId_ = GetApp' {_gaApplicationId = pApplicationId_}


-- | Undocumented member.
gaApplicationId :: Lens' GetApp Text
gaApplicationId = lens _gaApplicationId (\ s a -> s{_gaApplicationId = a})

instance AWSRequest GetApp where
        type Rs GetApp = GetAppResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetAppResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetApp where

instance NFData GetApp where

instance ToHeaders GetApp where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetApp where
        toPath GetApp'{..}
          = mconcat ["/v1/apps/", toBS _gaApplicationId]

instance ToQuery GetApp where
        toQuery = const mempty

-- | /See:/ 'getAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { _garsResponseStatus      :: !Int
  , _garsApplicationResponse :: !ApplicationResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsResponseStatus' - -- | The response status code.
--
-- * 'garsApplicationResponse' - Undocumented member.
getAppResponse
    :: Int -- ^ 'garsResponseStatus'
    -> ApplicationResponse -- ^ 'garsApplicationResponse'
    -> GetAppResponse
getAppResponse pResponseStatus_ pApplicationResponse_ =
  GetAppResponse'
    { _garsResponseStatus = pResponseStatus_
    , _garsApplicationResponse = pApplicationResponse_
    }


-- | -- | The response status code.
garsResponseStatus :: Lens' GetAppResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a})

-- | Undocumented member.
garsApplicationResponse :: Lens' GetAppResponse ApplicationResponse
garsApplicationResponse = lens _garsApplicationResponse (\ s a -> s{_garsApplicationResponse = a})

instance NFData GetAppResponse where
