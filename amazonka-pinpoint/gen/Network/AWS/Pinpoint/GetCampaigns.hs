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
-- Module      : Network.AWS.Pinpoint.GetCampaigns
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about your campaigns.
module Network.AWS.Pinpoint.GetCampaigns
    (
    -- * Creating a Request
      getCampaigns
    , GetCampaigns
    -- * Request Lenses
    , gccToken
    , gccPageSize
    , gccApplicationId

    -- * Destructuring the Response
    , getCampaignsResponse
    , GetCampaignsResponse
    -- * Response Lenses
    , getrsResponseStatus
    , getrsCampaignsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'
  { _gccToken         :: !(Maybe Text)
  , _gccPageSize      :: !(Maybe Text)
  , _gccApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaigns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccToken' - The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gccPageSize' - The number of entries you want on each page in the response.
--
-- * 'gccApplicationId' - The unique ID of your Amazon Pinpoint application.
getCampaigns
    :: Text -- ^ 'gccApplicationId'
    -> GetCampaigns
getCampaigns pApplicationId_ =
  GetCampaigns'
    { _gccToken = Nothing
    , _gccPageSize = Nothing
    , _gccApplicationId = pApplicationId_
    }


-- | The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gccToken :: Lens' GetCampaigns (Maybe Text)
gccToken = lens _gccToken (\ s a -> s{_gccToken = a})

-- | The number of entries you want on each page in the response.
gccPageSize :: Lens' GetCampaigns (Maybe Text)
gccPageSize = lens _gccPageSize (\ s a -> s{_gccPageSize = a})

-- | The unique ID of your Amazon Pinpoint application.
gccApplicationId :: Lens' GetCampaigns Text
gccApplicationId = lens _gccApplicationId (\ s a -> s{_gccApplicationId = a})

instance AWSRequest GetCampaigns where
        type Rs GetCampaigns = GetCampaignsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetCampaignsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetCampaigns where

instance NFData GetCampaigns where

instance ToHeaders GetCampaigns where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCampaigns where
        toPath GetCampaigns'{..}
          = mconcat
              ["/v1/apps/", toBS _gccApplicationId, "/campaigns"]

instance ToQuery GetCampaigns where
        toQuery GetCampaigns'{..}
          = mconcat
              ["token" =: _gccToken, "page-size" =: _gccPageSize]

-- | /See:/ 'getCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'
  { _getrsResponseStatus    :: !Int
  , _getrsCampaignsResponse :: !CampaignsResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsResponseStatus' - -- | The response status code.
--
-- * 'getrsCampaignsResponse' - Undocumented member.
getCampaignsResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> CampaignsResponse -- ^ 'getrsCampaignsResponse'
    -> GetCampaignsResponse
getCampaignsResponse pResponseStatus_ pCampaignsResponse_ =
  GetCampaignsResponse'
    { _getrsResponseStatus = pResponseStatus_
    , _getrsCampaignsResponse = pCampaignsResponse_
    }


-- | -- | The response status code.
getrsResponseStatus :: Lens' GetCampaignsResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a})

-- | Undocumented member.
getrsCampaignsResponse :: Lens' GetCampaignsResponse CampaignsResponse
getrsCampaignsResponse = lens _getrsCampaignsResponse (\ s a -> s{_getrsCampaignsResponse = a})

instance NFData GetCampaignsResponse where
