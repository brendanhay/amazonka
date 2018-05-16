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
    , gcsToken
    , gcsPageSize
    , gcsApplicationId

    -- * Destructuring the Response
    , getCampaignsResponse
    , GetCampaignsResponse
    -- * Response Lenses
    , gcsrsResponseStatus
    , gcsrsCampaignsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'
  { _gcsToken         :: !(Maybe Text)
  , _gcsPageSize      :: !(Maybe Text)
  , _gcsApplicationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaigns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsToken' - The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gcsPageSize' - The number of entries you want on each page in the response.
--
-- * 'gcsApplicationId' - Undocumented member.
getCampaigns
    :: Text -- ^ 'gcsApplicationId'
    -> GetCampaigns
getCampaigns pApplicationId_ =
  GetCampaigns'
    { _gcsToken = Nothing
    , _gcsPageSize = Nothing
    , _gcsApplicationId = pApplicationId_
    }


-- | The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gcsToken :: Lens' GetCampaigns (Maybe Text)
gcsToken = lens _gcsToken (\ s a -> s{_gcsToken = a})

-- | The number of entries you want on each page in the response.
gcsPageSize :: Lens' GetCampaigns (Maybe Text)
gcsPageSize = lens _gcsPageSize (\ s a -> s{_gcsPageSize = a})

-- | Undocumented member.
gcsApplicationId :: Lens' GetCampaigns Text
gcsApplicationId = lens _gcsApplicationId (\ s a -> s{_gcsApplicationId = a})

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
              ["/v1/apps/", toBS _gcsApplicationId, "/campaigns"]

instance ToQuery GetCampaigns where
        toQuery GetCampaigns'{..}
          = mconcat
              ["token" =: _gcsToken, "page-size" =: _gcsPageSize]

-- | /See:/ 'getCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'
  { _gcsrsResponseStatus    :: !Int
  , _gcsrsCampaignsResponse :: !CampaignsResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsrsResponseStatus' - -- | The response status code.
--
-- * 'gcsrsCampaignsResponse' - Undocumented member.
getCampaignsResponse
    :: Int -- ^ 'gcsrsResponseStatus'
    -> CampaignsResponse -- ^ 'gcsrsCampaignsResponse'
    -> GetCampaignsResponse
getCampaignsResponse pResponseStatus_ pCampaignsResponse_ =
  GetCampaignsResponse'
    { _gcsrsResponseStatus = pResponseStatus_
    , _gcsrsCampaignsResponse = pCampaignsResponse_
    }


-- | -- | The response status code.
gcsrsResponseStatus :: Lens' GetCampaignsResponse Int
gcsrsResponseStatus = lens _gcsrsResponseStatus (\ s a -> s{_gcsrsResponseStatus = a})

-- | Undocumented member.
gcsrsCampaignsResponse :: Lens' GetCampaignsResponse CampaignsResponse
gcsrsCampaignsResponse = lens _gcsrsCampaignsResponse (\ s a -> s{_gcsrsCampaignsResponse = a})

instance NFData GetCampaignsResponse where
