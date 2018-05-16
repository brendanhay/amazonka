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
-- Module      : Network.AWS.Pinpoint.GetCampaignVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about your campaign versions.
module Network.AWS.Pinpoint.GetCampaignVersions
    (
    -- * Creating a Request
      getCampaignVersions
    , GetCampaignVersions
    -- * Request Lenses
    , gcvToken
    , gcvPageSize
    , gcvApplicationId
    , gcvCampaignId

    -- * Destructuring the Response
    , getCampaignVersionsResponse
    , GetCampaignVersionsResponse
    -- * Response Lenses
    , gcvrsResponseStatus
    , gcvrsCampaignsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaignVersions' smart constructor.
data GetCampaignVersions = GetCampaignVersions'
  { _gcvToken         :: !(Maybe Text)
  , _gcvPageSize      :: !(Maybe Text)
  , _gcvApplicationId :: !Text
  , _gcvCampaignId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvToken' - The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gcvPageSize' - The number of entries you want on each page in the response.
--
-- * 'gcvApplicationId' - Undocumented member.
--
-- * 'gcvCampaignId' - Undocumented member.
getCampaignVersions
    :: Text -- ^ 'gcvApplicationId'
    -> Text -- ^ 'gcvCampaignId'
    -> GetCampaignVersions
getCampaignVersions pApplicationId_ pCampaignId_ =
  GetCampaignVersions'
    { _gcvToken = Nothing
    , _gcvPageSize = Nothing
    , _gcvApplicationId = pApplicationId_
    , _gcvCampaignId = pCampaignId_
    }


-- | The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gcvToken :: Lens' GetCampaignVersions (Maybe Text)
gcvToken = lens _gcvToken (\ s a -> s{_gcvToken = a})

-- | The number of entries you want on each page in the response.
gcvPageSize :: Lens' GetCampaignVersions (Maybe Text)
gcvPageSize = lens _gcvPageSize (\ s a -> s{_gcvPageSize = a})

-- | Undocumented member.
gcvApplicationId :: Lens' GetCampaignVersions Text
gcvApplicationId = lens _gcvApplicationId (\ s a -> s{_gcvApplicationId = a})

-- | Undocumented member.
gcvCampaignId :: Lens' GetCampaignVersions Text
gcvCampaignId = lens _gcvCampaignId (\ s a -> s{_gcvCampaignId = a})

instance AWSRequest GetCampaignVersions where
        type Rs GetCampaignVersions =
             GetCampaignVersionsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetCampaignVersionsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetCampaignVersions where

instance NFData GetCampaignVersions where

instance ToHeaders GetCampaignVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCampaignVersions where
        toPath GetCampaignVersions'{..}
          = mconcat
              ["/v1/apps/", toBS _gcvApplicationId, "/campaigns/",
               toBS _gcvCampaignId, "/versions"]

instance ToQuery GetCampaignVersions where
        toQuery GetCampaignVersions'{..}
          = mconcat
              ["token" =: _gcvToken, "page-size" =: _gcvPageSize]

-- | /See:/ 'getCampaignVersionsResponse' smart constructor.
data GetCampaignVersionsResponse = GetCampaignVersionsResponse'
  { _gcvrsResponseStatus    :: !Int
  , _gcvrsCampaignsResponse :: !CampaignsResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvrsResponseStatus' - -- | The response status code.
--
-- * 'gcvrsCampaignsResponse' - Undocumented member.
getCampaignVersionsResponse
    :: Int -- ^ 'gcvrsResponseStatus'
    -> CampaignsResponse -- ^ 'gcvrsCampaignsResponse'
    -> GetCampaignVersionsResponse
getCampaignVersionsResponse pResponseStatus_ pCampaignsResponse_ =
  GetCampaignVersionsResponse'
    { _gcvrsResponseStatus = pResponseStatus_
    , _gcvrsCampaignsResponse = pCampaignsResponse_
    }


-- | -- | The response status code.
gcvrsResponseStatus :: Lens' GetCampaignVersionsResponse Int
gcvrsResponseStatus = lens _gcvrsResponseStatus (\ s a -> s{_gcvrsResponseStatus = a})

-- | Undocumented member.
gcvrsCampaignsResponse :: Lens' GetCampaignVersionsResponse CampaignsResponse
gcvrsCampaignsResponse = lens _gcvrsCampaignsResponse (\ s a -> s{_gcvrsCampaignsResponse = a})

instance NFData GetCampaignVersionsResponse where
