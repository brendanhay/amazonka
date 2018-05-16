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
-- Module      : Network.AWS.Pinpoint.GetCampaignActivities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the activity performed by a campaign.
module Network.AWS.Pinpoint.GetCampaignActivities
    (
    -- * Creating a Request
      getCampaignActivities
    , GetCampaignActivities
    -- * Request Lenses
    , gcaToken
    , gcaPageSize
    , gcaApplicationId
    , gcaCampaignId

    -- * Destructuring the Response
    , getCampaignActivitiesResponse
    , GetCampaignActivitiesResponse
    -- * Response Lenses
    , gcarsResponseStatus
    , gcarsActivitiesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaignActivities' smart constructor.
data GetCampaignActivities = GetCampaignActivities'
  { _gcaToken         :: !(Maybe Text)
  , _gcaPageSize      :: !(Maybe Text)
  , _gcaApplicationId :: !Text
  , _gcaCampaignId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcaToken' - The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gcaPageSize' - The number of entries you want on each page in the response.
--
-- * 'gcaApplicationId' - Undocumented member.
--
-- * 'gcaCampaignId' - Undocumented member.
getCampaignActivities
    :: Text -- ^ 'gcaApplicationId'
    -> Text -- ^ 'gcaCampaignId'
    -> GetCampaignActivities
getCampaignActivities pApplicationId_ pCampaignId_ =
  GetCampaignActivities'
    { _gcaToken = Nothing
    , _gcaPageSize = Nothing
    , _gcaApplicationId = pApplicationId_
    , _gcaCampaignId = pCampaignId_
    }


-- | The NextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gcaToken :: Lens' GetCampaignActivities (Maybe Text)
gcaToken = lens _gcaToken (\ s a -> s{_gcaToken = a})

-- | The number of entries you want on each page in the response.
gcaPageSize :: Lens' GetCampaignActivities (Maybe Text)
gcaPageSize = lens _gcaPageSize (\ s a -> s{_gcaPageSize = a})

-- | Undocumented member.
gcaApplicationId :: Lens' GetCampaignActivities Text
gcaApplicationId = lens _gcaApplicationId (\ s a -> s{_gcaApplicationId = a})

-- | Undocumented member.
gcaCampaignId :: Lens' GetCampaignActivities Text
gcaCampaignId = lens _gcaCampaignId (\ s a -> s{_gcaCampaignId = a})

instance AWSRequest GetCampaignActivities where
        type Rs GetCampaignActivities =
             GetCampaignActivitiesResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetCampaignActivitiesResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetCampaignActivities where

instance NFData GetCampaignActivities where

instance ToHeaders GetCampaignActivities where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCampaignActivities where
        toPath GetCampaignActivities'{..}
          = mconcat
              ["/v1/apps/", toBS _gcaApplicationId, "/campaigns/",
               toBS _gcaCampaignId, "/activities"]

instance ToQuery GetCampaignActivities where
        toQuery GetCampaignActivities'{..}
          = mconcat
              ["token" =: _gcaToken, "page-size" =: _gcaPageSize]

-- | /See:/ 'getCampaignActivitiesResponse' smart constructor.
data GetCampaignActivitiesResponse = GetCampaignActivitiesResponse'
  { _gcarsResponseStatus     :: !Int
  , _gcarsActivitiesResponse :: !ActivitiesResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcarsResponseStatus' - -- | The response status code.
--
-- * 'gcarsActivitiesResponse' - Undocumented member.
getCampaignActivitiesResponse
    :: Int -- ^ 'gcarsResponseStatus'
    -> ActivitiesResponse -- ^ 'gcarsActivitiesResponse'
    -> GetCampaignActivitiesResponse
getCampaignActivitiesResponse pResponseStatus_ pActivitiesResponse_ =
  GetCampaignActivitiesResponse'
    { _gcarsResponseStatus = pResponseStatus_
    , _gcarsActivitiesResponse = pActivitiesResponse_
    }


-- | -- | The response status code.
gcarsResponseStatus :: Lens' GetCampaignActivitiesResponse Int
gcarsResponseStatus = lens _gcarsResponseStatus (\ s a -> s{_gcarsResponseStatus = a})

-- | Undocumented member.
gcarsActivitiesResponse :: Lens' GetCampaignActivitiesResponse ActivitiesResponse
gcarsActivitiesResponse = lens _gcarsActivitiesResponse (\ s a -> s{_gcarsActivitiesResponse = a})

instance NFData GetCampaignActivitiesResponse where
