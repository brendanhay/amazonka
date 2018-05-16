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
-- Module      : Network.AWS.Pinpoint.GetCampaignVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific version of a campaign.
module Network.AWS.Pinpoint.GetCampaignVersion
    (
    -- * Creating a Request
      getCampaignVersion
    , GetCampaignVersion
    -- * Request Lenses
    , gcvcVersion
    , gcvcApplicationId
    , gcvcCampaignId

    -- * Destructuring the Response
    , getCampaignVersionResponse
    , GetCampaignVersionResponse
    -- * Response Lenses
    , gcvcrsResponseStatus
    , gcvcrsCampaignResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaignVersion' smart constructor.
data GetCampaignVersion = GetCampaignVersion'
  { _gcvcVersion       :: !Text
  , _gcvcApplicationId :: !Text
  , _gcvcCampaignId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvcVersion' - Undocumented member.
--
-- * 'gcvcApplicationId' - Undocumented member.
--
-- * 'gcvcCampaignId' - Undocumented member.
getCampaignVersion
    :: Text -- ^ 'gcvcVersion'
    -> Text -- ^ 'gcvcApplicationId'
    -> Text -- ^ 'gcvcCampaignId'
    -> GetCampaignVersion
getCampaignVersion pVersion_ pApplicationId_ pCampaignId_ =
  GetCampaignVersion'
    { _gcvcVersion = pVersion_
    , _gcvcApplicationId = pApplicationId_
    , _gcvcCampaignId = pCampaignId_
    }


-- | Undocumented member.
gcvcVersion :: Lens' GetCampaignVersion Text
gcvcVersion = lens _gcvcVersion (\ s a -> s{_gcvcVersion = a})

-- | Undocumented member.
gcvcApplicationId :: Lens' GetCampaignVersion Text
gcvcApplicationId = lens _gcvcApplicationId (\ s a -> s{_gcvcApplicationId = a})

-- | Undocumented member.
gcvcCampaignId :: Lens' GetCampaignVersion Text
gcvcCampaignId = lens _gcvcCampaignId (\ s a -> s{_gcvcCampaignId = a})

instance AWSRequest GetCampaignVersion where
        type Rs GetCampaignVersion =
             GetCampaignVersionResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetCampaignVersionResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetCampaignVersion where

instance NFData GetCampaignVersion where

instance ToHeaders GetCampaignVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCampaignVersion where
        toPath GetCampaignVersion'{..}
          = mconcat
              ["/v1/apps/", toBS _gcvcApplicationId, "/campaigns/",
               toBS _gcvcCampaignId, "/versions/",
               toBS _gcvcVersion]

instance ToQuery GetCampaignVersion where
        toQuery = const mempty

-- | /See:/ 'getCampaignVersionResponse' smart constructor.
data GetCampaignVersionResponse = GetCampaignVersionResponse'
  { _gcvcrsResponseStatus   :: !Int
  , _gcvcrsCampaignResponse :: !CampaignResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCampaignVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvcrsResponseStatus' - -- | The response status code.
--
-- * 'gcvcrsCampaignResponse' - Undocumented member.
getCampaignVersionResponse
    :: Int -- ^ 'gcvcrsResponseStatus'
    -> CampaignResponse -- ^ 'gcvcrsCampaignResponse'
    -> GetCampaignVersionResponse
getCampaignVersionResponse pResponseStatus_ pCampaignResponse_ =
  GetCampaignVersionResponse'
    { _gcvcrsResponseStatus = pResponseStatus_
    , _gcvcrsCampaignResponse = pCampaignResponse_
    }


-- | -- | The response status code.
gcvcrsResponseStatus :: Lens' GetCampaignVersionResponse Int
gcvcrsResponseStatus = lens _gcvcrsResponseStatus (\ s a -> s{_gcvcrsResponseStatus = a})

-- | Undocumented member.
gcvcrsCampaignResponse :: Lens' GetCampaignVersionResponse CampaignResponse
gcvcrsCampaignResponse = lens _gcvcrsCampaignResponse (\ s a -> s{_gcvcrsCampaignResponse = a})

instance NFData GetCampaignVersionResponse where
