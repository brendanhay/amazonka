{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for a campaign.
module Network.AWS.Pinpoint.GetCampaign
  ( -- * Creating a Request
    getCampaign,
    GetCampaign,

    -- * Request Lenses
    getCampaignId,
    getApplicationId,

    -- * Destructuring the Response
    getCampaignResponse,
    GetCampaignResponse,

    -- * Response Lenses
    gcrsResponseStatus,
    gcrsCampaignResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaign' smart constructor.
data GetCampaign = GetCampaign'
  { _getCampaignId :: !Text,
    _getApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getCampaignId' - The unique identifier for the campaign.
--
-- * 'getApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getCampaign ::
  -- | 'getCampaignId'
  Text ->
  -- | 'getApplicationId'
  Text ->
  GetCampaign
getCampaign pCampaignId_ pApplicationId_ =
  GetCampaign'
    { _getCampaignId = pCampaignId_,
      _getApplicationId = pApplicationId_
    }

-- | The unique identifier for the campaign.
getCampaignId :: Lens' GetCampaign Text
getCampaignId = lens _getCampaignId (\s a -> s {_getCampaignId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getApplicationId :: Lens' GetCampaign Text
getApplicationId = lens _getApplicationId (\s a -> s {_getApplicationId = a})

instance AWSRequest GetCampaign where
  type Rs GetCampaign = GetCampaignResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetCampaignResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetCampaign

instance NFData GetCampaign

instance ToHeaders GetCampaign where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetCampaign where
  toPath GetCampaign' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _getApplicationId,
        "/campaigns/",
        toBS _getCampaignId
      ]

instance ToQuery GetCampaign where
  toQuery = const mempty

-- | /See:/ 'getCampaignResponse' smart constructor.
data GetCampaignResponse = GetCampaignResponse'
  { _gcrsResponseStatus ::
      !Int,
    _gcrsCampaignResponse :: !CampaignResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsResponseStatus' - -- | The response status code.
--
-- * 'gcrsCampaignResponse' - Undocumented member.
getCampaignResponse ::
  -- | 'gcrsResponseStatus'
  Int ->
  -- | 'gcrsCampaignResponse'
  CampaignResponse ->
  GetCampaignResponse
getCampaignResponse pResponseStatus_ pCampaignResponse_ =
  GetCampaignResponse'
    { _gcrsResponseStatus = pResponseStatus_,
      _gcrsCampaignResponse = pCampaignResponse_
    }

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetCampaignResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\s a -> s {_gcrsResponseStatus = a})

-- | Undocumented member.
gcrsCampaignResponse :: Lens' GetCampaignResponse CampaignResponse
gcrsCampaignResponse = lens _gcrsCampaignResponse (\s a -> s {_gcrsCampaignResponse = a})

instance NFData GetCampaignResponse
