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
-- Module      : Network.AWS.Pinpoint.GetCampaigns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for all the campaigns that are associated with an application.
module Network.AWS.Pinpoint.GetCampaigns
  ( -- * Creating a Request
    getCampaigns,
    GetCampaigns,

    -- * Request Lenses
    gccToken,
    gccPageSize,
    gccApplicationId,

    -- * Destructuring the Response
    getCampaignsResponse,
    GetCampaignsResponse,

    -- * Response Lenses
    gccrsResponseStatus,
    gccrsCampaignsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'
  { _gccToken :: !(Maybe Text),
    _gccPageSize :: !(Maybe Text),
    _gccApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaigns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'gccPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gccApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getCampaigns ::
  -- | 'gccApplicationId'
  Text ->
  GetCampaigns
getCampaigns pApplicationId_ =
  GetCampaigns'
    { _gccToken = Nothing,
      _gccPageSize = Nothing,
      _gccApplicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
gccToken :: Lens' GetCampaigns (Maybe Text)
gccToken = lens _gccToken (\s a -> s {_gccToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gccPageSize :: Lens' GetCampaigns (Maybe Text)
gccPageSize = lens _gccPageSize (\s a -> s {_gccPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gccApplicationId :: Lens' GetCampaigns Text
gccApplicationId = lens _gccApplicationId (\s a -> s {_gccApplicationId = a})

instance AWSRequest GetCampaigns where
  type Rs GetCampaigns = GetCampaignsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetCampaignsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetCampaigns

instance NFData GetCampaigns

instance ToHeaders GetCampaigns where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetCampaigns where
  toPath GetCampaigns' {..} =
    mconcat ["/v1/apps/", toBS _gccApplicationId, "/campaigns"]

instance ToQuery GetCampaigns where
  toQuery GetCampaigns' {..} =
    mconcat ["token" =: _gccToken, "page-size" =: _gccPageSize]

-- | /See:/ 'getCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'
  { _gccrsResponseStatus ::
      !Int,
    _gccrsCampaignsResponse :: !CampaignsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccrsResponseStatus' - -- | The response status code.
--
-- * 'gccrsCampaignsResponse' - Undocumented member.
getCampaignsResponse ::
  -- | 'gccrsResponseStatus'
  Int ->
  -- | 'gccrsCampaignsResponse'
  CampaignsResponse ->
  GetCampaignsResponse
getCampaignsResponse pResponseStatus_ pCampaignsResponse_ =
  GetCampaignsResponse'
    { _gccrsResponseStatus = pResponseStatus_,
      _gccrsCampaignsResponse = pCampaignsResponse_
    }

-- | -- | The response status code.
gccrsResponseStatus :: Lens' GetCampaignsResponse Int
gccrsResponseStatus = lens _gccrsResponseStatus (\s a -> s {_gccrsResponseStatus = a})

-- | Undocumented member.
gccrsCampaignsResponse :: Lens' GetCampaignsResponse CampaignsResponse
gccrsCampaignsResponse = lens _gccrsCampaignsResponse (\s a -> s {_gccrsCampaignsResponse = a})

instance NFData GetCampaignsResponse
