{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaignVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for all versions of a campaign.
module Network.AWS.Pinpoint.GetCampaignVersions
  ( -- * Creating a request
    GetCampaignVersions (..),
    mkGetCampaignVersions,

    -- ** Request lenses
    gcvToken,
    gcvCampaignId,
    gcvApplicationId,
    gcvPageSize,

    -- * Destructuring the response
    GetCampaignVersionsResponse (..),
    mkGetCampaignVersionsResponse,

    -- ** Response lenses
    gcvrsCampaignsResponse,
    gcvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCampaignVersions' smart constructor.
data GetCampaignVersions = GetCampaignVersions'
  { -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the campaign.
    campaignId :: Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaignVersions' with the minimum fields required to make a request.
--
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
-- * 'campaignId' - The unique identifier for the campaign.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkGetCampaignVersions ::
  -- | 'campaignId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetCampaignVersions
mkGetCampaignVersions pCampaignId_ pApplicationId_ =
  GetCampaignVersions'
    { token = Lude.Nothing,
      campaignId = pCampaignId_,
      applicationId = pApplicationId_,
      pageSize = Lude.Nothing
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvToken :: Lens.Lens' GetCampaignVersions (Lude.Maybe Lude.Text)
gcvToken = Lens.lens (token :: GetCampaignVersions -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetCampaignVersions)
{-# DEPRECATED gcvToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvCampaignId :: Lens.Lens' GetCampaignVersions Lude.Text
gcvCampaignId = Lens.lens (campaignId :: GetCampaignVersions -> Lude.Text) (\s a -> s {campaignId = a} :: GetCampaignVersions)
{-# DEPRECATED gcvCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvApplicationId :: Lens.Lens' GetCampaignVersions Lude.Text
gcvApplicationId = Lens.lens (applicationId :: GetCampaignVersions -> Lude.Text) (\s a -> s {applicationId = a} :: GetCampaignVersions)
{-# DEPRECATED gcvApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvPageSize :: Lens.Lens' GetCampaignVersions (Lude.Maybe Lude.Text)
gcvPageSize = Lens.lens (pageSize :: GetCampaignVersions -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetCampaignVersions)
{-# DEPRECATED gcvPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetCampaignVersions where
  type Rs GetCampaignVersions = GetCampaignVersionsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCampaignVersionsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCampaignVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCampaignVersions where
  toPath GetCampaignVersions' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/campaigns/",
        Lude.toBS campaignId,
        "/versions"
      ]

instance Lude.ToQuery GetCampaignVersions where
  toQuery GetCampaignVersions' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetCampaignVersionsResponse' smart constructor.
data GetCampaignVersionsResponse = GetCampaignVersionsResponse'
  { campaignsResponse :: CampaignsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaignVersionsResponse' with the minimum fields required to make a request.
--
-- * 'campaignsResponse' -
-- * 'responseStatus' - The response status code.
mkGetCampaignVersionsResponse ::
  -- | 'campaignsResponse'
  CampaignsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetCampaignVersionsResponse
mkGetCampaignVersionsResponse pCampaignsResponse_ pResponseStatus_ =
  GetCampaignVersionsResponse'
    { campaignsResponse =
        pCampaignsResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrsCampaignsResponse :: Lens.Lens' GetCampaignVersionsResponse CampaignsResponse
gcvrsCampaignsResponse = Lens.lens (campaignsResponse :: GetCampaignVersionsResponse -> CampaignsResponse) (\s a -> s {campaignsResponse = a} :: GetCampaignVersionsResponse)
{-# DEPRECATED gcvrsCampaignsResponse "Use generic-lens or generic-optics with 'campaignsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrsResponseStatus :: Lens.Lens' GetCampaignVersionsResponse Lude.Int
gcvrsResponseStatus = Lens.lens (responseStatus :: GetCampaignVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCampaignVersionsResponse)
{-# DEPRECATED gcvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
