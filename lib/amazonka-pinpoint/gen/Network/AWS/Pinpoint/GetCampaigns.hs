{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    GetCampaigns (..),
    mkGetCampaigns,

    -- ** Request lenses
    gcsToken,
    gcsApplicationId,
    gcsPageSize,

    -- * Destructuring the response
    GetCampaignsResponse (..),
    mkGetCampaignsResponse,

    -- ** Response lenses
    gcfrsCampaignsResponse,
    gcfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'
  { -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaigns' with the minimum fields required to make a request.
--
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkGetCampaigns ::
  -- | 'applicationId'
  Lude.Text ->
  GetCampaigns
mkGetCampaigns pApplicationId_ =
  GetCampaigns'
    { token = Lude.Nothing,
      applicationId = pApplicationId_,
      pageSize = Lude.Nothing
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsToken :: Lens.Lens' GetCampaigns (Lude.Maybe Lude.Text)
gcsToken = Lens.lens (token :: GetCampaigns -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetCampaigns)
{-# DEPRECATED gcsToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsApplicationId :: Lens.Lens' GetCampaigns Lude.Text
gcsApplicationId = Lens.lens (applicationId :: GetCampaigns -> Lude.Text) (\s a -> s {applicationId = a} :: GetCampaigns)
{-# DEPRECATED gcsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsPageSize :: Lens.Lens' GetCampaigns (Lude.Maybe Lude.Text)
gcsPageSize = Lens.lens (pageSize :: GetCampaigns -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetCampaigns)
{-# DEPRECATED gcsPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetCampaigns where
  type Rs GetCampaigns = GetCampaignsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCampaignsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCampaigns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCampaigns where
  toPath GetCampaigns' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/campaigns"]

instance Lude.ToQuery GetCampaigns where
  toQuery GetCampaigns' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'
  { campaignsResponse :: CampaignsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaignsResponse' with the minimum fields required to make a request.
--
-- * 'campaignsResponse' -
-- * 'responseStatus' - The response status code.
mkGetCampaignsResponse ::
  -- | 'campaignsResponse'
  CampaignsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetCampaignsResponse
mkGetCampaignsResponse pCampaignsResponse_ pResponseStatus_ =
  GetCampaignsResponse'
    { campaignsResponse = pCampaignsResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrsCampaignsResponse :: Lens.Lens' GetCampaignsResponse CampaignsResponse
gcfrsCampaignsResponse = Lens.lens (campaignsResponse :: GetCampaignsResponse -> CampaignsResponse) (\s a -> s {campaignsResponse = a} :: GetCampaignsResponse)
{-# DEPRECATED gcfrsCampaignsResponse "Use generic-lens or generic-optics with 'campaignsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfrsResponseStatus :: Lens.Lens' GetCampaignsResponse Lude.Int
gcfrsResponseStatus = Lens.lens (responseStatus :: GetCampaignsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCampaignsResponse)
{-# DEPRECATED gcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
