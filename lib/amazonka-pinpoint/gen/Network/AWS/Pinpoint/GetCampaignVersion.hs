{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaignVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for a specific version of a campaign.
module Network.AWS.Pinpoint.GetCampaignVersion
  ( -- * Creating a request
    GetCampaignVersion (..),
    mkGetCampaignVersion,

    -- ** Request lenses
    gCampaignId,
    gApplicationId,
    gVersion,

    -- * Destructuring the response
    GetCampaignVersionResponse (..),
    mkGetCampaignVersionResponse,

    -- ** Response lenses
    gcvfrsCampaignResponse,
    gcvfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCampaignVersion' smart constructor.
data GetCampaignVersion = GetCampaignVersion'
  { -- | The unique identifier for the campaign.
    campaignId :: Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The unique version number (Version property) for the campaign version.
    version :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaignVersion' with the minimum fields required to make a request.
--
-- * 'campaignId' - The unique identifier for the campaign.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'version' - The unique version number (Version property) for the campaign version.
mkGetCampaignVersion ::
  -- | 'campaignId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  GetCampaignVersion
mkGetCampaignVersion pCampaignId_ pApplicationId_ pVersion_ =
  GetCampaignVersion'
    { campaignId = pCampaignId_,
      applicationId = pApplicationId_,
      version = pVersion_
    }

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gCampaignId :: Lens.Lens' GetCampaignVersion Lude.Text
gCampaignId = Lens.lens (campaignId :: GetCampaignVersion -> Lude.Text) (\s a -> s {campaignId = a} :: GetCampaignVersion)
{-# DEPRECATED gCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gApplicationId :: Lens.Lens' GetCampaignVersion Lude.Text
gApplicationId = Lens.lens (applicationId :: GetCampaignVersion -> Lude.Text) (\s a -> s {applicationId = a} :: GetCampaignVersion)
{-# DEPRECATED gApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique version number (Version property) for the campaign version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gVersion :: Lens.Lens' GetCampaignVersion Lude.Text
gVersion = Lens.lens (version :: GetCampaignVersion -> Lude.Text) (\s a -> s {version = a} :: GetCampaignVersion)
{-# DEPRECATED gVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest GetCampaignVersion where
  type Rs GetCampaignVersion = GetCampaignVersionResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCampaignVersionResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCampaignVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCampaignVersion where
  toPath GetCampaignVersion' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/campaigns/",
        Lude.toBS campaignId,
        "/versions/",
        Lude.toBS version
      ]

instance Lude.ToQuery GetCampaignVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCampaignVersionResponse' smart constructor.
data GetCampaignVersionResponse = GetCampaignVersionResponse'
  { campaignResponse :: CampaignResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaignVersionResponse' with the minimum fields required to make a request.
--
-- * 'campaignResponse' -
-- * 'responseStatus' - The response status code.
mkGetCampaignVersionResponse ::
  -- | 'campaignResponse'
  CampaignResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetCampaignVersionResponse
mkGetCampaignVersionResponse pCampaignResponse_ pResponseStatus_ =
  GetCampaignVersionResponse'
    { campaignResponse =
        pCampaignResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfrsCampaignResponse :: Lens.Lens' GetCampaignVersionResponse CampaignResponse
gcvfrsCampaignResponse = Lens.lens (campaignResponse :: GetCampaignVersionResponse -> CampaignResponse) (\s a -> s {campaignResponse = a} :: GetCampaignVersionResponse)
{-# DEPRECATED gcvfrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfrsResponseStatus :: Lens.Lens' GetCampaignVersionResponse Lude.Int
gcvfrsResponseStatus = Lens.lens (responseStatus :: GetCampaignVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCampaignVersionResponse)
{-# DEPRECATED gcvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
