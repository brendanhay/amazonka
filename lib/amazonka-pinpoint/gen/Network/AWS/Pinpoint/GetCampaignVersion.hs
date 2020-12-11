{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gcvcVersion,
    gcvcApplicationId,
    gcvcCampaignId,

    -- * Destructuring the response
    GetCampaignVersionResponse (..),
    mkGetCampaignVersionResponse,

    -- ** Response lenses
    gcvcrsResponseStatus,
    gcvcrsCampaignResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCampaignVersion' smart constructor.
data GetCampaignVersion = GetCampaignVersion'
  { version :: Lude.Text,
    applicationId :: Lude.Text,
    campaignId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaignVersion' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'campaignId' - The unique identifier for the campaign.
-- * 'version' - The unique version number (Version property) for the campaign version.
mkGetCampaignVersion ::
  -- | 'version'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'campaignId'
  Lude.Text ->
  GetCampaignVersion
mkGetCampaignVersion pVersion_ pApplicationId_ pCampaignId_ =
  GetCampaignVersion'
    { version = pVersion_,
      applicationId = pApplicationId_,
      campaignId = pCampaignId_
    }

-- | The unique version number (Version property) for the campaign version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvcVersion :: Lens.Lens' GetCampaignVersion Lude.Text
gcvcVersion = Lens.lens (version :: GetCampaignVersion -> Lude.Text) (\s a -> s {version = a} :: GetCampaignVersion)
{-# DEPRECATED gcvcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvcApplicationId :: Lens.Lens' GetCampaignVersion Lude.Text
gcvcApplicationId = Lens.lens (applicationId :: GetCampaignVersion -> Lude.Text) (\s a -> s {applicationId = a} :: GetCampaignVersion)
{-# DEPRECATED gcvcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvcCampaignId :: Lens.Lens' GetCampaignVersion Lude.Text
gcvcCampaignId = Lens.lens (campaignId :: GetCampaignVersion -> Lude.Text) (\s a -> s {campaignId = a} :: GetCampaignVersion)
{-# DEPRECATED gcvcCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

instance Lude.AWSRequest GetCampaignVersion where
  type Rs GetCampaignVersion = GetCampaignVersionResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCampaignVersionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
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
  { responseStatus ::
      Lude.Int,
    campaignResponse :: CampaignResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaignVersionResponse' with the minimum fields required to make a request.
--
-- * 'campaignResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetCampaignVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  GetCampaignVersionResponse
mkGetCampaignVersionResponse pResponseStatus_ pCampaignResponse_ =
  GetCampaignVersionResponse'
    { responseStatus = pResponseStatus_,
      campaignResponse = pCampaignResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvcrsResponseStatus :: Lens.Lens' GetCampaignVersionResponse Lude.Int
gcvcrsResponseStatus = Lens.lens (responseStatus :: GetCampaignVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCampaignVersionResponse)
{-# DEPRECATED gcvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvcrsCampaignResponse :: Lens.Lens' GetCampaignVersionResponse CampaignResponse
gcvcrsCampaignResponse = Lens.lens (campaignResponse :: GetCampaignVersionResponse -> CampaignResponse) (\s a -> s {campaignResponse = a} :: GetCampaignVersionResponse)
{-# DEPRECATED gcvcrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}
