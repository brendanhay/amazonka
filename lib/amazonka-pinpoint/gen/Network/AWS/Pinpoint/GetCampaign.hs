{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    GetCampaign (..),
    mkGetCampaign,

    -- ** Request lenses
    getCampaignId,
    getApplicationId,

    -- * Destructuring the response
    GetCampaignResponse (..),
    mkGetCampaignResponse,

    -- ** Response lenses
    gcrsResponseStatus,
    gcrsCampaignResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCampaign' smart constructor.
data GetCampaign = GetCampaign'
  { campaignId :: Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCampaign' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'campaignId' - The unique identifier for the campaign.
mkGetCampaign ::
  -- | 'campaignId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetCampaign
mkGetCampaign pCampaignId_ pApplicationId_ =
  GetCampaign'
    { campaignId = pCampaignId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getCampaignId :: Lens.Lens' GetCampaign Lude.Text
getCampaignId = Lens.lens (campaignId :: GetCampaign -> Lude.Text) (\s a -> s {campaignId = a} :: GetCampaign)
{-# DEPRECATED getCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getApplicationId :: Lens.Lens' GetCampaign Lude.Text
getApplicationId = Lens.lens (applicationId :: GetCampaign -> Lude.Text) (\s a -> s {applicationId = a} :: GetCampaign)
{-# DEPRECATED getApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetCampaign where
  type Rs GetCampaign = GetCampaignResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCampaignResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetCampaign where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetCampaign where
  toPath GetCampaign' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/campaigns/",
        Lude.toBS campaignId
      ]

instance Lude.ToQuery GetCampaign where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCampaignResponse' smart constructor.
data GetCampaignResponse = GetCampaignResponse'
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

-- | Creates a value of 'GetCampaignResponse' with the minimum fields required to make a request.
--
-- * 'campaignResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetCampaignResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  GetCampaignResponse
mkGetCampaignResponse pResponseStatus_ pCampaignResponse_ =
  GetCampaignResponse'
    { responseStatus = pResponseStatus_,
      campaignResponse = pCampaignResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetCampaignResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetCampaignResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCampaignResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsCampaignResponse :: Lens.Lens' GetCampaignResponse CampaignResponse
gcrsCampaignResponse = Lens.lens (campaignResponse :: GetCampaignResponse -> CampaignResponse) (\s a -> s {campaignResponse = a} :: GetCampaignResponse)
{-# DEPRECATED gcrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}
