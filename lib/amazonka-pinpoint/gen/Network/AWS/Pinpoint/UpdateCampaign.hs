{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateCampaign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration and other settings for a campaign.
module Network.AWS.Pinpoint.UpdateCampaign
  ( -- * Creating a request
    UpdateCampaign (..),
    mkUpdateCampaign,

    -- ** Request lenses
    ucCampaignId,
    ucApplicationId,
    ucWriteCampaignRequest,

    -- * Destructuring the response
    UpdateCampaignResponse (..),
    mkUpdateCampaignResponse,

    -- ** Response lenses
    ucrsCampaignResponse,
    ucrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCampaign' smart constructor.
data UpdateCampaign = UpdateCampaign'
  { -- | The unique identifier for the campaign.
    campaignId :: Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    writeCampaignRequest :: WriteCampaignRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCampaign' with the minimum fields required to make a request.
--
-- * 'campaignId' - The unique identifier for the campaign.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'writeCampaignRequest' -
mkUpdateCampaign ::
  -- | 'campaignId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'writeCampaignRequest'
  WriteCampaignRequest ->
  UpdateCampaign
mkUpdateCampaign
  pCampaignId_
  pApplicationId_
  pWriteCampaignRequest_ =
    UpdateCampaign'
      { campaignId = pCampaignId_,
        applicationId = pApplicationId_,
        writeCampaignRequest = pWriteCampaignRequest_
      }

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCampaignId :: Lens.Lens' UpdateCampaign Lude.Text
ucCampaignId = Lens.lens (campaignId :: UpdateCampaign -> Lude.Text) (\s a -> s {campaignId = a} :: UpdateCampaign)
{-# DEPRECATED ucCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucApplicationId :: Lens.Lens' UpdateCampaign Lude.Text
ucApplicationId = Lens.lens (applicationId :: UpdateCampaign -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateCampaign)
{-# DEPRECATED ucApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeCampaignRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucWriteCampaignRequest :: Lens.Lens' UpdateCampaign WriteCampaignRequest
ucWriteCampaignRequest = Lens.lens (writeCampaignRequest :: UpdateCampaign -> WriteCampaignRequest) (\s a -> s {writeCampaignRequest = a} :: UpdateCampaign)
{-# DEPRECATED ucWriteCampaignRequest "Use generic-lens or generic-optics with 'writeCampaignRequest' instead." #-}

instance Lude.AWSRequest UpdateCampaign where
  type Rs UpdateCampaign = UpdateCampaignResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateCampaignResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCampaign where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCampaign where
  toJSON UpdateCampaign' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WriteCampaignRequest" Lude..= writeCampaignRequest)]
      )

instance Lude.ToPath UpdateCampaign where
  toPath UpdateCampaign' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/campaigns/",
        Lude.toBS campaignId
      ]

instance Lude.ToQuery UpdateCampaign where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCampaignResponse' smart constructor.
data UpdateCampaignResponse = UpdateCampaignResponse'
  { campaignResponse :: CampaignResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCampaignResponse' with the minimum fields required to make a request.
--
-- * 'campaignResponse' -
-- * 'responseStatus' - The response status code.
mkUpdateCampaignResponse ::
  -- | 'campaignResponse'
  CampaignResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCampaignResponse
mkUpdateCampaignResponse pCampaignResponse_ pResponseStatus_ =
  UpdateCampaignResponse'
    { campaignResponse = pCampaignResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsCampaignResponse :: Lens.Lens' UpdateCampaignResponse CampaignResponse
ucrsCampaignResponse = Lens.lens (campaignResponse :: UpdateCampaignResponse -> CampaignResponse) (\s a -> s {campaignResponse = a} :: UpdateCampaignResponse)
{-# DEPRECATED ucrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateCampaignResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateCampaignResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCampaignResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
