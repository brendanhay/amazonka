{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteCampaign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a campaign from an application.
module Network.AWS.Pinpoint.DeleteCampaign
  ( -- * Creating a request
    DeleteCampaign (..),
    mkDeleteCampaign,

    -- ** Request lenses
    dcCampaignId,
    dcApplicationId,

    -- * Destructuring the response
    DeleteCampaignResponse (..),
    mkDeleteCampaignResponse,

    -- ** Response lenses
    dcrsResponseStatus,
    dcrsCampaignResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCampaign' smart constructor.
data DeleteCampaign = DeleteCampaign'
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

-- | Creates a value of 'DeleteCampaign' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'campaignId' - The unique identifier for the campaign.
mkDeleteCampaign ::
  -- | 'campaignId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  DeleteCampaign
mkDeleteCampaign pCampaignId_ pApplicationId_ =
  DeleteCampaign'
    { campaignId = pCampaignId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCampaignId :: Lens.Lens' DeleteCampaign Lude.Text
dcCampaignId = Lens.lens (campaignId :: DeleteCampaign -> Lude.Text) (\s a -> s {campaignId = a} :: DeleteCampaign)
{-# DEPRECATED dcCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcApplicationId :: Lens.Lens' DeleteCampaign Lude.Text
dcApplicationId = Lens.lens (applicationId :: DeleteCampaign -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteCampaign)
{-# DEPRECATED dcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteCampaign where
  type Rs DeleteCampaign = DeleteCampaignResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteCampaignResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteCampaign where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteCampaign where
  toPath DeleteCampaign' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/campaigns/",
        Lude.toBS campaignId
      ]

instance Lude.ToQuery DeleteCampaign where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
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

-- | Creates a value of 'DeleteCampaignResponse' with the minimum fields required to make a request.
--
-- * 'campaignResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteCampaignResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'campaignResponse'
  CampaignResponse ->
  DeleteCampaignResponse
mkDeleteCampaignResponse pResponseStatus_ pCampaignResponse_ =
  DeleteCampaignResponse'
    { responseStatus = pResponseStatus_,
      campaignResponse = pCampaignResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeleteCampaignResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeleteCampaignResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCampaignResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsCampaignResponse :: Lens.Lens' DeleteCampaignResponse CampaignResponse
dcrsCampaignResponse = Lens.lens (campaignResponse :: DeleteCampaignResponse -> CampaignResponse) (\s a -> s {campaignResponse = a} :: DeleteCampaignResponse)
{-# DEPRECATED dcrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}
