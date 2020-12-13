{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateCampaign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new campaign for an application or updates the settings of an existing campaign for an application.
module Network.AWS.Pinpoint.CreateCampaign
  ( -- * Creating a request
    CreateCampaign (..),
    mkCreateCampaign,

    -- ** Request lenses
    ccApplicationId,
    ccWriteCampaignRequest,

    -- * Destructuring the response
    CreateCampaignResponse (..),
    mkCreateCampaignResponse,

    -- ** Response lenses
    ccrsCampaignResponse,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCampaign' smart constructor.
data CreateCampaign = CreateCampaign'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    writeCampaignRequest :: WriteCampaignRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCampaign' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'writeCampaignRequest' -
mkCreateCampaign ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'writeCampaignRequest'
  WriteCampaignRequest ->
  CreateCampaign
mkCreateCampaign pApplicationId_ pWriteCampaignRequest_ =
  CreateCampaign'
    { applicationId = pApplicationId_,
      writeCampaignRequest = pWriteCampaignRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccApplicationId :: Lens.Lens' CreateCampaign Lude.Text
ccApplicationId = Lens.lens (applicationId :: CreateCampaign -> Lude.Text) (\s a -> s {applicationId = a} :: CreateCampaign)
{-# DEPRECATED ccApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeCampaignRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccWriteCampaignRequest :: Lens.Lens' CreateCampaign WriteCampaignRequest
ccWriteCampaignRequest = Lens.lens (writeCampaignRequest :: CreateCampaign -> WriteCampaignRequest) (\s a -> s {writeCampaignRequest = a} :: CreateCampaign)
{-# DEPRECATED ccWriteCampaignRequest "Use generic-lens or generic-optics with 'writeCampaignRequest' instead." #-}

instance Lude.AWSRequest CreateCampaign where
  type Rs CreateCampaign = CreateCampaignResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCampaignResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCampaign where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCampaign where
  toJSON CreateCampaign' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WriteCampaignRequest" Lude..= writeCampaignRequest)]
      )

instance Lude.ToPath CreateCampaign where
  toPath CreateCampaign' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/campaigns"]

instance Lude.ToQuery CreateCampaign where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCampaignResponse' smart constructor.
data CreateCampaignResponse = CreateCampaignResponse'
  { campaignResponse :: CampaignResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCampaignResponse' with the minimum fields required to make a request.
--
-- * 'campaignResponse' -
-- * 'responseStatus' - The response status code.
mkCreateCampaignResponse ::
  -- | 'campaignResponse'
  CampaignResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateCampaignResponse
mkCreateCampaignResponse pCampaignResponse_ pResponseStatus_ =
  CreateCampaignResponse'
    { campaignResponse = pCampaignResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCampaignResponse :: Lens.Lens' CreateCampaignResponse CampaignResponse
ccrsCampaignResponse = Lens.lens (campaignResponse :: CreateCampaignResponse -> CampaignResponse) (\s a -> s {campaignResponse = a} :: CreateCampaignResponse)
{-# DEPRECATED ccrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateCampaignResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateCampaignResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCampaignResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
