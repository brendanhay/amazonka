{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    GetCampaign (..),
    mkGetCampaign,

    -- ** Request lenses
    gcCampaignId,
    gcApplicationId,

    -- * Destructuring the response
    GetCampaignResponse (..),
    mkGetCampaignResponse,

    -- ** Response lenses
    gcrrsCampaignResponse,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCampaign' smart constructor.
data GetCampaign = GetCampaign'
  { -- | The unique identifier for the campaign.
    campaignId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaign' value with any optional fields omitted.
mkGetCampaign ::
  -- | 'campaignId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetCampaign
mkGetCampaign campaignId applicationId =
  GetCampaign' {campaignId, applicationId}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCampaignId :: Lens.Lens' GetCampaign Core.Text
gcCampaignId = Lens.field @"campaignId"
{-# DEPRECATED gcCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcApplicationId :: Lens.Lens' GetCampaign Core.Text
gcApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetCampaign where
  type Rs GetCampaign = GetCampaignResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/campaigns/")
                Core.<> (Core.toText campaignId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCampaignResponse' smart constructor.
data GetCampaignResponse = GetCampaignResponse'
  { campaignResponse :: Types.CampaignResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignResponse' value with any optional fields omitted.
mkGetCampaignResponse ::
  -- | 'campaignResponse'
  Types.CampaignResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetCampaignResponse
mkGetCampaignResponse campaignResponse responseStatus =
  GetCampaignResponse' {campaignResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCampaignResponse :: Lens.Lens' GetCampaignResponse Types.CampaignResponse
gcrrsCampaignResponse = Lens.field @"campaignResponse"
{-# DEPRECATED gcrrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCampaignResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
