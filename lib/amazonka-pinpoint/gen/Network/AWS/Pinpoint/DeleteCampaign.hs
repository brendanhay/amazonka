{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dcrrsCampaignResponse,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCampaign' smart constructor.
data DeleteCampaign = DeleteCampaign'
  { -- | The unique identifier for the campaign.
    campaignId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCampaign' value with any optional fields omitted.
mkDeleteCampaign ::
  -- | 'campaignId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  DeleteCampaign
mkDeleteCampaign campaignId applicationId =
  DeleteCampaign' {campaignId, applicationId}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCampaignId :: Lens.Lens' DeleteCampaign Core.Text
dcCampaignId = Lens.field @"campaignId"
{-# DEPRECATED dcCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcApplicationId :: Lens.Lens' DeleteCampaign Core.Text
dcApplicationId = Lens.field @"applicationId"
{-# DEPRECATED dcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest DeleteCampaign where
  type Rs DeleteCampaign = DeleteCampaignResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
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
          DeleteCampaignResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
  { campaignResponse :: Types.CampaignResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCampaignResponse' value with any optional fields omitted.
mkDeleteCampaignResponse ::
  -- | 'campaignResponse'
  Types.CampaignResponse ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteCampaignResponse
mkDeleteCampaignResponse campaignResponse responseStatus =
  DeleteCampaignResponse' {campaignResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCampaignResponse :: Lens.Lens' DeleteCampaignResponse Types.CampaignResponse
dcrrsCampaignResponse = Lens.field @"campaignResponse"
{-# DEPRECATED dcrrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteCampaignResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
