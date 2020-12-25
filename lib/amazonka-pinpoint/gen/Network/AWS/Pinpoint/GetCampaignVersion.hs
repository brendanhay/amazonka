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
    gcvfVersion,
    gcvfApplicationId,
    gcvfCampaignId,

    -- * Destructuring the response
    GetCampaignVersionResponse (..),
    mkGetCampaignVersionResponse,

    -- ** Response lenses
    gcvrfrsCampaignResponse,
    gcvrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCampaignVersion' smart constructor.
data GetCampaignVersion = GetCampaignVersion'
  { -- | The unique version number (Version property) for the campaign version.
    version :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the campaign.
    campaignId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignVersion' value with any optional fields omitted.
mkGetCampaignVersion ::
  -- | 'version'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  -- | 'campaignId'
  Core.Text ->
  GetCampaignVersion
mkGetCampaignVersion version applicationId campaignId =
  GetCampaignVersion' {version, applicationId, campaignId}

-- | The unique version number (Version property) for the campaign version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfVersion :: Lens.Lens' GetCampaignVersion Core.Text
gcvfVersion = Lens.field @"version"
{-# DEPRECATED gcvfVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfApplicationId :: Lens.Lens' GetCampaignVersion Core.Text
gcvfApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcvfApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvfCampaignId :: Lens.Lens' GetCampaignVersion Core.Text
gcvfCampaignId = Lens.field @"campaignId"
{-# DEPRECATED gcvfCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

instance Core.AWSRequest GetCampaignVersion where
  type Rs GetCampaignVersion = GetCampaignVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/campaigns/")
                Core.<> (Core.toText campaignId)
                Core.<> ("/versions/")
                Core.<> (Core.toText version)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignVersionResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCampaignVersionResponse' smart constructor.
data GetCampaignVersionResponse = GetCampaignVersionResponse'
  { campaignResponse :: Types.CampaignResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignVersionResponse' value with any optional fields omitted.
mkGetCampaignVersionResponse ::
  -- | 'campaignResponse'
  Types.CampaignResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetCampaignVersionResponse
mkGetCampaignVersionResponse campaignResponse responseStatus =
  GetCampaignVersionResponse' {campaignResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrfrsCampaignResponse :: Lens.Lens' GetCampaignVersionResponse Types.CampaignResponse
gcvrfrsCampaignResponse = Lens.field @"campaignResponse"
{-# DEPRECATED gcvrfrsCampaignResponse "Use generic-lens or generic-optics with 'campaignResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrfrsResponseStatus :: Lens.Lens' GetCampaignVersionResponse Core.Int
gcvrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcvrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
