{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaignVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for all versions of a campaign.
module Network.AWS.Pinpoint.GetCampaignVersions
  ( -- * Creating a request
    GetCampaignVersions (..),
    mkGetCampaignVersions,

    -- ** Request lenses
    gcvApplicationId,
    gcvCampaignId,
    gcvPageSize,
    gcvToken,

    -- * Destructuring the response
    GetCampaignVersionsResponse (..),
    mkGetCampaignVersionsResponse,

    -- ** Response lenses
    gcvrrsCampaignsResponse,
    gcvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCampaignVersions' smart constructor.
data GetCampaignVersions = GetCampaignVersions'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the campaign.
    campaignId :: Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignVersions' value with any optional fields omitted.
mkGetCampaignVersions ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'campaignId'
  Core.Text ->
  GetCampaignVersions
mkGetCampaignVersions applicationId campaignId =
  GetCampaignVersions'
    { applicationId,
      campaignId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvApplicationId :: Lens.Lens' GetCampaignVersions Core.Text
gcvApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcvApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvCampaignId :: Lens.Lens' GetCampaignVersions Core.Text
gcvCampaignId = Lens.field @"campaignId"
{-# DEPRECATED gcvCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvPageSize :: Lens.Lens' GetCampaignVersions (Core.Maybe Core.Text)
gcvPageSize = Lens.field @"pageSize"
{-# DEPRECATED gcvPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvToken :: Lens.Lens' GetCampaignVersions (Core.Maybe Core.Text)
gcvToken = Lens.field @"token"
{-# DEPRECATED gcvToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetCampaignVersions where
  type Rs GetCampaignVersions = GetCampaignVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/campaigns/")
                Core.<> (Core.toText campaignId)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "page-size" Core.<$> pageSize
            Core.<> (Core.toQueryValue "token" Core.<$> token),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignVersionsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCampaignVersionsResponse' smart constructor.
data GetCampaignVersionsResponse = GetCampaignVersionsResponse'
  { campaignsResponse :: Types.CampaignsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignVersionsResponse' value with any optional fields omitted.
mkGetCampaignVersionsResponse ::
  -- | 'campaignsResponse'
  Types.CampaignsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetCampaignVersionsResponse
mkGetCampaignVersionsResponse campaignsResponse responseStatus =
  GetCampaignVersionsResponse' {campaignsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrrsCampaignsResponse :: Lens.Lens' GetCampaignVersionsResponse Types.CampaignsResponse
gcvrrsCampaignsResponse = Lens.field @"campaignsResponse"
{-# DEPRECATED gcvrrsCampaignsResponse "Use generic-lens or generic-optics with 'campaignsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcvrrsResponseStatus :: Lens.Lens' GetCampaignVersionsResponse Core.Int
gcvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
