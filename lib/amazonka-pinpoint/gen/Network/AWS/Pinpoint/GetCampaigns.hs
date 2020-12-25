{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaigns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for all the campaigns that are associated with an application.
module Network.AWS.Pinpoint.GetCampaigns
  ( -- * Creating a request
    GetCampaigns (..),
    mkGetCampaigns,

    -- ** Request lenses
    gcfApplicationId,
    gcfPageSize,
    gcfToken,

    -- * Destructuring the response
    GetCampaignsResponse (..),
    mkGetCampaignsResponse,

    -- ** Response lenses
    gcrgrsCampaignsResponse,
    gcrgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaigns' value with any optional fields omitted.
mkGetCampaigns ::
  -- | 'applicationId'
  Core.Text ->
  GetCampaigns
mkGetCampaigns applicationId =
  GetCampaigns'
    { applicationId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfApplicationId :: Lens.Lens' GetCampaigns Core.Text
gcfApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcfApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfPageSize :: Lens.Lens' GetCampaigns (Core.Maybe Core.Text)
gcfPageSize = Lens.field @"pageSize"
{-# DEPRECATED gcfPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfToken :: Lens.Lens' GetCampaigns (Core.Maybe Core.Text)
gcfToken = Lens.field @"token"
{-# DEPRECATED gcfToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetCampaigns where
  type Rs GetCampaigns = GetCampaignsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/campaigns")
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
          GetCampaignsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'
  { campaignsResponse :: Types.CampaignsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignsResponse' value with any optional fields omitted.
mkGetCampaignsResponse ::
  -- | 'campaignsResponse'
  Types.CampaignsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetCampaignsResponse
mkGetCampaignsResponse campaignsResponse responseStatus =
  GetCampaignsResponse' {campaignsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'campaignsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrgrsCampaignsResponse :: Lens.Lens' GetCampaignsResponse Types.CampaignsResponse
gcrgrsCampaignsResponse = Lens.field @"campaignsResponse"
{-# DEPRECATED gcrgrsCampaignsResponse "Use generic-lens or generic-optics with 'campaignsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrgrsResponseStatus :: Lens.Lens' GetCampaignsResponse Core.Int
gcrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
