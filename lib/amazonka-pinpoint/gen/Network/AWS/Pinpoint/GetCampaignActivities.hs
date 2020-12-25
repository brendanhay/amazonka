{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetCampaignActivities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the activities for a campaign.
module Network.AWS.Pinpoint.GetCampaignActivities
  ( -- * Creating a request
    GetCampaignActivities (..),
    mkGetCampaignActivities,

    -- ** Request lenses
    gcaApplicationId,
    gcaCampaignId,
    gcaPageSize,
    gcaToken,

    -- * Destructuring the response
    GetCampaignActivitiesResponse (..),
    mkGetCampaignActivitiesResponse,

    -- ** Response lenses
    gcarrsActivitiesResponse,
    gcarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCampaignActivities' smart constructor.
data GetCampaignActivities = GetCampaignActivities'
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

-- | Creates a 'GetCampaignActivities' value with any optional fields omitted.
mkGetCampaignActivities ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'campaignId'
  Core.Text ->
  GetCampaignActivities
mkGetCampaignActivities applicationId campaignId =
  GetCampaignActivities'
    { applicationId,
      campaignId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaApplicationId :: Lens.Lens' GetCampaignActivities Core.Text
gcaApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gcaApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the campaign.
--
-- /Note:/ Consider using 'campaignId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaCampaignId :: Lens.Lens' GetCampaignActivities Core.Text
gcaCampaignId = Lens.field @"campaignId"
{-# DEPRECATED gcaCampaignId "Use generic-lens or generic-optics with 'campaignId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaPageSize :: Lens.Lens' GetCampaignActivities (Core.Maybe Core.Text)
gcaPageSize = Lens.field @"pageSize"
{-# DEPRECATED gcaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaToken :: Lens.Lens' GetCampaignActivities (Core.Maybe Core.Text)
gcaToken = Lens.field @"token"
{-# DEPRECATED gcaToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetCampaignActivities where
  type Rs GetCampaignActivities = GetCampaignActivitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/campaigns/")
                Core.<> (Core.toText campaignId)
                Core.<> ("/activities")
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
          GetCampaignActivitiesResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCampaignActivitiesResponse' smart constructor.
data GetCampaignActivitiesResponse = GetCampaignActivitiesResponse'
  { activitiesResponse :: Types.ActivitiesResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCampaignActivitiesResponse' value with any optional fields omitted.
mkGetCampaignActivitiesResponse ::
  -- | 'activitiesResponse'
  Types.ActivitiesResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetCampaignActivitiesResponse
mkGetCampaignActivitiesResponse activitiesResponse responseStatus =
  GetCampaignActivitiesResponse'
    { activitiesResponse,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'activitiesResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcarrsActivitiesResponse :: Lens.Lens' GetCampaignActivitiesResponse Types.ActivitiesResponse
gcarrsActivitiesResponse = Lens.field @"activitiesResponse"
{-# DEPRECATED gcarrsActivitiesResponse "Use generic-lens or generic-optics with 'activitiesResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcarrsResponseStatus :: Lens.Lens' GetCampaignActivitiesResponse Core.Int
gcarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
