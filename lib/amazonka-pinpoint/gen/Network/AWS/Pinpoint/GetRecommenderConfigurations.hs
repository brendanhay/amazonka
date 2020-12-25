{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetRecommenderConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the recommender model configurations that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.GetRecommenderConfigurations
  ( -- * Creating a request
    GetRecommenderConfigurations (..),
    mkGetRecommenderConfigurations,

    -- ** Request lenses
    grcPageSize,
    grcToken,

    -- * Destructuring the response
    GetRecommenderConfigurationsResponse (..),
    mkGetRecommenderConfigurationsResponse,

    -- ** Response lenses
    grcrrsListRecommenderConfigurationsResponse,
    grcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRecommenderConfigurations' smart constructor.
data GetRecommenderConfigurations = GetRecommenderConfigurations'
  { -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRecommenderConfigurations' value with any optional fields omitted.
mkGetRecommenderConfigurations ::
  GetRecommenderConfigurations
mkGetRecommenderConfigurations =
  GetRecommenderConfigurations'
    { pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcPageSize :: Lens.Lens' GetRecommenderConfigurations (Core.Maybe Core.Text)
grcPageSize = Lens.field @"pageSize"
{-# DEPRECATED grcPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcToken :: Lens.Lens' GetRecommenderConfigurations (Core.Maybe Core.Text)
grcToken = Lens.field @"token"
{-# DEPRECATED grcToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetRecommenderConfigurations where
  type
    Rs GetRecommenderConfigurations =
      GetRecommenderConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/v1/recommenders",
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
          GetRecommenderConfigurationsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRecommenderConfigurationsResponse' smart constructor.
data GetRecommenderConfigurationsResponse = GetRecommenderConfigurationsResponse'
  { listRecommenderConfigurationsResponse :: Types.ListRecommenderConfigurationsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRecommenderConfigurationsResponse' value with any optional fields omitted.
mkGetRecommenderConfigurationsResponse ::
  -- | 'listRecommenderConfigurationsResponse'
  Types.ListRecommenderConfigurationsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetRecommenderConfigurationsResponse
mkGetRecommenderConfigurationsResponse
  listRecommenderConfigurationsResponse
  responseStatus =
    GetRecommenderConfigurationsResponse'
      { listRecommenderConfigurationsResponse,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'listRecommenderConfigurationsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsListRecommenderConfigurationsResponse :: Lens.Lens' GetRecommenderConfigurationsResponse Types.ListRecommenderConfigurationsResponse
grcrrsListRecommenderConfigurationsResponse = Lens.field @"listRecommenderConfigurationsResponse"
{-# DEPRECATED grcrrsListRecommenderConfigurationsResponse "Use generic-lens or generic-optics with 'listRecommenderConfigurationsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrrsResponseStatus :: Lens.Lens' GetRecommenderConfigurationsResponse Core.Int
grcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
