{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the applications that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.GetApps
    (
    -- * Creating a request
      GetApps (..)
    , mkGetApps
    -- ** Request lenses
    , gaPageSize
    , gaToken

    -- * Destructuring the response
    , GetAppsResponse (..)
    , mkGetAppsResponse
    -- ** Response lenses
    , garfrsApplicationsResponse
    , garfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApps' smart constructor.
data GetApps = GetApps'
  { pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , token :: Core.Maybe Core.Text
    -- ^ The NextToken string that specifies which page of results to return in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApps' value with any optional fields omitted.
mkGetApps
    :: GetApps
mkGetApps = GetApps'{pageSize = Core.Nothing, token = Core.Nothing}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaPageSize :: Lens.Lens' GetApps (Core.Maybe Core.Text)
gaPageSize = Lens.field @"pageSize"
{-# INLINEABLE gaPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaToken :: Lens.Lens' GetApps (Core.Maybe Core.Text)
gaToken = Lens.field @"token"
{-# INLINEABLE gaToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery GetApps where
        toQuery GetApps{..}
          = Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "token") token

instance Core.ToHeaders GetApps where
        toHeaders GetApps{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApps where
        type Rs GetApps = GetAppsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/v1/apps",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAppsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAppsResponse' smart constructor.
data GetAppsResponse = GetAppsResponse'
  { applicationsResponse :: Types.ApplicationsResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppsResponse' value with any optional fields omitted.
mkGetAppsResponse
    :: Types.ApplicationsResponse -- ^ 'applicationsResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetAppsResponse
mkGetAppsResponse applicationsResponse responseStatus
  = GetAppsResponse'{applicationsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garfrsApplicationsResponse :: Lens.Lens' GetAppsResponse Types.ApplicationsResponse
garfrsApplicationsResponse = Lens.field @"applicationsResponse"
{-# INLINEABLE garfrsApplicationsResponse #-}
{-# DEPRECATED applicationsResponse "Use generic-lens or generic-optics with 'applicationsResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garfrsResponseStatus :: Lens.Lens' GetAppsResponse Core.Int
garfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
