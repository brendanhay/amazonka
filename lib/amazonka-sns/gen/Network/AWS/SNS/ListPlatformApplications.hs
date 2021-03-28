{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform application objects for the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging). The results for @ListPlatformApplications@ are paginated and return a limited list of applications, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call @ListPlatformApplications@ using the NextToken string received from the previous call. When there are no more records to return, @NextToken@ will be null. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> . 
--
-- This action is throttled at 15 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListPlatformApplications
    (
    -- * Creating a request
      ListPlatformApplications (..)
    , mkListPlatformApplications
    -- ** Request lenses
    , lpaNextToken

    -- * Destructuring the response
    , ListPlatformApplicationsResponse (..)
    , mkListPlatformApplicationsResponse
    -- ** Response lenses
    , lparrsNextToken
    , lparrsPlatformApplications
    , lparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for ListPlatformApplications action.
--
-- /See:/ 'mkListPlatformApplications' smart constructor.
newtype ListPlatformApplications = ListPlatformApplications'
  { nextToken :: Core.Maybe Core.Text
    -- ^ NextToken string is used when calling ListPlatformApplications action to retrieve additional records that are available after the first page results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformApplications' value with any optional fields omitted.
mkListPlatformApplications
    :: ListPlatformApplications
mkListPlatformApplications
  = ListPlatformApplications'{nextToken = Core.Nothing}

-- | NextToken string is used when calling ListPlatformApplications action to retrieve additional records that are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaNextToken :: Lens.Lens' ListPlatformApplications (Core.Maybe Core.Text)
lpaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPlatformApplications where
        toQuery ListPlatformApplications{..}
          = Core.toQueryPair "Action"
              ("ListPlatformApplications" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-03-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListPlatformApplications where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPlatformApplications where
        type Rs ListPlatformApplications = ListPlatformApplicationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListPlatformApplicationsResult"
              (\ s h x ->
                 ListPlatformApplicationsResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "PlatformApplications" Core..<@>
                       Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPlatformApplications where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"platformApplications" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Response for ListPlatformApplications action.
--
-- /See:/ 'mkListPlatformApplicationsResponse' smart constructor.
data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ NextToken string is returned when calling ListPlatformApplications action if additional records are available after the first page results.
  , platformApplications :: Core.Maybe [Types.PlatformApplication]
    -- ^ Platform applications returned when calling ListPlatformApplications action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformApplicationsResponse' value with any optional fields omitted.
mkListPlatformApplicationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPlatformApplicationsResponse
mkListPlatformApplicationsResponse responseStatus
  = ListPlatformApplicationsResponse'{nextToken = Core.Nothing,
                                      platformApplications = Core.Nothing, responseStatus}

-- | NextToken string is returned when calling ListPlatformApplications action if additional records are available after the first page results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsNextToken :: Lens.Lens' ListPlatformApplicationsResponse (Core.Maybe Core.Text)
lparrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lparrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Platform applications returned when calling ListPlatformApplications action.
--
-- /Note:/ Consider using 'platformApplications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsPlatformApplications :: Lens.Lens' ListPlatformApplicationsResponse (Core.Maybe [Types.PlatformApplication])
lparrsPlatformApplications = Lens.field @"platformApplications"
{-# INLINEABLE lparrsPlatformApplications #-}
{-# DEPRECATED platformApplications "Use generic-lens or generic-optics with 'platformApplications' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsResponseStatus :: Lens.Lens' ListPlatformApplicationsResponse Core.Int
lparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
