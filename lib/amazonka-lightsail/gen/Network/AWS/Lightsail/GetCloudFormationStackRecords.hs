{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetCloudFormationStackRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the CloudFormation stack record created as a result of the @create cloud formation stack@ operation.
--
-- An AWS CloudFormation stack is used to create a new Amazon EC2 instance from an exported Lightsail snapshot.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetCloudFormationStackRecords
    (
    -- * Creating a request
      GetCloudFormationStackRecords (..)
    , mkGetCloudFormationStackRecords
    -- ** Request lenses
    , gcfsrPageToken

    -- * Destructuring the response
    , GetCloudFormationStackRecordsResponse (..)
    , mkGetCloudFormationStackRecordsResponse
    -- ** Response lenses
    , gcfsrrrsCloudFormationStackRecords
    , gcfsrrrsNextPageToken
    , gcfsrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCloudFormationStackRecords' smart constructor.
newtype GetCloudFormationStackRecords = GetCloudFormationStackRecords'
  { pageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetClouFormationStackRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCloudFormationStackRecords' value with any optional fields omitted.
mkGetCloudFormationStackRecords
    :: GetCloudFormationStackRecords
mkGetCloudFormationStackRecords
  = GetCloudFormationStackRecords'{pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetClouFormationStackRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrPageToken :: Lens.Lens' GetCloudFormationStackRecords (Core.Maybe Core.Text)
gcfsrPageToken = Lens.field @"pageToken"
{-# INLINEABLE gcfsrPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery GetCloudFormationStackRecords where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCloudFormationStackRecords where
        toHeaders GetCloudFormationStackRecords{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.GetCloudFormationStackRecords")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCloudFormationStackRecords where
        toJSON GetCloudFormationStackRecords{..}
          = Core.object
              (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetCloudFormationStackRecords where
        type Rs GetCloudFormationStackRecords =
             GetCloudFormationStackRecordsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCloudFormationStackRecordsResponse' Core.<$>
                   (x Core..:? "cloudFormationStackRecords") Core.<*>
                     x Core..:? "nextPageToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetCloudFormationStackRecords where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"cloudFormationStackRecords" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkGetCloudFormationStackRecordsResponse' smart constructor.
data GetCloudFormationStackRecordsResponse = GetCloudFormationStackRecordsResponse'
  { cloudFormationStackRecords :: Core.Maybe [Types.CloudFormationStackRecord]
    -- ^ A list of objects describing the CloudFormation stack records.
  , nextPageToken :: Core.Maybe Core.Text
    -- ^ The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetCloudFormationStackRecords@ request and specify the next page token using the @pageToken@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCloudFormationStackRecordsResponse' value with any optional fields omitted.
mkGetCloudFormationStackRecordsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCloudFormationStackRecordsResponse
mkGetCloudFormationStackRecordsResponse responseStatus
  = GetCloudFormationStackRecordsResponse'{cloudFormationStackRecords
                                             = Core.Nothing,
                                           nextPageToken = Core.Nothing, responseStatus}

-- | A list of objects describing the CloudFormation stack records.
--
-- /Note:/ Consider using 'cloudFormationStackRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrrrsCloudFormationStackRecords :: Lens.Lens' GetCloudFormationStackRecordsResponse (Core.Maybe [Types.CloudFormationStackRecord])
gcfsrrrsCloudFormationStackRecords = Lens.field @"cloudFormationStackRecords"
{-# INLINEABLE gcfsrrrsCloudFormationStackRecords #-}
{-# DEPRECATED cloudFormationStackRecords "Use generic-lens or generic-optics with 'cloudFormationStackRecords' instead"  #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetCloudFormationStackRecords@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrrrsNextPageToken :: Lens.Lens' GetCloudFormationStackRecordsResponse (Core.Maybe Core.Text)
gcfsrrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gcfsrrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfsrrrsResponseStatus :: Lens.Lens' GetCloudFormationStackRecordsResponse Core.Int
gcfsrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcfsrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
