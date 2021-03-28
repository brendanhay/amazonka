{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.StopLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends the recording of AWS API calls and log file delivery for the specified trail. Under most circumstances, there is no need to use this action. You can update a trail without stopping it first. This action is the only way to stop recording. For a trail enabled in all regions, this operation must be called from the region in which the trail was created, or an @InvalidHomeRegionException@ will occur. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail enabled in all regions.
module Network.AWS.CloudTrail.StopLogging
    (
    -- * Creating a request
      StopLogging (..)
    , mkStopLogging
    -- ** Request lenses
    , slName

    -- * Destructuring the response
    , StopLoggingResponse (..)
    , mkStopLoggingResponse
    -- ** Response lenses
    , slrrsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Passes the request to CloudTrail to stop logging AWS API calls for the specified account.
--
-- /See:/ 'mkStopLogging' smart constructor.
newtype StopLogging = StopLogging'
  { name :: Core.Text
    -- ^ Specifies the name or the CloudTrail ARN of the trail for which CloudTrail will stop logging AWS API calls. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopLogging' value with any optional fields omitted.
mkStopLogging
    :: Core.Text -- ^ 'name'
    -> StopLogging
mkStopLogging name = StopLogging'{name}

-- | Specifies the name or the CloudTrail ARN of the trail for which CloudTrail will stop logging AWS API calls. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slName :: Lens.Lens' StopLogging Core.Text
slName = Lens.field @"name"
{-# INLINEABLE slName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StopLogging where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopLogging where
        toHeaders StopLogging{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.StopLogging")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopLogging where
        toJSON StopLogging{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StopLogging where
        type Rs StopLogging = StopLoggingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopLoggingResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkStopLoggingResponse' smart constructor.
newtype StopLoggingResponse = StopLoggingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopLoggingResponse' value with any optional fields omitted.
mkStopLoggingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopLoggingResponse
mkStopLoggingResponse responseStatus
  = StopLoggingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slrrsResponseStatus :: Lens.Lens' StopLoggingResponse Core.Int
slrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE slrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
