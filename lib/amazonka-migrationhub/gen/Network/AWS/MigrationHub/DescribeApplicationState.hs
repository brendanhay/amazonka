{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.DescribeApplicationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the migration status of an application.
module Network.AWS.MigrationHub.DescribeApplicationState
    (
    -- * Creating a request
      DescribeApplicationState (..)
    , mkDescribeApplicationState
    -- ** Request lenses
    , dasApplicationId

    -- * Destructuring the response
    , DescribeApplicationStateResponse (..)
    , mkDescribeApplicationStateResponse
    -- ** Response lenses
    , dasrrsApplicationStatus
    , dasrrsLastUpdatedTime
    , dasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeApplicationState' smart constructor.
newtype DescribeApplicationState = DescribeApplicationState'
  { applicationId :: Types.ApplicationId
    -- ^ The configurationId in Application Discovery Service that uniquely identifies the grouped application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApplicationState' value with any optional fields omitted.
mkDescribeApplicationState
    :: Types.ApplicationId -- ^ 'applicationId'
    -> DescribeApplicationState
mkDescribeApplicationState applicationId
  = DescribeApplicationState'{applicationId}

-- | The configurationId in Application Discovery Service that uniquely identifies the grouped application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasApplicationId :: Lens.Lens' DescribeApplicationState Types.ApplicationId
dasApplicationId = Lens.field @"applicationId"
{-# INLINEABLE dasApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DescribeApplicationState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeApplicationState where
        toHeaders DescribeApplicationState{..}
          = Core.pure
              ("X-Amz-Target", "AWSMigrationHub.DescribeApplicationState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeApplicationState where
        toJSON DescribeApplicationState{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationId" Core..= applicationId)])

instance Core.AWSRequest DescribeApplicationState where
        type Rs DescribeApplicationState = DescribeApplicationStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeApplicationStateResponse' Core.<$>
                   (x Core..:? "ApplicationStatus") Core.<*>
                     x Core..:? "LastUpdatedTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeApplicationStateResponse' smart constructor.
data DescribeApplicationStateResponse = DescribeApplicationStateResponse'
  { applicationStatus :: Core.Maybe Types.ApplicationStatus
    -- ^ Status of the application - Not Started, In-Progress, Complete.
  , lastUpdatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the application status was last updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeApplicationStateResponse' value with any optional fields omitted.
mkDescribeApplicationStateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeApplicationStateResponse
mkDescribeApplicationStateResponse responseStatus
  = DescribeApplicationStateResponse'{applicationStatus =
                                        Core.Nothing,
                                      lastUpdatedTime = Core.Nothing, responseStatus}

-- | Status of the application - Not Started, In-Progress, Complete.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsApplicationStatus :: Lens.Lens' DescribeApplicationStateResponse (Core.Maybe Types.ApplicationStatus)
dasrrsApplicationStatus = Lens.field @"applicationStatus"
{-# INLINEABLE dasrrsApplicationStatus #-}
{-# DEPRECATED applicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead"  #-}

-- | The timestamp when the application status was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsLastUpdatedTime :: Lens.Lens' DescribeApplicationStateResponse (Core.Maybe Core.NominalDiffTime)
dasrrsLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# INLINEABLE dasrrsLastUpdatedTime #-}
{-# DEPRECATED lastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsResponseStatus :: Lens.Lens' DescribeApplicationStateResponse Core.Int
dasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
