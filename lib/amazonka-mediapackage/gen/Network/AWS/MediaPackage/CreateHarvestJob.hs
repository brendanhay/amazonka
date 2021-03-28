{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.CreateHarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new HarvestJob record.
module Network.AWS.MediaPackage.CreateHarvestJob
    (
    -- * Creating a request
      CreateHarvestJob (..)
    , mkCreateHarvestJob
    -- ** Request lenses
    , chjS3Destination
    , chjEndTime
    , chjOriginEndpointId
    , chjStartTime
    , chjId

    -- * Destructuring the response
    , CreateHarvestJobResponse (..)
    , mkCreateHarvestJobResponse
    -- ** Response lenses
    , chjrrsArn
    , chjrrsChannelId
    , chjrrsCreatedAt
    , chjrrsEndTime
    , chjrrsId
    , chjrrsOriginEndpointId
    , chjrrsS3Destination
    , chjrrsStartTime
    , chjrrsStatus
    , chjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to create a new HarvestJob.
--
-- /See:/ 'mkCreateHarvestJob' smart constructor.
data CreateHarvestJob = CreateHarvestJob'
  { s3Destination :: Types.S3Destination
  , endTime :: Core.Text
    -- ^ The end of the time-window which will be harvested
  , originEndpointId :: Core.Text
    -- ^ The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
  , startTime :: Core.Text
    -- ^ The start of the time-window which will be harvested
  , id :: Core.Text
    -- ^ The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHarvestJob' value with any optional fields omitted.
mkCreateHarvestJob
    :: Types.S3Destination -- ^ 's3Destination'
    -> Core.Text -- ^ 'endTime'
    -> Core.Text -- ^ 'originEndpointId'
    -> Core.Text -- ^ 'startTime'
    -> Core.Text -- ^ 'id'
    -> CreateHarvestJob
mkCreateHarvestJob s3Destination endTime originEndpointId startTime
  id
  = CreateHarvestJob'{s3Destination, endTime, originEndpointId,
                      startTime, id}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjS3Destination :: Lens.Lens' CreateHarvestJob Types.S3Destination
chjS3Destination = Lens.field @"s3Destination"
{-# INLINEABLE chjS3Destination #-}
{-# DEPRECATED s3Destination "Use generic-lens or generic-optics with 's3Destination' instead"  #-}

-- | The end of the time-window which will be harvested
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjEndTime :: Lens.Lens' CreateHarvestJob Core.Text
chjEndTime = Lens.field @"endTime"
{-# INLINEABLE chjEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjOriginEndpointId :: Lens.Lens' CreateHarvestJob Core.Text
chjOriginEndpointId = Lens.field @"originEndpointId"
{-# INLINEABLE chjOriginEndpointId #-}
{-# DEPRECATED originEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead"  #-}

-- | The start of the time-window which will be harvested
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjStartTime :: Lens.Lens' CreateHarvestJob Core.Text
chjStartTime = Lens.field @"startTime"
{-# INLINEABLE chjStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjId :: Lens.Lens' CreateHarvestJob Core.Text
chjId = Lens.field @"id"
{-# INLINEABLE chjId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery CreateHarvestJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateHarvestJob where
        toHeaders CreateHarvestJob{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateHarvestJob where
        toJSON CreateHarvestJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("s3Destination" Core..= s3Destination),
                  Core.Just ("endTime" Core..= endTime),
                  Core.Just ("originEndpointId" Core..= originEndpointId),
                  Core.Just ("startTime" Core..= startTime),
                  Core.Just ("id" Core..= id)])

instance Core.AWSRequest CreateHarvestJob where
        type Rs CreateHarvestJob = CreateHarvestJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/harvest_jobs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateHarvestJobResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "channelId" Core.<*>
                     x Core..:? "createdAt"
                     Core.<*> x Core..:? "endTime"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "originEndpointId"
                     Core.<*> x Core..:? "s3Destination"
                     Core.<*> x Core..:? "startTime"
                     Core.<*> x Core..:? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateHarvestJobResponse' smart constructor.
data CreateHarvestJobResponse = CreateHarvestJobResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) assigned to the HarvestJob.
  , channelId :: Core.Maybe Core.Text
    -- ^ The ID of the Channel that the HarvestJob will harvest from.
  , createdAt :: Core.Maybe Core.Text
    -- ^ The time the HarvestJob was submitted
  , endTime :: Core.Maybe Core.Text
    -- ^ The end of the time-window which will be harvested.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
  , originEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
  , s3Destination :: Core.Maybe Types.S3Destination
  , startTime :: Core.Maybe Core.Text
    -- ^ The start of the time-window which will be harvested.
  , status :: Core.Maybe Types.Status
    -- ^ The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHarvestJobResponse' value with any optional fields omitted.
mkCreateHarvestJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateHarvestJobResponse
mkCreateHarvestJobResponse responseStatus
  = CreateHarvestJobResponse'{arn = Core.Nothing,
                              channelId = Core.Nothing, createdAt = Core.Nothing,
                              endTime = Core.Nothing, id = Core.Nothing,
                              originEndpointId = Core.Nothing, s3Destination = Core.Nothing,
                              startTime = Core.Nothing, status = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsArn :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Core.Text)
chjrrsArn = Lens.field @"arn"
{-# INLINEABLE chjrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The ID of the Channel that the HarvestJob will harvest from.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsChannelId :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Core.Text)
chjrrsChannelId = Lens.field @"channelId"
{-# INLINEABLE chjrrsChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | The time the HarvestJob was submitted
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsCreatedAt :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Core.Text)
chjrrsCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE chjrrsCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The end of the time-window which will be harvested.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsEndTime :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Core.Text)
chjrrsEndTime = Lens.field @"endTime"
{-# INLINEABLE chjrrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsId :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Core.Text)
chjrrsId = Lens.field @"id"
{-# INLINEABLE chjrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsOriginEndpointId :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Core.Text)
chjrrsOriginEndpointId = Lens.field @"originEndpointId"
{-# INLINEABLE chjrrsOriginEndpointId #-}
{-# DEPRECATED originEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsS3Destination :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Types.S3Destination)
chjrrsS3Destination = Lens.field @"s3Destination"
{-# INLINEABLE chjrrsS3Destination #-}
{-# DEPRECATED s3Destination "Use generic-lens or generic-optics with 's3Destination' instead"  #-}

-- | The start of the time-window which will be harvested.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsStartTime :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Core.Text)
chjrrsStartTime = Lens.field @"startTime"
{-# INLINEABLE chjrrsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsStatus :: Lens.Lens' CreateHarvestJobResponse (Core.Maybe Types.Status)
chjrrsStatus = Lens.field @"status"
{-# INLINEABLE chjrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chjrrsResponseStatus :: Lens.Lens' CreateHarvestJobResponse Core.Int
chjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE chjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
