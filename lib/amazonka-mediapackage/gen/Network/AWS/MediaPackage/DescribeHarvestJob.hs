{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DescribeHarvestJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing HarvestJob.
module Network.AWS.MediaPackage.DescribeHarvestJob
  ( -- * Creating a request
    DescribeHarvestJob (..),
    mkDescribeHarvestJob,

    -- ** Request lenses
    dhjId,

    -- * Destructuring the response
    DescribeHarvestJobResponse (..),
    mkDescribeHarvestJobResponse,

    -- ** Response lenses
    dhjrrsArn,
    dhjrrsChannelId,
    dhjrrsCreatedAt,
    dhjrrsEndTime,
    dhjrrsId,
    dhjrrsOriginEndpointId,
    dhjrrsS3Destination,
    dhjrrsStartTime,
    dhjrrsStatus,
    dhjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeHarvestJob' smart constructor.
newtype DescribeHarvestJob = DescribeHarvestJob'
  { -- | The ID of the HarvestJob.
    id :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHarvestJob' value with any optional fields omitted.
mkDescribeHarvestJob ::
  -- | 'id'
  Core.Text ->
  DescribeHarvestJob
mkDescribeHarvestJob id = DescribeHarvestJob' {id}

-- | The ID of the HarvestJob.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjId :: Lens.Lens' DescribeHarvestJob Core.Text
dhjId = Lens.field @"id"
{-# DEPRECATED dhjId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest DescribeHarvestJob where
  type Rs DescribeHarvestJob = DescribeHarvestJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/harvest_jobs/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHarvestJobResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "channelId")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "endTime")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "originEndpointId")
            Core.<*> (x Core..:? "s3Destination")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeHarvestJobResponse' smart constructor.
data DescribeHarvestJobResponse = DescribeHarvestJobResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the Channel that the HarvestJob will harvest from.
    channelId :: Core.Maybe Core.Text,
    -- | The time the HarvestJob was submitted
    createdAt :: Core.Maybe Core.Text,
    -- | The end of the time-window which will be harvested.
    endTime :: Core.Maybe Core.Text,
    -- | The ID of the HarvestJob. The ID must be unique within the region
    --
    -- and it cannot be changed after the HarvestJob is submitted.
    id :: Core.Maybe Core.Text,
    -- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
    --
    -- This cannot be changed after the HarvestJob is submitted.
    originEndpointId :: Core.Maybe Core.Text,
    s3Destination :: Core.Maybe Types.S3Destination,
    -- | The start of the time-window which will be harvested.
    startTime :: Core.Maybe Core.Text,
    -- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
    --
    -- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
    -- include an explanation of why the HarvestJob failed.
    status :: Core.Maybe Types.Status,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHarvestJobResponse' value with any optional fields omitted.
mkDescribeHarvestJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeHarvestJobResponse
mkDescribeHarvestJobResponse responseStatus =
  DescribeHarvestJobResponse'
    { arn = Core.Nothing,
      channelId = Core.Nothing,
      createdAt = Core.Nothing,
      endTime = Core.Nothing,
      id = Core.Nothing,
      originEndpointId = Core.Nothing,
      s3Destination = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) assigned to the HarvestJob.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsArn :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Core.Text)
dhjrrsArn = Lens.field @"arn"
{-# DEPRECATED dhjrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the Channel that the HarvestJob will harvest from.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsChannelId :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Core.Text)
dhjrrsChannelId = Lens.field @"channelId"
{-# DEPRECATED dhjrrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The time the HarvestJob was submitted
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsCreatedAt :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Core.Text)
dhjrrsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED dhjrrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The end of the time-window which will be harvested.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsEndTime :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Core.Text)
dhjrrsEndTime = Lens.field @"endTime"
{-# DEPRECATED dhjrrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The ID of the HarvestJob. The ID must be unique within the region
--
-- and it cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsId :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Core.Text)
dhjrrsId = Lens.field @"id"
{-# DEPRECATED dhjrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the OriginEndpoint that the HarvestJob will harvest from.
--
-- This cannot be changed after the HarvestJob is submitted.
--
-- /Note:/ Consider using 'originEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsOriginEndpointId :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Core.Text)
dhjrrsOriginEndpointId = Lens.field @"originEndpointId"
{-# DEPRECATED dhjrrsOriginEndpointId "Use generic-lens or generic-optics with 'originEndpointId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsS3Destination :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Types.S3Destination)
dhjrrsS3Destination = Lens.field @"s3Destination"
{-# DEPRECATED dhjrrsS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

-- | The start of the time-window which will be harvested.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsStartTime :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Core.Text)
dhjrrsStartTime = Lens.field @"startTime"
{-# DEPRECATED dhjrrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The current status of the HarvestJob. Consider setting up a CloudWatch Event to listen for
--
-- HarvestJobs as they succeed or fail. In the event of failure, the CloudWatch Event will
-- include an explanation of why the HarvestJob failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsStatus :: Lens.Lens' DescribeHarvestJobResponse (Core.Maybe Types.Status)
dhjrrsStatus = Lens.field @"status"
{-# DEPRECATED dhjrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhjrrsResponseStatus :: Lens.Lens' DescribeHarvestJobResponse Core.Int
dhjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
