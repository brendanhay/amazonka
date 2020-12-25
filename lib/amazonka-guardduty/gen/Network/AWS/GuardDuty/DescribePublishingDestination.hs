{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DescribePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the publishing destination specified by the provided @destinationId@ .
module Network.AWS.GuardDuty.DescribePublishingDestination
  ( -- * Creating a request
    DescribePublishingDestination (..),
    mkDescribePublishingDestination,

    -- ** Request lenses
    dpdfDetectorId,
    dpdfDestinationId,

    -- * Destructuring the response
    DescribePublishingDestinationResponse (..),
    mkDescribePublishingDestinationResponse,

    -- ** Response lenses
    dpdrfrsDestinationId,
    dpdrfrsDestinationType,
    dpdrfrsStatus,
    dpdrfrsPublishingFailureStartTimestamp,
    dpdrfrsDestinationProperties,
    dpdrfrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePublishingDestination' smart constructor.
data DescribePublishingDestination = DescribePublishingDestination'
  { -- | The unique ID of the detector associated with the publishing destination to retrieve.
    detectorId :: Types.DetectorId,
    -- | The ID of the publishing destination to retrieve.
    destinationId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePublishingDestination' value with any optional fields omitted.
mkDescribePublishingDestination ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'destinationId'
  Types.String ->
  DescribePublishingDestination
mkDescribePublishingDestination detectorId destinationId =
  DescribePublishingDestination' {detectorId, destinationId}

-- | The unique ID of the detector associated with the publishing destination to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfDetectorId :: Lens.Lens' DescribePublishingDestination Types.DetectorId
dpdfDetectorId = Lens.field @"detectorId"
{-# DEPRECATED dpdfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The ID of the publishing destination to retrieve.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfDestinationId :: Lens.Lens' DescribePublishingDestination Types.String
dpdfDestinationId = Lens.field @"destinationId"
{-# DEPRECATED dpdfDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Core.AWSRequest DescribePublishingDestination where
  type
    Rs DescribePublishingDestination =
      DescribePublishingDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/publishingDestination/")
                Core.<> (Core.toText destinationId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePublishingDestinationResponse'
            Core.<$> (x Core..: "destinationId")
            Core.<*> (x Core..: "destinationType")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..: "publishingFailureStartTimestamp")
            Core.<*> (x Core..: "destinationProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribePublishingDestinationResponse' smart constructor.
data DescribePublishingDestinationResponse = DescribePublishingDestinationResponse'
  { -- | The ID of the publishing destination.
    destinationId :: Types.String,
    -- | The type of publishing destination. Currently, only Amazon S3 buckets are supported.
    destinationType :: Types.DestinationType,
    -- | The status of the publishing destination.
    status :: Types.PublishingStatus,
    -- | The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
    publishingFailureStartTimestamp :: Core.Integer,
    -- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
    destinationProperties :: Types.DestinationProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePublishingDestinationResponse' value with any optional fields omitted.
mkDescribePublishingDestinationResponse ::
  -- | 'destinationId'
  Types.String ->
  -- | 'destinationType'
  Types.DestinationType ->
  -- | 'status'
  Types.PublishingStatus ->
  -- | 'publishingFailureStartTimestamp'
  Core.Integer ->
  -- | 'destinationProperties'
  Types.DestinationProperties ->
  -- | 'responseStatus'
  Core.Int ->
  DescribePublishingDestinationResponse
mkDescribePublishingDestinationResponse
  destinationId
  destinationType
  status
  publishingFailureStartTimestamp
  destinationProperties
  responseStatus =
    DescribePublishingDestinationResponse'
      { destinationId,
        destinationType,
        status,
        publishingFailureStartTimestamp,
        destinationProperties,
        responseStatus
      }

-- | The ID of the publishing destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsDestinationId :: Lens.Lens' DescribePublishingDestinationResponse Types.String
dpdrfrsDestinationId = Lens.field @"destinationId"
{-# DEPRECATED dpdrfrsDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The type of publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsDestinationType :: Lens.Lens' DescribePublishingDestinationResponse Types.DestinationType
dpdrfrsDestinationType = Lens.field @"destinationType"
{-# DEPRECATED dpdrfrsDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | The status of the publishing destination.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsStatus :: Lens.Lens' DescribePublishingDestinationResponse Types.PublishingStatus
dpdrfrsStatus = Lens.field @"status"
{-# DEPRECATED dpdrfrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
--
-- /Note:/ Consider using 'publishingFailureStartTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsPublishingFailureStartTimestamp :: Lens.Lens' DescribePublishingDestinationResponse Core.Integer
dpdrfrsPublishingFailureStartTimestamp = Lens.field @"publishingFailureStartTimestamp"
{-# DEPRECATED dpdrfrsPublishingFailureStartTimestamp "Use generic-lens or generic-optics with 'publishingFailureStartTimestamp' instead." #-}

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsDestinationProperties :: Lens.Lens' DescribePublishingDestinationResponse Types.DestinationProperties
dpdrfrsDestinationProperties = Lens.field @"destinationProperties"
{-# DEPRECATED dpdrfrsDestinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsResponseStatus :: Lens.Lens' DescribePublishingDestinationResponse Core.Int
dpdrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpdrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
