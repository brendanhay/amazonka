{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribePublishingDestination (..)
    , mkDescribePublishingDestination
    -- ** Request lenses
    , dpdfDetectorId
    , dpdfDestinationId

    -- * Destructuring the response
    , DescribePublishingDestinationResponse (..)
    , mkDescribePublishingDestinationResponse
    -- ** Response lenses
    , dpdrfrsDestinationId
    , dpdrfrsDestinationType
    , dpdrfrsStatus
    , dpdrfrsPublishingFailureStartTimestamp
    , dpdrfrsDestinationProperties
    , dpdrfrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePublishingDestination' smart constructor.
data DescribePublishingDestination = DescribePublishingDestination'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector associated with the publishing destination to retrieve.
  , destinationId :: Core.Text
    -- ^ The ID of the publishing destination to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePublishingDestination' value with any optional fields omitted.
mkDescribePublishingDestination
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'destinationId'
    -> DescribePublishingDestination
mkDescribePublishingDestination detectorId destinationId
  = DescribePublishingDestination'{detectorId, destinationId}

-- | The unique ID of the detector associated with the publishing destination to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfDetectorId :: Lens.Lens' DescribePublishingDestination Types.DetectorId
dpdfDetectorId = Lens.field @"detectorId"
{-# INLINEABLE dpdfDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The ID of the publishing destination to retrieve.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdfDestinationId :: Lens.Lens' DescribePublishingDestination Core.Text
dpdfDestinationId = Lens.field @"destinationId"
{-# INLINEABLE dpdfDestinationId #-}
{-# DEPRECATED destinationId "Use generic-lens or generic-optics with 'destinationId' instead"  #-}

instance Core.ToQuery DescribePublishingDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePublishingDestination where
        toHeaders DescribePublishingDestination{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribePublishingDestination where
        type Rs DescribePublishingDestination =
             DescribePublishingDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/publishingDestination/"
                             Core.<> Core.toText destinationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePublishingDestinationResponse' Core.<$>
                   (x Core..: "destinationId") Core.<*> x Core..: "destinationType"
                     Core.<*> x Core..: "status"
                     Core.<*> x Core..: "publishingFailureStartTimestamp"
                     Core.<*> x Core..: "destinationProperties"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribePublishingDestinationResponse' smart constructor.
data DescribePublishingDestinationResponse = DescribePublishingDestinationResponse'
  { destinationId :: Core.Text
    -- ^ The ID of the publishing destination.
  , destinationType :: Types.DestinationType
    -- ^ The type of publishing destination. Currently, only Amazon S3 buckets are supported.
  , status :: Types.PublishingStatus
    -- ^ The status of the publishing destination.
  , publishingFailureStartTimestamp :: Core.Integer
    -- ^ The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
  , destinationProperties :: Types.DestinationProperties
    -- ^ A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePublishingDestinationResponse' value with any optional fields omitted.
mkDescribePublishingDestinationResponse
    :: Core.Text -- ^ 'destinationId'
    -> Types.DestinationType -- ^ 'destinationType'
    -> Types.PublishingStatus -- ^ 'status'
    -> Core.Integer -- ^ 'publishingFailureStartTimestamp'
    -> Types.DestinationProperties -- ^ 'destinationProperties'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribePublishingDestinationResponse
mkDescribePublishingDestinationResponse destinationId
  destinationType status publishingFailureStartTimestamp
  destinationProperties responseStatus
  = DescribePublishingDestinationResponse'{destinationId,
                                           destinationType, status, publishingFailureStartTimestamp,
                                           destinationProperties, responseStatus}

-- | The ID of the publishing destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsDestinationId :: Lens.Lens' DescribePublishingDestinationResponse Core.Text
dpdrfrsDestinationId = Lens.field @"destinationId"
{-# INLINEABLE dpdrfrsDestinationId #-}
{-# DEPRECATED destinationId "Use generic-lens or generic-optics with 'destinationId' instead"  #-}

-- | The type of publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsDestinationType :: Lens.Lens' DescribePublishingDestinationResponse Types.DestinationType
dpdrfrsDestinationType = Lens.field @"destinationType"
{-# INLINEABLE dpdrfrsDestinationType #-}
{-# DEPRECATED destinationType "Use generic-lens or generic-optics with 'destinationType' instead"  #-}

-- | The status of the publishing destination.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsStatus :: Lens.Lens' DescribePublishingDestinationResponse Types.PublishingStatus
dpdrfrsStatus = Lens.field @"status"
{-# INLINEABLE dpdrfrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The time, in epoch millisecond format, at which GuardDuty was first unable to publish findings to the destination.
--
-- /Note:/ Consider using 'publishingFailureStartTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsPublishingFailureStartTimestamp :: Lens.Lens' DescribePublishingDestinationResponse Core.Integer
dpdrfrsPublishingFailureStartTimestamp = Lens.field @"publishingFailureStartTimestamp"
{-# INLINEABLE dpdrfrsPublishingFailureStartTimestamp #-}
{-# DEPRECATED publishingFailureStartTimestamp "Use generic-lens or generic-optics with 'publishingFailureStartTimestamp' instead"  #-}

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsDestinationProperties :: Lens.Lens' DescribePublishingDestinationResponse Types.DestinationProperties
dpdrfrsDestinationProperties = Lens.field @"destinationProperties"
{-# INLINEABLE dpdrfrsDestinationProperties #-}
{-# DEPRECATED destinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrfrsResponseStatus :: Lens.Lens' DescribePublishingDestinationResponse Core.Int
dpdrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpdrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
