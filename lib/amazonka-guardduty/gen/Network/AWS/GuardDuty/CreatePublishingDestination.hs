{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreatePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a publishing destination to export findings to. The resource to export findings to must exist before you use this operation.
module Network.AWS.GuardDuty.CreatePublishingDestination
    (
    -- * Creating a request
      CreatePublishingDestination (..)
    , mkCreatePublishingDestination
    -- ** Request lenses
    , cpdDetectorId
    , cpdDestinationType
    , cpdDestinationProperties
    , cpdClientToken

    -- * Destructuring the response
    , CreatePublishingDestinationResponse (..)
    , mkCreatePublishingDestinationResponse
    -- ** Response lenses
    , cpdrrsDestinationId
    , cpdrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePublishingDestination' smart constructor.
data CreatePublishingDestination = CreatePublishingDestination'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the GuardDuty detector associated with the publishing destination.
  , destinationType :: Types.DestinationType
    -- ^ The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
  , destinationProperties :: Types.DestinationProperties
    -- ^ The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ The idempotency token for the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublishingDestination' value with any optional fields omitted.
mkCreatePublishingDestination
    :: Types.DetectorId -- ^ 'detectorId'
    -> Types.DestinationType -- ^ 'destinationType'
    -> Types.DestinationProperties -- ^ 'destinationProperties'
    -> CreatePublishingDestination
mkCreatePublishingDestination detectorId destinationType
  destinationProperties
  = CreatePublishingDestination'{detectorId, destinationType,
                                 destinationProperties, clientToken = Core.Nothing}

-- | The ID of the GuardDuty detector associated with the publishing destination.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDetectorId :: Lens.Lens' CreatePublishingDestination Types.DetectorId
cpdDetectorId = Lens.field @"detectorId"
{-# INLINEABLE cpdDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDestinationType :: Lens.Lens' CreatePublishingDestination Types.DestinationType
cpdDestinationType = Lens.field @"destinationType"
{-# INLINEABLE cpdDestinationType #-}
{-# DEPRECATED destinationType "Use generic-lens or generic-optics with 'destinationType' instead"  #-}

-- | The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDestinationProperties :: Lens.Lens' CreatePublishingDestination Types.DestinationProperties
cpdDestinationProperties = Lens.field @"destinationProperties"
{-# INLINEABLE cpdDestinationProperties #-}
{-# DEPRECATED destinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead"  #-}

-- | The idempotency token for the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdClientToken :: Lens.Lens' CreatePublishingDestination (Core.Maybe Types.ClientToken)
cpdClientToken = Lens.field @"clientToken"
{-# INLINEABLE cpdClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

instance Core.ToQuery CreatePublishingDestination where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePublishingDestination where
        toHeaders CreatePublishingDestination{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePublishingDestination where
        toJSON CreatePublishingDestination{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("destinationType" Core..= destinationType),
                  Core.Just ("destinationProperties" Core..= destinationProperties),
                  ("clientToken" Core..=) Core.<$> clientToken])

instance Core.AWSRequest CreatePublishingDestination where
        type Rs CreatePublishingDestination =
             CreatePublishingDestinationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/publishingDestination",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePublishingDestinationResponse' Core.<$>
                   (x Core..: "destinationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePublishingDestinationResponse' smart constructor.
data CreatePublishingDestinationResponse = CreatePublishingDestinationResponse'
  { destinationId :: Core.Text
    -- ^ The ID of the publishing destination that is created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublishingDestinationResponse' value with any optional fields omitted.
mkCreatePublishingDestinationResponse
    :: Core.Text -- ^ 'destinationId'
    -> Core.Int -- ^ 'responseStatus'
    -> CreatePublishingDestinationResponse
mkCreatePublishingDestinationResponse destinationId responseStatus
  = CreatePublishingDestinationResponse'{destinationId,
                                         responseStatus}

-- | The ID of the publishing destination that is created.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrrsDestinationId :: Lens.Lens' CreatePublishingDestinationResponse Core.Text
cpdrrsDestinationId = Lens.field @"destinationId"
{-# INLINEABLE cpdrrsDestinationId #-}
{-# DEPRECATED destinationId "Use generic-lens or generic-optics with 'destinationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrrsResponseStatus :: Lens.Lens' CreatePublishingDestinationResponse Core.Int
cpdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
