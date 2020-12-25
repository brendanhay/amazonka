{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreatePublishingDestination (..),
    mkCreatePublishingDestination,

    -- ** Request lenses
    cpdDetectorId,
    cpdDestinationType,
    cpdDestinationProperties,
    cpdClientToken,

    -- * Destructuring the response
    CreatePublishingDestinationResponse (..),
    mkCreatePublishingDestinationResponse,

    -- ** Response lenses
    cpdrrsDestinationId,
    cpdrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePublishingDestination' smart constructor.
data CreatePublishingDestination = CreatePublishingDestination'
  { -- | The ID of the GuardDuty detector associated with the publishing destination.
    detectorId :: Types.DetectorId,
    -- | The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
    destinationType :: Types.DestinationType,
    -- | The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
    destinationProperties :: Types.DestinationProperties,
    -- | The idempotency token for the request.
    clientToken :: Core.Maybe Types.ClientToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublishingDestination' value with any optional fields omitted.
mkCreatePublishingDestination ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'destinationType'
  Types.DestinationType ->
  -- | 'destinationProperties'
  Types.DestinationProperties ->
  CreatePublishingDestination
mkCreatePublishingDestination
  detectorId
  destinationType
  destinationProperties =
    CreatePublishingDestination'
      { detectorId,
        destinationType,
        destinationProperties,
        clientToken = Core.Nothing
      }

-- | The ID of the GuardDuty detector associated with the publishing destination.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDetectorId :: Lens.Lens' CreatePublishingDestination Types.DetectorId
cpdDetectorId = Lens.field @"detectorId"
{-# DEPRECATED cpdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDestinationType :: Lens.Lens' CreatePublishingDestination Types.DestinationType
cpdDestinationType = Lens.field @"destinationType"
{-# DEPRECATED cpdDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDestinationProperties :: Lens.Lens' CreatePublishingDestination Types.DestinationProperties
cpdDestinationProperties = Lens.field @"destinationProperties"
{-# DEPRECATED cpdDestinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead." #-}

-- | The idempotency token for the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdClientToken :: Lens.Lens' CreatePublishingDestination (Core.Maybe Types.ClientToken)
cpdClientToken = Lens.field @"clientToken"
{-# DEPRECATED cpdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

instance Core.FromJSON CreatePublishingDestination where
  toJSON CreatePublishingDestination {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("destinationType" Core..= destinationType),
            Core.Just ("destinationProperties" Core..= destinationProperties),
            ("clientToken" Core..=) Core.<$> clientToken
          ]
      )

instance Core.AWSRequest CreatePublishingDestination where
  type
    Rs CreatePublishingDestination =
      CreatePublishingDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/publishingDestination")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePublishingDestinationResponse'
            Core.<$> (x Core..: "destinationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePublishingDestinationResponse' smart constructor.
data CreatePublishingDestinationResponse = CreatePublishingDestinationResponse'
  { -- | The ID of the publishing destination that is created.
    destinationId :: Types.DestinationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublishingDestinationResponse' value with any optional fields omitted.
mkCreatePublishingDestinationResponse ::
  -- | 'destinationId'
  Types.DestinationId ->
  -- | 'responseStatus'
  Core.Int ->
  CreatePublishingDestinationResponse
mkCreatePublishingDestinationResponse destinationId responseStatus =
  CreatePublishingDestinationResponse'
    { destinationId,
      responseStatus
    }

-- | The ID of the publishing destination that is created.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrrsDestinationId :: Lens.Lens' CreatePublishingDestinationResponse Types.DestinationId
cpdrrsDestinationId = Lens.field @"destinationId"
{-# DEPRECATED cpdrrsDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrrsResponseStatus :: Lens.Lens' CreatePublishingDestinationResponse Core.Int
cpdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
