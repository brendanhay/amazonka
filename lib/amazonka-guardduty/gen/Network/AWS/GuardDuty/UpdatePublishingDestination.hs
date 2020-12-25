{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdatePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the publishing destination specified by the @destinationId@ .
module Network.AWS.GuardDuty.UpdatePublishingDestination
  ( -- * Creating a request
    UpdatePublishingDestination (..),
    mkUpdatePublishingDestination,

    -- ** Request lenses
    updDetectorId,
    updDestinationId,
    updDestinationProperties,

    -- * Destructuring the response
    UpdatePublishingDestinationResponse (..),
    mkUpdatePublishingDestinationResponse,

    -- ** Response lenses
    updrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePublishingDestination' smart constructor.
data UpdatePublishingDestination = UpdatePublishingDestination'
  { -- | The ID of the detector associated with the publishing destinations to update.
    detectorId :: Types.DetectorId,
    -- | The ID of the publishing destination to update.
    destinationId :: Types.String,
    -- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
    destinationProperties :: Core.Maybe Types.DestinationProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePublishingDestination' value with any optional fields omitted.
mkUpdatePublishingDestination ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'destinationId'
  Types.String ->
  UpdatePublishingDestination
mkUpdatePublishingDestination detectorId destinationId =
  UpdatePublishingDestination'
    { detectorId,
      destinationId,
      destinationProperties = Core.Nothing
    }

-- | The ID of the detector associated with the publishing destinations to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDetectorId :: Lens.Lens' UpdatePublishingDestination Types.DetectorId
updDetectorId = Lens.field @"detectorId"
{-# DEPRECATED updDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The ID of the publishing destination to update.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDestinationId :: Lens.Lens' UpdatePublishingDestination Types.String
updDestinationId = Lens.field @"destinationId"
{-# DEPRECATED updDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | A @DestinationProperties@ object that includes the @DestinationArn@ and @KmsKeyArn@ of the publishing destination.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updDestinationProperties :: Lens.Lens' UpdatePublishingDestination (Core.Maybe Types.DestinationProperties)
updDestinationProperties = Lens.field @"destinationProperties"
{-# DEPRECATED updDestinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead." #-}

instance Core.FromJSON UpdatePublishingDestination where
  toJSON UpdatePublishingDestination {..} =
    Core.object
      ( Core.catMaybes
          [("destinationProperties" Core..=) Core.<$> destinationProperties]
      )

instance Core.AWSRequest UpdatePublishingDestination where
  type
    Rs UpdatePublishingDestination =
      UpdatePublishingDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/publishingDestination/")
                Core.<> (Core.toText destinationId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePublishingDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePublishingDestinationResponse' smart constructor.
newtype UpdatePublishingDestinationResponse = UpdatePublishingDestinationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePublishingDestinationResponse' value with any optional fields omitted.
mkUpdatePublishingDestinationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdatePublishingDestinationResponse
mkUpdatePublishingDestinationResponse responseStatus =
  UpdatePublishingDestinationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrrsResponseStatus :: Lens.Lens' UpdatePublishingDestinationResponse Core.Int
updrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED updrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
