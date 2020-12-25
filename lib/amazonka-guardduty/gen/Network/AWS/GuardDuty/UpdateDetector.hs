{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.UpdateDetector
  ( -- * Creating a request
    UpdateDetector (..),
    mkUpdateDetector,

    -- ** Request lenses
    udDetectorId,
    udDataSources,
    udEnable,
    udFindingPublishingFrequency,

    -- * Destructuring the response
    UpdateDetectorResponse (..),
    mkUpdateDetectorResponse,

    -- ** Response lenses
    udrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDetector' smart constructor.
data UpdateDetector = UpdateDetector'
  { -- | The unique ID of the detector to update.
    detectorId :: Types.DetectorId,
    -- | An object that describes which data sources will be updated.
    dataSources :: Core.Maybe Types.DataSourceConfigurations,
    -- | Specifies whether the detector is enabled or not enabled.
    enable :: Core.Maybe Core.Bool,
    -- | An enum value that specifies how frequently findings are exported, such as to CloudWatch Events.
    findingPublishingFrequency :: Core.Maybe Types.FindingPublishingFrequency
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDetector' value with any optional fields omitted.
mkUpdateDetector ::
  -- | 'detectorId'
  Types.DetectorId ->
  UpdateDetector
mkUpdateDetector detectorId =
  UpdateDetector'
    { detectorId,
      dataSources = Core.Nothing,
      enable = Core.Nothing,
      findingPublishingFrequency = Core.Nothing
    }

-- | The unique ID of the detector to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDetectorId :: Lens.Lens' UpdateDetector Types.DetectorId
udDetectorId = Lens.field @"detectorId"
{-# DEPRECATED udDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | An object that describes which data sources will be updated.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDataSources :: Lens.Lens' UpdateDetector (Core.Maybe Types.DataSourceConfigurations)
udDataSources = Lens.field @"dataSources"
{-# DEPRECATED udDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | Specifies whether the detector is enabled or not enabled.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEnable :: Lens.Lens' UpdateDetector (Core.Maybe Core.Bool)
udEnable = Lens.field @"enable"
{-# DEPRECATED udEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | An enum value that specifies how frequently findings are exported, such as to CloudWatch Events.
--
-- /Note:/ Consider using 'findingPublishingFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udFindingPublishingFrequency :: Lens.Lens' UpdateDetector (Core.Maybe Types.FindingPublishingFrequency)
udFindingPublishingFrequency = Lens.field @"findingPublishingFrequency"
{-# DEPRECATED udFindingPublishingFrequency "Use generic-lens or generic-optics with 'findingPublishingFrequency' instead." #-}

instance Core.FromJSON UpdateDetector where
  toJSON UpdateDetector {..} =
    Core.object
      ( Core.catMaybes
          [ ("dataSources" Core..=) Core.<$> dataSources,
            ("enable" Core..=) Core.<$> enable,
            ("findingPublishingFrequency" Core..=)
              Core.<$> findingPublishingFrequency
          ]
      )

instance Core.AWSRequest UpdateDetector where
  type Rs UpdateDetector = UpdateDetectorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath ("/detector/" Core.<> (Core.toText detectorId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDetectorResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDetectorResponse' smart constructor.
newtype UpdateDetectorResponse = UpdateDetectorResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDetectorResponse' value with any optional fields omitted.
mkUpdateDetectorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDetectorResponse
mkUpdateDetectorResponse responseStatus =
  UpdateDetectorResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDetectorResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
