{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.GetDetector
  ( -- * Creating a request
    GetDetector (..),
    mkGetDetector,

    -- ** Request lenses
    gdDetectorId,

    -- * Destructuring the response
    GetDetectorResponse (..),
    mkGetDetectorResponse,

    -- ** Response lenses
    gdrrsServiceRole,
    gdrrsStatus,
    gdrrsCreatedAt,
    gdrrsDataSources,
    gdrrsFindingPublishingFrequency,
    gdrrsTags,
    gdrrsUpdatedAt,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDetector' smart constructor.
newtype GetDetector = GetDetector'
  { -- | The unique ID of the detector that you want to get.
    detectorId :: Types.DetectorId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDetector' value with any optional fields omitted.
mkGetDetector ::
  -- | 'detectorId'
  Types.DetectorId ->
  GetDetector
mkGetDetector detectorId = GetDetector' {detectorId}

-- | The unique ID of the detector that you want to get.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDetectorId :: Lens.Lens' GetDetector Types.DetectorId
gdDetectorId = Lens.field @"detectorId"
{-# DEPRECATED gdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Core.AWSRequest GetDetector where
  type Rs GetDetector = GetDetectorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/detector/" Core.<> (Core.toText detectorId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDetectorResponse'
            Core.<$> (x Core..: "serviceRole")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "dataSources")
            Core.<*> (x Core..:? "findingPublishingFrequency")
            Core.<*> (x Core..:? "tags")
            Core.<*> (x Core..:? "updatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDetectorResponse' smart constructor.
data GetDetectorResponse = GetDetectorResponse'
  { -- | The GuardDuty service role.
    serviceRole :: Types.String,
    -- | The detector status.
    status :: Types.DetectorStatus,
    -- | The timestamp of when the detector was created.
    createdAt :: Core.Maybe Types.String,
    -- | An object that describes which data sources are enabled for the detector.
    dataSources :: Core.Maybe Types.DataSourceConfigurationsResult,
    -- | The publishing frequency of the finding.
    findingPublishingFrequency :: Core.Maybe Types.FindingPublishingFrequency,
    -- | The tags of the detector resource.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The last-updated timestamp for the detector.
    updatedAt :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDetectorResponse' value with any optional fields omitted.
mkGetDetectorResponse ::
  -- | 'serviceRole'
  Types.String ->
  -- | 'status'
  Types.DetectorStatus ->
  -- | 'responseStatus'
  Core.Int ->
  GetDetectorResponse
mkGetDetectorResponse serviceRole status responseStatus =
  GetDetectorResponse'
    { serviceRole,
      status,
      createdAt = Core.Nothing,
      dataSources = Core.Nothing,
      findingPublishingFrequency = Core.Nothing,
      tags = Core.Nothing,
      updatedAt = Core.Nothing,
      responseStatus
    }

-- | The GuardDuty service role.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsServiceRole :: Lens.Lens' GetDetectorResponse Types.String
gdrrsServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED gdrrsServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The detector status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsStatus :: Lens.Lens' GetDetectorResponse Types.DetectorStatus
gdrrsStatus = Lens.field @"status"
{-# DEPRECATED gdrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The timestamp of when the detector was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsCreatedAt :: Lens.Lens' GetDetectorResponse (Core.Maybe Types.String)
gdrrsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED gdrrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An object that describes which data sources are enabled for the detector.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDataSources :: Lens.Lens' GetDetectorResponse (Core.Maybe Types.DataSourceConfigurationsResult)
gdrrsDataSources = Lens.field @"dataSources"
{-# DEPRECATED gdrrsDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | The publishing frequency of the finding.
--
-- /Note:/ Consider using 'findingPublishingFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsFindingPublishingFrequency :: Lens.Lens' GetDetectorResponse (Core.Maybe Types.FindingPublishingFrequency)
gdrrsFindingPublishingFrequency = Lens.field @"findingPublishingFrequency"
{-# DEPRECATED gdrrsFindingPublishingFrequency "Use generic-lens or generic-optics with 'findingPublishingFrequency' instead." #-}

-- | The tags of the detector resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsTags :: Lens.Lens' GetDetectorResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gdrrsTags = Lens.field @"tags"
{-# DEPRECATED gdrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The last-updated timestamp for the detector.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsUpdatedAt :: Lens.Lens' GetDetectorResponse (Core.Maybe Types.String)
gdrrsUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED gdrrsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDetectorResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
