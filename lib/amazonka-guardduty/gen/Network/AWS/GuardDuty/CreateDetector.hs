{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a single Amazon GuardDuty detector. A detector is a resource that represents the GuardDuty service. To start using GuardDuty, you must create a detector in each Region where you enable the service. You can have only one detector per account per Region. All data sources are enabled in a new detector by default.
module Network.AWS.GuardDuty.CreateDetector
    (
    -- * Creating a request
      CreateDetector (..)
    , mkCreateDetector
    -- ** Request lenses
    , cdEnable
    , cdClientToken
    , cdDataSources
    , cdFindingPublishingFrequency
    , cdTags

    -- * Destructuring the response
    , CreateDetectorResponse (..)
    , mkCreateDetectorResponse
    -- ** Response lenses
    , cdrrsDetectorId
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDetector' smart constructor.
data CreateDetector = CreateDetector'
  { enable :: Core.Bool
    -- ^ A Boolean value that specifies whether the detector is to be enabled.
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ The idempotency token for the create request.
  , dataSources :: Core.Maybe Types.DataSourceConfigurations
    -- ^ An object that describes which data sources will be enabled for the detector.
  , findingPublishingFrequency :: Core.Maybe Types.FindingPublishingFrequency
    -- ^ An enum value that specifies how frequently updated findings are exported.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags to be added to a new detector resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDetector' value with any optional fields omitted.
mkCreateDetector
    :: Core.Bool -- ^ 'enable'
    -> CreateDetector
mkCreateDetector enable
  = CreateDetector'{enable, clientToken = Core.Nothing,
                    dataSources = Core.Nothing,
                    findingPublishingFrequency = Core.Nothing, tags = Core.Nothing}

-- | A Boolean value that specifies whether the detector is to be enabled.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEnable :: Lens.Lens' CreateDetector Core.Bool
cdEnable = Lens.field @"enable"
{-# INLINEABLE cdEnable #-}
{-# DEPRECATED enable "Use generic-lens or generic-optics with 'enable' instead"  #-}

-- | The idempotency token for the create request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdClientToken :: Lens.Lens' CreateDetector (Core.Maybe Types.ClientToken)
cdClientToken = Lens.field @"clientToken"
{-# INLINEABLE cdClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | An object that describes which data sources will be enabled for the detector.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDataSources :: Lens.Lens' CreateDetector (Core.Maybe Types.DataSourceConfigurations)
cdDataSources = Lens.field @"dataSources"
{-# INLINEABLE cdDataSources #-}
{-# DEPRECATED dataSources "Use generic-lens or generic-optics with 'dataSources' instead"  #-}

-- | An enum value that specifies how frequently updated findings are exported.
--
-- /Note:/ Consider using 'findingPublishingFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFindingPublishingFrequency :: Lens.Lens' CreateDetector (Core.Maybe Types.FindingPublishingFrequency)
cdFindingPublishingFrequency = Lens.field @"findingPublishingFrequency"
{-# INLINEABLE cdFindingPublishingFrequency #-}
{-# DEPRECATED findingPublishingFrequency "Use generic-lens or generic-optics with 'findingPublishingFrequency' instead"  #-}

-- | The tags to be added to a new detector resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDetector (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cdTags = Lens.field @"tags"
{-# INLINEABLE cdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDetector where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDetector where
        toHeaders CreateDetector{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDetector where
        toJSON CreateDetector{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("enable" Core..= enable),
                  ("clientToken" Core..=) Core.<$> clientToken,
                  ("dataSources" Core..=) Core.<$> dataSources,
                  ("findingPublishingFrequency" Core..=) Core.<$>
                    findingPublishingFrequency,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDetector where
        type Rs CreateDetector = CreateDetectorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/detector",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDetectorResponse' Core.<$>
                   (x Core..:? "detectorId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDetectorResponse' smart constructor.
data CreateDetectorResponse = CreateDetectorResponse'
  { detectorId :: Core.Maybe Types.DetectorId
    -- ^ The unique ID of the created detector.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDetectorResponse' value with any optional fields omitted.
mkCreateDetectorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDetectorResponse
mkCreateDetectorResponse responseStatus
  = CreateDetectorResponse'{detectorId = Core.Nothing,
                            responseStatus}

-- | The unique ID of the created detector.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDetectorId :: Lens.Lens' CreateDetectorResponse (Core.Maybe Types.DetectorId)
cdrrsDetectorId = Lens.field @"detectorId"
{-# INLINEABLE cdrrsDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDetectorResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
