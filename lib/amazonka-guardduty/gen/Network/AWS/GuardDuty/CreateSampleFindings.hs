{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateSampleFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates example findings of types specified by the list of finding types. If 'NULL' is specified for @findingTypes@ , the API generates example findings of all supported finding types.
module Network.AWS.GuardDuty.CreateSampleFindings
    (
    -- * Creating a request
      CreateSampleFindings (..)
    , mkCreateSampleFindings
    -- ** Request lenses
    , csfDetectorId
    , csfFindingTypes

    -- * Destructuring the response
    , CreateSampleFindingsResponse (..)
    , mkCreateSampleFindingsResponse
    -- ** Response lenses
    , csfrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSampleFindings' smart constructor.
data CreateSampleFindings = CreateSampleFindings'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector to create sample findings for.
  , findingTypes :: Core.Maybe [Types.FindingType]
    -- ^ The types of sample findings to generate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSampleFindings' value with any optional fields omitted.
mkCreateSampleFindings
    :: Types.DetectorId -- ^ 'detectorId'
    -> CreateSampleFindings
mkCreateSampleFindings detectorId
  = CreateSampleFindings'{detectorId, findingTypes = Core.Nothing}

-- | The ID of the detector to create sample findings for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDetectorId :: Lens.Lens' CreateSampleFindings Types.DetectorId
csfDetectorId = Lens.field @"detectorId"
{-# INLINEABLE csfDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The types of sample findings to generate.
--
-- /Note:/ Consider using 'findingTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfFindingTypes :: Lens.Lens' CreateSampleFindings (Core.Maybe [Types.FindingType])
csfFindingTypes = Lens.field @"findingTypes"
{-# INLINEABLE csfFindingTypes #-}
{-# DEPRECATED findingTypes "Use generic-lens or generic-optics with 'findingTypes' instead"  #-}

instance Core.ToQuery CreateSampleFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSampleFindings where
        toHeaders CreateSampleFindings{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSampleFindings where
        toJSON CreateSampleFindings{..}
          = Core.object
              (Core.catMaybes [("findingTypes" Core..=) Core.<$> findingTypes])

instance Core.AWSRequest CreateSampleFindings where
        type Rs CreateSampleFindings = CreateSampleFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/findings/create",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateSampleFindingsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSampleFindingsResponse' smart constructor.
newtype CreateSampleFindingsResponse = CreateSampleFindingsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSampleFindingsResponse' value with any optional fields omitted.
mkCreateSampleFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSampleFindingsResponse
mkCreateSampleFindingsResponse responseStatus
  = CreateSampleFindingsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrrsResponseStatus :: Lens.Lens' CreateSampleFindingsResponse Core.Int
csfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
