{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateThreatIntelSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the ThreatIntelSet specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.UpdateThreatIntelSet
    (
    -- * Creating a request
      UpdateThreatIntelSet (..)
    , mkUpdateThreatIntelSet
    -- ** Request lenses
    , utisDetectorId
    , utisThreatIntelSetId
    , utisActivate
    , utisLocation
    , utisName

    -- * Destructuring the response
    , UpdateThreatIntelSetResponse (..)
    , mkUpdateThreatIntelSetResponse
    -- ** Response lenses
    , utisrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateThreatIntelSet' smart constructor.
data UpdateThreatIntelSet = UpdateThreatIntelSet'
  { detectorId :: Types.DetectorId
    -- ^ The detectorID that specifies the GuardDuty service whose ThreatIntelSet you want to update.
  , threatIntelSetId :: Core.Text
    -- ^ The unique ID that specifies the ThreatIntelSet that you want to update.
  , activate :: Core.Maybe Core.Bool
    -- ^ The updated Boolean value that specifies whether the ThreateIntelSet is active or not.
  , location :: Core.Maybe Types.Location
    -- ^ The updated URI of the file that contains the ThreateIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
  , name :: Core.Maybe Types.Name
    -- ^ The unique ID that specifies the ThreatIntelSet that you want to update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThreatIntelSet' value with any optional fields omitted.
mkUpdateThreatIntelSet
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'threatIntelSetId'
    -> UpdateThreatIntelSet
mkUpdateThreatIntelSet detectorId threatIntelSetId
  = UpdateThreatIntelSet'{detectorId, threatIntelSetId,
                          activate = Core.Nothing, location = Core.Nothing,
                          name = Core.Nothing}

-- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet you want to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisDetectorId :: Lens.Lens' UpdateThreatIntelSet Types.DetectorId
utisDetectorId = Lens.field @"detectorId"
{-# INLINEABLE utisDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
--
-- /Note:/ Consider using 'threatIntelSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisThreatIntelSetId :: Lens.Lens' UpdateThreatIntelSet Core.Text
utisThreatIntelSetId = Lens.field @"threatIntelSetId"
{-# INLINEABLE utisThreatIntelSetId #-}
{-# DEPRECATED threatIntelSetId "Use generic-lens or generic-optics with 'threatIntelSetId' instead"  #-}

-- | The updated Boolean value that specifies whether the ThreateIntelSet is active or not.
--
-- /Note:/ Consider using 'activate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisActivate :: Lens.Lens' UpdateThreatIntelSet (Core.Maybe Core.Bool)
utisActivate = Lens.field @"activate"
{-# INLINEABLE utisActivate #-}
{-# DEPRECATED activate "Use generic-lens or generic-optics with 'activate' instead"  #-}

-- | The updated URI of the file that contains the ThreateIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisLocation :: Lens.Lens' UpdateThreatIntelSet (Core.Maybe Types.Location)
utisLocation = Lens.field @"location"
{-# INLINEABLE utisLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisName :: Lens.Lens' UpdateThreatIntelSet (Core.Maybe Types.Name)
utisName = Lens.field @"name"
{-# INLINEABLE utisName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateThreatIntelSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateThreatIntelSet where
        toHeaders UpdateThreatIntelSet{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateThreatIntelSet where
        toJSON UpdateThreatIntelSet{..}
          = Core.object
              (Core.catMaybes
                 [("activate" Core..=) Core.<$> activate,
                  ("location" Core..=) Core.<$> location,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateThreatIntelSet where
        type Rs UpdateThreatIntelSet = UpdateThreatIntelSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/threatintelset/"
                             Core.<> Core.toText threatIntelSetId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateThreatIntelSetResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateThreatIntelSetResponse' smart constructor.
newtype UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThreatIntelSetResponse' value with any optional fields omitted.
mkUpdateThreatIntelSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateThreatIntelSetResponse
mkUpdateThreatIntelSetResponse responseStatus
  = UpdateThreatIntelSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utisrrsResponseStatus :: Lens.Lens' UpdateThreatIntelSetResponse Core.Int
utisrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utisrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
