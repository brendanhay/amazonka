{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the IPSet specified by the IPSet ID.
module Network.AWS.GuardDuty.UpdateIPSet
    (
    -- * Creating a request
      UpdateIPSet (..)
    , mkUpdateIPSet
    -- ** Request lenses
    , uipsDetectorId
    , uipsIpSetId
    , uipsActivate
    , uipsLocation
    , uipsName

    -- * Destructuring the response
    , UpdateIPSetResponse (..)
    , mkUpdateIPSetResponse
    -- ** Response lenses
    , uipsrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { detectorId :: Types.DetectorId
    -- ^ The detectorID that specifies the GuardDuty service whose IPSet you want to update.
  , ipSetId :: Core.Text
    -- ^ The unique ID that specifies the IPSet that you want to update.
  , activate :: Core.Maybe Core.Bool
    -- ^ The updated Boolean value that specifies whether the IPSet is active or not.
  , location :: Core.Maybe Types.Location
    -- ^ The updated URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
  , name :: Core.Maybe Types.Name
    -- ^ The unique ID that specifies the IPSet that you want to update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIPSet' value with any optional fields omitted.
mkUpdateIPSet
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'ipSetId'
    -> UpdateIPSet
mkUpdateIPSet detectorId ipSetId
  = UpdateIPSet'{detectorId, ipSetId, activate = Core.Nothing,
                 location = Core.Nothing, name = Core.Nothing}

-- | The detectorID that specifies the GuardDuty service whose IPSet you want to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsDetectorId :: Lens.Lens' UpdateIPSet Types.DetectorId
uipsDetectorId = Lens.field @"detectorId"
{-# INLINEABLE uipsDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The unique ID that specifies the IPSet that you want to update.
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsIpSetId :: Lens.Lens' UpdateIPSet Core.Text
uipsIpSetId = Lens.field @"ipSetId"
{-# INLINEABLE uipsIpSetId #-}
{-# DEPRECATED ipSetId "Use generic-lens or generic-optics with 'ipSetId' instead"  #-}

-- | The updated Boolean value that specifies whether the IPSet is active or not.
--
-- /Note:/ Consider using 'activate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsActivate :: Lens.Lens' UpdateIPSet (Core.Maybe Core.Bool)
uipsActivate = Lens.field @"activate"
{-# INLINEABLE uipsActivate #-}
{-# DEPRECATED activate "Use generic-lens or generic-optics with 'activate' instead"  #-}

-- | The updated URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsLocation :: Lens.Lens' UpdateIPSet (Core.Maybe Types.Location)
uipsLocation = Lens.field @"location"
{-# INLINEABLE uipsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The unique ID that specifies the IPSet that you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsName :: Lens.Lens' UpdateIPSet (Core.Maybe Types.Name)
uipsName = Lens.field @"name"
{-# INLINEABLE uipsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateIPSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateIPSet where
        toHeaders UpdateIPSet{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateIPSet where
        toJSON UpdateIPSet{..}
          = Core.object
              (Core.catMaybes
                 [("activate" Core..=) Core.<$> activate,
                  ("location" Core..=) Core.<$> location,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateIPSet where
        type Rs UpdateIPSet = UpdateIPSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/ipset/"
                             Core.<> Core.toText ipSetId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateIPSetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateIPSetResponse' smart constructor.
newtype UpdateIPSetResponse = UpdateIPSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateIPSetResponse' value with any optional fields omitted.
mkUpdateIPSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateIPSetResponse
mkUpdateIPSetResponse responseStatus
  = UpdateIPSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipsrrsResponseStatus :: Lens.Lens' UpdateIPSetResponse Core.Int
uipsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uipsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
