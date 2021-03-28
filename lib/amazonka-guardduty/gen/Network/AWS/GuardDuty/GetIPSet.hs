{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the IPSet specified by the @ipSetId@ .
module Network.AWS.GuardDuty.GetIPSet
    (
    -- * Creating a request
      GetIPSet (..)
    , mkGetIPSet
    -- ** Request lenses
    , gipsDetectorId
    , gipsIpSetId

    -- * Destructuring the response
    , GetIPSetResponse (..)
    , mkGetIPSetResponse
    -- ** Response lenses
    , gipsrrsName
    , gipsrrsFormat
    , gipsrrsLocation
    , gipsrrsStatus
    , gipsrrsTags
    , gipsrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIPSet' smart constructor.
data GetIPSet = GetIPSet'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector that the IPSet is associated with.
  , ipSetId :: Core.Text
    -- ^ The unique ID of the IPSet to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIPSet' value with any optional fields omitted.
mkGetIPSet
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'ipSetId'
    -> GetIPSet
mkGetIPSet detectorId ipSetId = GetIPSet'{detectorId, ipSetId}

-- | The unique ID of the detector that the IPSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsDetectorId :: Lens.Lens' GetIPSet Types.DetectorId
gipsDetectorId = Lens.field @"detectorId"
{-# INLINEABLE gipsDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The unique ID of the IPSet to retrieve.
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsIpSetId :: Lens.Lens' GetIPSet Core.Text
gipsIpSetId = Lens.field @"ipSetId"
{-# INLINEABLE gipsIpSetId #-}
{-# DEPRECATED ipSetId "Use generic-lens or generic-optics with 'ipSetId' instead"  #-}

instance Core.ToQuery GetIPSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetIPSet where
        toHeaders GetIPSet{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetIPSet where
        type Rs GetIPSet = GetIPSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/ipset/"
                             Core.<> Core.toText ipSetId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetIPSetResponse' Core.<$>
                   (x Core..: "name") Core.<*> x Core..: "format" Core.<*>
                     x Core..: "location"
                     Core.<*> x Core..: "status"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { name :: Types.Name
    -- ^ The user-friendly name for the IPSet.
  , format :: Types.IpSetFormat
    -- ^ The format of the file that contains the IPSet.
  , location :: Types.Location
    -- ^ The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
  , status :: Types.IpSetStatus
    -- ^ The status of IPSet file that was uploaded.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags of the IPSet resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIPSetResponse' value with any optional fields omitted.
mkGetIPSetResponse
    :: Types.Name -- ^ 'name'
    -> Types.IpSetFormat -- ^ 'format'
    -> Types.Location -- ^ 'location'
    -> Types.IpSetStatus -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> GetIPSetResponse
mkGetIPSetResponse name format location status responseStatus
  = GetIPSetResponse'{name, format, location, status,
                      tags = Core.Nothing, responseStatus}

-- | The user-friendly name for the IPSet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsName :: Lens.Lens' GetIPSetResponse Types.Name
gipsrrsName = Lens.field @"name"
{-# INLINEABLE gipsrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The format of the file that contains the IPSet.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsFormat :: Lens.Lens' GetIPSetResponse Types.IpSetFormat
gipsrrsFormat = Lens.field @"format"
{-# INLINEABLE gipsrrsFormat #-}
{-# DEPRECATED format "Use generic-lens or generic-optics with 'format' instead"  #-}

-- | The URI of the file that contains the IPSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsLocation :: Lens.Lens' GetIPSetResponse Types.Location
gipsrrsLocation = Lens.field @"location"
{-# INLINEABLE gipsrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The status of IPSet file that was uploaded.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsStatus :: Lens.Lens' GetIPSetResponse Types.IpSetStatus
gipsrrsStatus = Lens.field @"status"
{-# INLINEABLE gipsrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The tags of the IPSet resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsTags :: Lens.Lens' GetIPSetResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gipsrrsTags = Lens.field @"tags"
{-# INLINEABLE gipsrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipsrrsResponseStatus :: Lens.Lens' GetIPSetResponse Core.Int
gipsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gipsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
