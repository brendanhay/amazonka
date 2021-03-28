{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.DeleteTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a trail. This operation must be called from the region in which the trail was created. @DeleteTrail@ cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.
module Network.AWS.CloudTrail.DeleteTrail
    (
    -- * Creating a request
      DeleteTrail (..)
    , mkDeleteTrail
    -- ** Request lenses
    , dtName

    -- * Destructuring the response
    , DeleteTrailResponse (..)
    , mkDeleteTrailResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request that specifies the name of a trail to delete.
--
-- /See:/ 'mkDeleteTrail' smart constructor.
newtype DeleteTrail = DeleteTrail'
  { name :: Core.Text
    -- ^ Specifies the name or the CloudTrail ARN of the trail to be deleted. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrail' value with any optional fields omitted.
mkDeleteTrail
    :: Core.Text -- ^ 'name'
    -> DeleteTrail
mkDeleteTrail name = DeleteTrail'{name}

-- | Specifies the name or the CloudTrail ARN of the trail to be deleted. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@ 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtName :: Lens.Lens' DeleteTrail Core.Text
dtName = Lens.field @"name"
{-# INLINEABLE dtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteTrail where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTrail where
        toHeaders DeleteTrail{..}
          = Core.pure
              ("X-Amz-Target",
               "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DeleteTrail")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTrail where
        toJSON DeleteTrail{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteTrail where
        type Rs DeleteTrail = DeleteTrailResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteTrailResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkDeleteTrailResponse' smart constructor.
newtype DeleteTrailResponse = DeleteTrailResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrailResponse' value with any optional fields omitted.
mkDeleteTrailResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTrailResponse
mkDeleteTrailResponse responseStatus
  = DeleteTrailResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteTrailResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
