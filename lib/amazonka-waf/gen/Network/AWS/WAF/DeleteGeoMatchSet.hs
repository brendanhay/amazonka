{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteGeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'GeoMatchSet' . You can't delete a @GeoMatchSet@ if it's still used in any @Rules@ or if it still includes any countries.
--
-- If you just want to remove a @GeoMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @GeoMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @GeoMatchSet@ to remove any countries. For more information, see 'UpdateGeoMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteGeoMatchSet@ request.
--
--
--     * Submit a @DeleteGeoMatchSet@ request.
--
--
module Network.AWS.WAF.DeleteGeoMatchSet
    (
    -- * Creating a request
      DeleteGeoMatchSet (..)
    , mkDeleteGeoMatchSet
    -- ** Request lenses
    , dgmsGeoMatchSetId
    , dgmsChangeToken

    -- * Destructuring the response
    , DeleteGeoMatchSetResponse (..)
    , mkDeleteGeoMatchSetResponse
    -- ** Response lenses
    , dgmsrrsChangeToken
    , dgmsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkDeleteGeoMatchSet' smart constructor.
data DeleteGeoMatchSet = DeleteGeoMatchSet'
  { geoMatchSetId :: Types.ResourceId
    -- ^ The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGeoMatchSet' value with any optional fields omitted.
mkDeleteGeoMatchSet
    :: Types.ResourceId -- ^ 'geoMatchSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteGeoMatchSet
mkDeleteGeoMatchSet geoMatchSetId changeToken
  = DeleteGeoMatchSet'{geoMatchSetId, changeToken}

-- | The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsGeoMatchSetId :: Lens.Lens' DeleteGeoMatchSet Types.ResourceId
dgmsGeoMatchSetId = Lens.field @"geoMatchSetId"
{-# INLINEABLE dgmsGeoMatchSetId #-}
{-# DEPRECATED geoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsChangeToken :: Lens.Lens' DeleteGeoMatchSet Types.ChangeToken
dgmsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dgmsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteGeoMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteGeoMatchSet where
        toHeaders DeleteGeoMatchSet{..}
          = Core.pure ("X-Amz-Target", "AWSWAF_20150824.DeleteGeoMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteGeoMatchSet where
        toJSON DeleteGeoMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GeoMatchSetId" Core..= geoMatchSetId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteGeoMatchSet where
        type Rs DeleteGeoMatchSet = DeleteGeoMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteGeoMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGeoMatchSetResponse' smart constructor.
data DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGeoMatchSetResponse' value with any optional fields omitted.
mkDeleteGeoMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteGeoMatchSetResponse
mkDeleteGeoMatchSetResponse responseStatus
  = DeleteGeoMatchSetResponse'{changeToken = Core.Nothing,
                               responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsrrsChangeToken :: Lens.Lens' DeleteGeoMatchSetResponse (Core.Maybe Types.ChangeToken)
dgmsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dgmsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsrrsResponseStatus :: Lens.Lens' DeleteGeoMatchSetResponse Core.Int
dgmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
