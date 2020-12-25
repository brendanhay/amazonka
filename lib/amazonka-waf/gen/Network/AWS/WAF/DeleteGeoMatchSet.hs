{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.WAF.DeleteGeoMatchSet
  ( -- * Creating a request
    DeleteGeoMatchSet (..),
    mkDeleteGeoMatchSet,

    -- ** Request lenses
    dgmsGeoMatchSetId,
    dgmsChangeToken,

    -- * Destructuring the response
    DeleteGeoMatchSetResponse (..),
    mkDeleteGeoMatchSetResponse,

    -- ** Response lenses
    dgmsrrsChangeToken,
    dgmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkDeleteGeoMatchSet' smart constructor.
data DeleteGeoMatchSet = DeleteGeoMatchSet'
  { -- | The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
    geoMatchSetId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGeoMatchSet' value with any optional fields omitted.
mkDeleteGeoMatchSet ::
  -- | 'geoMatchSetId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  DeleteGeoMatchSet
mkDeleteGeoMatchSet geoMatchSetId changeToken =
  DeleteGeoMatchSet' {geoMatchSetId, changeToken}

-- | The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsGeoMatchSetId :: Lens.Lens' DeleteGeoMatchSet Types.ResourceId
dgmsGeoMatchSetId = Lens.field @"geoMatchSetId"
{-# DEPRECATED dgmsGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsChangeToken :: Lens.Lens' DeleteGeoMatchSet Types.ChangeToken
dgmsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dgmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Core.FromJSON DeleteGeoMatchSet where
  toJSON DeleteGeoMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GeoMatchSetId" Core..= geoMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.AWSRequest DeleteGeoMatchSet where
  type Rs DeleteGeoMatchSet = DeleteGeoMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.DeleteGeoMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGeoMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteGeoMatchSetResponse' smart constructor.
data DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGeoMatchSetResponse' value with any optional fields omitted.
mkDeleteGeoMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteGeoMatchSetResponse
mkDeleteGeoMatchSetResponse responseStatus =
  DeleteGeoMatchSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsrrsChangeToken :: Lens.Lens' DeleteGeoMatchSetResponse (Core.Maybe Types.ChangeToken)
dgmsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED dgmsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsrrsResponseStatus :: Lens.Lens' DeleteGeoMatchSetResponse Core.Int
dgmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
