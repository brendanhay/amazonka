{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateGeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'GeoMatchConstraint' objects in an @GeoMatchSet@ . For each @GeoMatchConstraint@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change an @GeoMatchConstraint@ object, you delete the existing object and add a new one.
--
--
--     * The @Type@ . The only valid value for @Type@ is @Country@ .
--
--
--     * The @Value@ , which is a two character code for the country to add to the @GeoMatchConstraint@ object. Valid codes are listed in 'GeoMatchConstraint$Value' .
--
--
-- To create and configure an @GeoMatchSet@ , perform the following steps:
--
--     * Submit a 'CreateGeoMatchSet' request.
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateGeoMatchSet' request.
--
--
--     * Submit an @UpdateGeoMatchSet@ request to specify the country that you want AWS WAF to watch for.
--
--
-- When you update an @GeoMatchSet@ , you specify the country that you want to add and/or the country that you want to delete. If you want to change a country, you delete the existing country and add the new one.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateGeoMatchSet
  ( -- * Creating a request
    UpdateGeoMatchSet (..),
    mkUpdateGeoMatchSet,

    -- ** Request lenses
    ugmsGeoMatchSetId,
    ugmsChangeToken,
    ugmsUpdates,

    -- * Destructuring the response
    UpdateGeoMatchSetResponse (..),
    mkUpdateGeoMatchSetResponse,

    -- ** Response lenses
    ugmsrrsChangeToken,
    ugmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkUpdateGeoMatchSet' smart constructor.
data UpdateGeoMatchSet = UpdateGeoMatchSet'
  { -- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to update. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
    geoMatchSetId :: Types.ResourceId,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Types.ChangeToken,
    -- | An array of @GeoMatchSetUpdate@ objects that you want to insert into or delete from an 'GeoMatchSet' . For more information, see the applicable data types:
    --
    --
    --     * 'GeoMatchSetUpdate' : Contains @Action@ and @GeoMatchConstraint@
    --
    --
    --     * 'GeoMatchConstraint' : Contains @Type@ and @Value@
    -- You can have only one @Type@ and @Value@ per @GeoMatchConstraint@ . To add multiple countries, include multiple @GeoMatchSetUpdate@ objects in your request.
    updates :: Core.NonEmpty Types.GeoMatchSetUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGeoMatchSet' value with any optional fields omitted.
mkUpdateGeoMatchSet ::
  -- | 'geoMatchSetId'
  Types.ResourceId ->
  -- | 'changeToken'
  Types.ChangeToken ->
  -- | 'updates'
  Core.NonEmpty Types.GeoMatchSetUpdate ->
  UpdateGeoMatchSet
mkUpdateGeoMatchSet geoMatchSetId changeToken updates =
  UpdateGeoMatchSet' {geoMatchSetId, changeToken, updates}

-- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to update. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsGeoMatchSetId :: Lens.Lens' UpdateGeoMatchSet Types.ResourceId
ugmsGeoMatchSetId = Lens.field @"geoMatchSetId"
{-# DEPRECATED ugmsGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsChangeToken :: Lens.Lens' UpdateGeoMatchSet Types.ChangeToken
ugmsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED ugmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | An array of @GeoMatchSetUpdate@ objects that you want to insert into or delete from an 'GeoMatchSet' . For more information, see the applicable data types:
--
--
--     * 'GeoMatchSetUpdate' : Contains @Action@ and @GeoMatchConstraint@
--
--
--     * 'GeoMatchConstraint' : Contains @Type@ and @Value@
-- You can have only one @Type@ and @Value@ per @GeoMatchConstraint@ . To add multiple countries, include multiple @GeoMatchSetUpdate@ objects in your request.
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsUpdates :: Lens.Lens' UpdateGeoMatchSet (Core.NonEmpty Types.GeoMatchSetUpdate)
ugmsUpdates = Lens.field @"updates"
{-# DEPRECATED ugmsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

instance Core.FromJSON UpdateGeoMatchSet where
  toJSON UpdateGeoMatchSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GeoMatchSetId" Core..= geoMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken),
            Core.Just ("Updates" Core..= updates)
          ]
      )

instance Core.AWSRequest UpdateGeoMatchSet where
  type Rs UpdateGeoMatchSet = UpdateGeoMatchSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.UpdateGeoMatchSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGeoMatchSetResponse'
            Core.<$> (x Core..:? "ChangeToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateGeoMatchSetResponse' smart constructor.
data UpdateGeoMatchSetResponse = UpdateGeoMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Core.Maybe Types.ChangeToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGeoMatchSetResponse' value with any optional fields omitted.
mkUpdateGeoMatchSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGeoMatchSetResponse
mkUpdateGeoMatchSetResponse responseStatus =
  UpdateGeoMatchSetResponse'
    { changeToken = Core.Nothing,
      responseStatus
    }

-- | The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsrrsChangeToken :: Lens.Lens' UpdateGeoMatchSetResponse (Core.Maybe Types.ChangeToken)
ugmsrrsChangeToken = Lens.field @"changeToken"
{-# DEPRECATED ugmsrrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsrrsResponseStatus :: Lens.Lens' UpdateGeoMatchSetResponse Core.Int
ugmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
