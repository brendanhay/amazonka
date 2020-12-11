{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ugmsrsChangeToken,
    ugmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkUpdateGeoMatchSet' smart constructor.
data UpdateGeoMatchSet = UpdateGeoMatchSet'
  { geoMatchSetId ::
      Lude.Text,
    changeToken :: Lude.Text,
    updates :: Lude.NonEmpty GeoMatchSetUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGeoMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'geoMatchSetId' - The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to update. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
-- * 'updates' - An array of @GeoMatchSetUpdate@ objects that you want to insert into or delete from an 'GeoMatchSet' . For more information, see the applicable data types:
--
--
--     * 'GeoMatchSetUpdate' : Contains @Action@ and @GeoMatchConstraint@
--
--
--     * 'GeoMatchConstraint' : Contains @Type@ and @Value@
-- You can have only one @Type@ and @Value@ per @GeoMatchConstraint@ . To add multiple countries, include multiple @GeoMatchSetUpdate@ objects in your request.
mkUpdateGeoMatchSet ::
  -- | 'geoMatchSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  -- | 'updates'
  Lude.NonEmpty GeoMatchSetUpdate ->
  UpdateGeoMatchSet
mkUpdateGeoMatchSet pGeoMatchSetId_ pChangeToken_ pUpdates_ =
  UpdateGeoMatchSet'
    { geoMatchSetId = pGeoMatchSetId_,
      changeToken = pChangeToken_,
      updates = pUpdates_
    }

-- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to update. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsGeoMatchSetId :: Lens.Lens' UpdateGeoMatchSet Lude.Text
ugmsGeoMatchSetId = Lens.lens (geoMatchSetId :: UpdateGeoMatchSet -> Lude.Text) (\s a -> s {geoMatchSetId = a} :: UpdateGeoMatchSet)
{-# DEPRECATED ugmsGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsChangeToken :: Lens.Lens' UpdateGeoMatchSet Lude.Text
ugmsChangeToken = Lens.lens (changeToken :: UpdateGeoMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateGeoMatchSet)
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
ugmsUpdates :: Lens.Lens' UpdateGeoMatchSet (Lude.NonEmpty GeoMatchSetUpdate)
ugmsUpdates = Lens.lens (updates :: UpdateGeoMatchSet -> Lude.NonEmpty GeoMatchSetUpdate) (\s a -> s {updates = a} :: UpdateGeoMatchSet)
{-# DEPRECATED ugmsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

instance Lude.AWSRequest UpdateGeoMatchSet where
  type Rs UpdateGeoMatchSet = UpdateGeoMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGeoMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGeoMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.UpdateGeoMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGeoMatchSet where
  toJSON UpdateGeoMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GeoMatchSetId" Lude..= geoMatchSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken),
            Lude.Just ("Updates" Lude..= updates)
          ]
      )

instance Lude.ToPath UpdateGeoMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGeoMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGeoMatchSetResponse' smart constructor.
data UpdateGeoMatchSetResponse = UpdateGeoMatchSetResponse'
  { changeToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGeoMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateGeoMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGeoMatchSetResponse
mkUpdateGeoMatchSetResponse pResponseStatus_ =
  UpdateGeoMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsrsChangeToken :: Lens.Lens' UpdateGeoMatchSetResponse (Lude.Maybe Lude.Text)
ugmsrsChangeToken = Lens.lens (changeToken :: UpdateGeoMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateGeoMatchSetResponse)
{-# DEPRECATED ugmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugmsrsResponseStatus :: Lens.Lens' UpdateGeoMatchSetResponse Lude.Int
ugmsrsResponseStatus = Lens.lens (responseStatus :: UpdateGeoMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGeoMatchSetResponse)
{-# DEPRECATED ugmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
