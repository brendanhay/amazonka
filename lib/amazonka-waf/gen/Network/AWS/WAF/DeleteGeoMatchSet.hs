{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dgmsrsChangeToken,
    dgmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkDeleteGeoMatchSet' smart constructor.
data DeleteGeoMatchSet = DeleteGeoMatchSet'
  { geoMatchSetId ::
      Lude.Text,
    changeToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGeoMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'geoMatchSetId' - The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
mkDeleteGeoMatchSet ::
  -- | 'geoMatchSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteGeoMatchSet
mkDeleteGeoMatchSet pGeoMatchSetId_ pChangeToken_ =
  DeleteGeoMatchSet'
    { geoMatchSetId = pGeoMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsGeoMatchSetId :: Lens.Lens' DeleteGeoMatchSet Lude.Text
dgmsGeoMatchSetId = Lens.lens (geoMatchSetId :: DeleteGeoMatchSet -> Lude.Text) (\s a -> s {geoMatchSetId = a} :: DeleteGeoMatchSet)
{-# DEPRECATED dgmsGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsChangeToken :: Lens.Lens' DeleteGeoMatchSet Lude.Text
dgmsChangeToken = Lens.lens (changeToken :: DeleteGeoMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteGeoMatchSet)
{-# DEPRECATED dgmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteGeoMatchSet where
  type Rs DeleteGeoMatchSet = DeleteGeoMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteGeoMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGeoMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteGeoMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteGeoMatchSet where
  toJSON DeleteGeoMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GeoMatchSetId" Lude..= geoMatchSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteGeoMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGeoMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGeoMatchSetResponse' smart constructor.
data DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse'
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

-- | Creates a value of 'DeleteGeoMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteGeoMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGeoMatchSetResponse
mkDeleteGeoMatchSetResponse pResponseStatus_ =
  DeleteGeoMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsrsChangeToken :: Lens.Lens' DeleteGeoMatchSetResponse (Lude.Maybe Lude.Text)
dgmsrsChangeToken = Lens.lens (changeToken :: DeleteGeoMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteGeoMatchSetResponse)
{-# DEPRECATED dgmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgmsrsResponseStatus :: Lens.Lens' DeleteGeoMatchSetResponse Lude.Int
dgmsrsResponseStatus = Lens.lens (responseStatus :: DeleteGeoMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGeoMatchSetResponse)
{-# DEPRECATED dgmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
