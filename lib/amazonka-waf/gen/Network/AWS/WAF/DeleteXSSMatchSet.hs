{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteXSSMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an 'XssMatchSet' . You can't delete an @XssMatchSet@ if it's still used in any @Rules@ or if it still contains any 'XssMatchTuple' objects.
--
-- If you just want to remove an @XssMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete an @XssMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @XssMatchSet@ to remove filters, if any. For more information, see 'UpdateXssMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteXssMatchSet@ request.
--
--
--     * Submit a @DeleteXssMatchSet@ request.
module Network.AWS.WAF.DeleteXSSMatchSet
  ( -- * Creating a request
    DeleteXSSMatchSet (..),
    mkDeleteXSSMatchSet,

    -- ** Request lenses
    dxmsXSSMatchSetId,
    dxmsChangeToken,

    -- * Destructuring the response
    DeleteXSSMatchSetResponse (..),
    mkDeleteXSSMatchSetResponse,

    -- ** Response lenses
    dxmsrsChangeToken,
    dxmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | A request to delete an 'XssMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteXSSMatchSet' smart constructor.
data DeleteXSSMatchSet = DeleteXSSMatchSet'
  { -- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to delete. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
    xssMatchSetId :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteXSSMatchSet' with the minimum fields required to make a request.
--
-- * 'xssMatchSetId' - The @XssMatchSetId@ of the 'XssMatchSet' that you want to delete. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkDeleteXSSMatchSet ::
  -- | 'xssMatchSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteXSSMatchSet
mkDeleteXSSMatchSet pXSSMatchSetId_ pChangeToken_ =
  DeleteXSSMatchSet'
    { xssMatchSetId = pXSSMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to delete. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsXSSMatchSetId :: Lens.Lens' DeleteXSSMatchSet Lude.Text
dxmsXSSMatchSetId = Lens.lens (xssMatchSetId :: DeleteXSSMatchSet -> Lude.Text) (\s a -> s {xssMatchSetId = a} :: DeleteXSSMatchSet)
{-# DEPRECATED dxmsXSSMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsChangeToken :: Lens.Lens' DeleteXSSMatchSet Lude.Text
dxmsChangeToken = Lens.lens (changeToken :: DeleteXSSMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteXSSMatchSet)
{-# DEPRECATED dxmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteXSSMatchSet where
  type Rs DeleteXSSMatchSet = DeleteXSSMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteXSSMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteXSSMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteXssMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteXSSMatchSet where
  toJSON DeleteXSSMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("XssMatchSetId" Lude..= xssMatchSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteXSSMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteXSSMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to a request to delete an 'XssMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteXSSMatchSetResponse' smart constructor.
data DeleteXSSMatchSetResponse = DeleteXSSMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteXSSMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteXSSMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteXSSMatchSetResponse
mkDeleteXSSMatchSetResponse pResponseStatus_ =
  DeleteXSSMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsrsChangeToken :: Lens.Lens' DeleteXSSMatchSetResponse (Lude.Maybe Lude.Text)
dxmsrsChangeToken = Lens.lens (changeToken :: DeleteXSSMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteXSSMatchSetResponse)
{-# DEPRECATED dxmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dxmsrsResponseStatus :: Lens.Lens' DeleteXSSMatchSetResponse Lude.Int
dxmsrsResponseStatus = Lens.lens (responseStatus :: DeleteXSSMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteXSSMatchSetResponse)
{-# DEPRECATED dxmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
