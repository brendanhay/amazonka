{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RegexMatchSet' . You can't delete a @RegexMatchSet@ if it's still used in any @Rules@ or if it still includes any @RegexMatchTuples@ objects (any filters).
--
-- If you just want to remove a @RegexMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @RegexMatchSet@ , perform the following steps:
--
--     * Update the @RegexMatchSet@ to remove filters, if any. For more information, see 'UpdateRegexMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRegexMatchSet@ request.
--
--
--     * Submit a @DeleteRegexMatchSet@ request.
module Network.AWS.WAF.DeleteRegexMatchSet
  ( -- * Creating a request
    DeleteRegexMatchSet (..),
    mkDeleteRegexMatchSet,

    -- ** Request lenses
    drmsRegexMatchSetId,
    drmsChangeToken,

    -- * Destructuring the response
    DeleteRegexMatchSetResponse (..),
    mkDeleteRegexMatchSetResponse,

    -- ** Response lenses
    drmsrsChangeToken,
    drmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkDeleteRegexMatchSet' smart constructor.
data DeleteRegexMatchSet = DeleteRegexMatchSet'
  { regexMatchSetId ::
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

-- | Creates a value of 'DeleteRegexMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'regexMatchSetId' - The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
mkDeleteRegexMatchSet ::
  -- | 'regexMatchSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteRegexMatchSet
mkDeleteRegexMatchSet pRegexMatchSetId_ pChangeToken_ =
  DeleteRegexMatchSet'
    { regexMatchSetId = pRegexMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsRegexMatchSetId :: Lens.Lens' DeleteRegexMatchSet Lude.Text
drmsRegexMatchSetId = Lens.lens (regexMatchSetId :: DeleteRegexMatchSet -> Lude.Text) (\s a -> s {regexMatchSetId = a} :: DeleteRegexMatchSet)
{-# DEPRECATED drmsRegexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsChangeToken :: Lens.Lens' DeleteRegexMatchSet Lude.Text
drmsChangeToken = Lens.lens (changeToken :: DeleteRegexMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteRegexMatchSet)
{-# DEPRECATED drmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteRegexMatchSet where
  type Rs DeleteRegexMatchSet = DeleteRegexMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRegexMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRegexMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteRegexMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRegexMatchSet where
  toJSON DeleteRegexMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RegexMatchSetId" Lude..= regexMatchSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteRegexMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRegexMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRegexMatchSetResponse' smart constructor.
data DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse'
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

-- | Creates a value of 'DeleteRegexMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteRegexMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRegexMatchSetResponse
mkDeleteRegexMatchSetResponse pResponseStatus_ =
  DeleteRegexMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsrsChangeToken :: Lens.Lens' DeleteRegexMatchSetResponse (Lude.Maybe Lude.Text)
drmsrsChangeToken = Lens.lens (changeToken :: DeleteRegexMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteRegexMatchSetResponse)
{-# DEPRECATED drmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drmsrsResponseStatus :: Lens.Lens' DeleteRegexMatchSetResponse Lude.Int
drmsrsResponseStatus = Lens.lens (responseStatus :: DeleteRegexMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRegexMatchSetResponse)
{-# DEPRECATED drmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
