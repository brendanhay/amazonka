{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RegexPatternSet' . You can't delete a @RegexPatternSet@ if it's still used in any @RegexMatchSet@ or if the @RegexPatternSet@ is not empty.
module Network.AWS.WAFRegional.DeleteRegexPatternSet
  ( -- * Creating a request
    DeleteRegexPatternSet (..),
    mkDeleteRegexPatternSet,

    -- ** Request lenses
    drpsRegexPatternSetId,
    drpsChangeToken,

    -- * Destructuring the response
    DeleteRegexPatternSetResponse (..),
    mkDeleteRegexPatternSetResponse,

    -- ** Response lenses
    drpsrsChangeToken,
    drpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkDeleteRegexPatternSet' smart constructor.
data DeleteRegexPatternSet = DeleteRegexPatternSet'
  { regexPatternSetId ::
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

-- | Creates a value of 'DeleteRegexPatternSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'regexPatternSetId' - The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
mkDeleteRegexPatternSet ::
  -- | 'regexPatternSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteRegexPatternSet
mkDeleteRegexPatternSet pRegexPatternSetId_ pChangeToken_ =
  DeleteRegexPatternSet'
    { regexPatternSetId = pRegexPatternSetId_,
      changeToken = pChangeToken_
    }

-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- /Note:/ Consider using 'regexPatternSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsRegexPatternSetId :: Lens.Lens' DeleteRegexPatternSet Lude.Text
drpsRegexPatternSetId = Lens.lens (regexPatternSetId :: DeleteRegexPatternSet -> Lude.Text) (\s a -> s {regexPatternSetId = a} :: DeleteRegexPatternSet)
{-# DEPRECATED drpsRegexPatternSetId "Use generic-lens or generic-optics with 'regexPatternSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsChangeToken :: Lens.Lens' DeleteRegexPatternSet Lude.Text
drpsChangeToken = Lens.lens (changeToken :: DeleteRegexPatternSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteRegexPatternSet)
{-# DEPRECATED drpsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteRegexPatternSet where
  type Rs DeleteRegexPatternSet = DeleteRegexPatternSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRegexPatternSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRegexPatternSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.DeleteRegexPatternSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRegexPatternSet where
  toJSON DeleteRegexPatternSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RegexPatternSetId" Lude..= regexPatternSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteRegexPatternSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRegexPatternSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRegexPatternSetResponse' smart constructor.
data DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse'
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

-- | Creates a value of 'DeleteRegexPatternSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteRegexPatternSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRegexPatternSetResponse
mkDeleteRegexPatternSetResponse pResponseStatus_ =
  DeleteRegexPatternSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsrsChangeToken :: Lens.Lens' DeleteRegexPatternSetResponse (Lude.Maybe Lude.Text)
drpsrsChangeToken = Lens.lens (changeToken :: DeleteRegexPatternSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteRegexPatternSetResponse)
{-# DEPRECATED drpsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpsrsResponseStatus :: Lens.Lens' DeleteRegexPatternSetResponse Lude.Int
drpsrsResponseStatus = Lens.lens (responseStatus :: DeleteRegexPatternSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRegexPatternSetResponse)
{-# DEPRECATED drpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
