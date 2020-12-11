{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'ByteMatchSet' . You can't delete a @ByteMatchSet@ if it's still used in any @Rules@ or if it still includes any 'ByteMatchTuple' objects (any filters).
--
-- If you just want to remove a @ByteMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @ByteMatchSet@ , perform the following steps:
--
--     * Update the @ByteMatchSet@ to remove filters, if any. For more information, see 'UpdateByteMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteByteMatchSet@ request.
--
--
--     * Submit a @DeleteByteMatchSet@ request.
module Network.AWS.WAFRegional.DeleteByteMatchSet
  ( -- * Creating a request
    DeleteByteMatchSet (..),
    mkDeleteByteMatchSet,

    -- ** Request lenses
    dbmsByteMatchSetId,
    dbmsChangeToken,

    -- * Destructuring the response
    DeleteByteMatchSetResponse (..),
    mkDeleteByteMatchSetResponse,

    -- ** Response lenses
    dbmsrsChangeToken,
    dbmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkDeleteByteMatchSet' smart constructor.
data DeleteByteMatchSet = DeleteByteMatchSet'
  { byteMatchSetId ::
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

-- | Creates a value of 'DeleteByteMatchSet' with the minimum fields required to make a request.
--
-- * 'byteMatchSetId' - The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkDeleteByteMatchSet ::
  -- | 'byteMatchSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteByteMatchSet
mkDeleteByteMatchSet pByteMatchSetId_ pChangeToken_ =
  DeleteByteMatchSet'
    { byteMatchSetId = pByteMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- /Note:/ Consider using 'byteMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsByteMatchSetId :: Lens.Lens' DeleteByteMatchSet Lude.Text
dbmsByteMatchSetId = Lens.lens (byteMatchSetId :: DeleteByteMatchSet -> Lude.Text) (\s a -> s {byteMatchSetId = a} :: DeleteByteMatchSet)
{-# DEPRECATED dbmsByteMatchSetId "Use generic-lens or generic-optics with 'byteMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsChangeToken :: Lens.Lens' DeleteByteMatchSet Lude.Text
dbmsChangeToken = Lens.lens (changeToken :: DeleteByteMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteByteMatchSet)
{-# DEPRECATED dbmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteByteMatchSet where
  type Rs DeleteByteMatchSet = DeleteByteMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteByteMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteByteMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.DeleteByteMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteByteMatchSet where
  toJSON DeleteByteMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ByteMatchSetId" Lude..= byteMatchSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteByteMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteByteMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteByteMatchSetResponse' smart constructor.
data DeleteByteMatchSetResponse = DeleteByteMatchSetResponse'
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

-- | Creates a value of 'DeleteByteMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteByteMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteByteMatchSetResponse
mkDeleteByteMatchSetResponse pResponseStatus_ =
  DeleteByteMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsrsChangeToken :: Lens.Lens' DeleteByteMatchSetResponse (Lude.Maybe Lude.Text)
dbmsrsChangeToken = Lens.lens (changeToken :: DeleteByteMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteByteMatchSetResponse)
{-# DEPRECATED dbmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbmsrsResponseStatus :: Lens.Lens' DeleteByteMatchSetResponse Lude.Int
dbmsrsResponseStatus = Lens.lens (responseStatus :: DeleteByteMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteByteMatchSetResponse)
{-# DEPRECATED dbmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
