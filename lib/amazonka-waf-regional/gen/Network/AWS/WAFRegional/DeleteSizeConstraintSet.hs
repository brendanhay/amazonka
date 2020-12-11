{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'SizeConstraintSet' . You can't delete a @SizeConstraintSet@ if it's still used in any @Rules@ or if it still includes any 'SizeConstraint' objects (any filters).
--
-- If you just want to remove a @SizeConstraintSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @SizeConstraintSet@ , perform the following steps:
--
--     * Update the @SizeConstraintSet@ to remove filters, if any. For more information, see 'UpdateSizeConstraintSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteSizeConstraintSet@ request.
--
--
--     * Submit a @DeleteSizeConstraintSet@ request.
module Network.AWS.WAFRegional.DeleteSizeConstraintSet
  ( -- * Creating a request
    DeleteSizeConstraintSet (..),
    mkDeleteSizeConstraintSet,

    -- ** Request lenses
    dscsSizeConstraintSetId,
    dscsChangeToken,

    -- * Destructuring the response
    DeleteSizeConstraintSetResponse (..),
    mkDeleteSizeConstraintSetResponse,

    -- ** Response lenses
    dscsrsChangeToken,
    dscsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkDeleteSizeConstraintSet' smart constructor.
data DeleteSizeConstraintSet = DeleteSizeConstraintSet'
  { sizeConstraintSetId ::
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

-- | Creates a value of 'DeleteSizeConstraintSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'sizeConstraintSetId' - The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to delete. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
mkDeleteSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteSizeConstraintSet
mkDeleteSizeConstraintSet pSizeConstraintSetId_ pChangeToken_ =
  DeleteSizeConstraintSet'
    { sizeConstraintSetId =
        pSizeConstraintSetId_,
      changeToken = pChangeToken_
    }

-- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to delete. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsSizeConstraintSetId :: Lens.Lens' DeleteSizeConstraintSet Lude.Text
dscsSizeConstraintSetId = Lens.lens (sizeConstraintSetId :: DeleteSizeConstraintSet -> Lude.Text) (\s a -> s {sizeConstraintSetId = a} :: DeleteSizeConstraintSet)
{-# DEPRECATED dscsSizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsChangeToken :: Lens.Lens' DeleteSizeConstraintSet Lude.Text
dscsChangeToken = Lens.lens (changeToken :: DeleteSizeConstraintSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteSizeConstraintSet)
{-# DEPRECATED dscsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteSizeConstraintSet where
  type Rs DeleteSizeConstraintSet = DeleteSizeConstraintSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSizeConstraintSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSizeConstraintSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.DeleteSizeConstraintSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSizeConstraintSet where
  toJSON DeleteSizeConstraintSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SizeConstraintSetId" Lude..= sizeConstraintSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteSizeConstraintSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSizeConstraintSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSizeConstraintSetResponse' smart constructor.
data DeleteSizeConstraintSetResponse = DeleteSizeConstraintSetResponse'
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

-- | Creates a value of 'DeleteSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteSizeConstraintSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSizeConstraintSetResponse
mkDeleteSizeConstraintSetResponse pResponseStatus_ =
  DeleteSizeConstraintSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsrsChangeToken :: Lens.Lens' DeleteSizeConstraintSetResponse (Lude.Maybe Lude.Text)
dscsrsChangeToken = Lens.lens (changeToken :: DeleteSizeConstraintSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteSizeConstraintSetResponse)
{-# DEPRECATED dscsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsrsResponseStatus :: Lens.Lens' DeleteSizeConstraintSetResponse Lude.Int
dscsrsResponseStatus = Lens.lens (responseStatus :: DeleteSizeConstraintSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSizeConstraintSetResponse)
{-# DEPRECATED dscsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
