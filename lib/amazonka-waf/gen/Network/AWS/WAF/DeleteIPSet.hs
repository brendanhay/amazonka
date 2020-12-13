{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an 'IPSet' . You can't delete an @IPSet@ if it's still used in any @Rules@ or if it still includes any IP addresses.
--
-- If you just want to remove an @IPSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete an @IPSet@ from AWS WAF, perform the following steps:
--
--     * Update the @IPSet@ to remove IP address ranges, if any. For more information, see 'UpdateIPSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteIPSet@ request.
--
--
--     * Submit a @DeleteIPSet@ request.
module Network.AWS.WAF.DeleteIPSet
  ( -- * Creating a request
    DeleteIPSet (..),
    mkDeleteIPSet,

    -- ** Request lenses
    disChangeToken,
    disIPSetId,

    -- * Destructuring the response
    DeleteIPSetResponse (..),
    mkDeleteIPSetResponse,

    -- ** Response lenses
    disrsChangeToken,
    disrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkDeleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text,
    -- | The @IPSetId@ of the 'IPSet' that you want to delete. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
    ipSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIPSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'ipSetId' - The @IPSetId@ of the 'IPSet' that you want to delete. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
mkDeleteIPSet ::
  -- | 'changeToken'
  Lude.Text ->
  -- | 'ipSetId'
  Lude.Text ->
  DeleteIPSet
mkDeleteIPSet pChangeToken_ pIPSetId_ =
  DeleteIPSet' {changeToken = pChangeToken_, ipSetId = pIPSetId_}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disChangeToken :: Lens.Lens' DeleteIPSet Lude.Text
disChangeToken = Lens.lens (changeToken :: DeleteIPSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteIPSet)
{-# DEPRECATED disChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The @IPSetId@ of the 'IPSet' that you want to delete. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disIPSetId :: Lens.Lens' DeleteIPSet Lude.Text
disIPSetId = Lens.lens (ipSetId :: DeleteIPSet -> Lude.Text) (\s a -> s {ipSetId = a} :: DeleteIPSet)
{-# DEPRECATED disIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

instance Lude.AWSRequest DeleteIPSet where
  type Rs DeleteIPSet = DeleteIPSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteIPSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteIPSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.DeleteIPSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteIPSet where
  toJSON DeleteIPSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ChangeToken" Lude..= changeToken),
            Lude.Just ("IPSetId" Lude..= ipSetId)
          ]
      )

instance Lude.ToPath DeleteIPSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIPSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIPSetResponse' smart constructor.
data DeleteIPSetResponse = DeleteIPSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIPSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteIPSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteIPSetResponse
mkDeleteIPSetResponse pResponseStatus_ =
  DeleteIPSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsChangeToken :: Lens.Lens' DeleteIPSetResponse (Lude.Maybe Lude.Text)
disrsChangeToken = Lens.lens (changeToken :: DeleteIPSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteIPSetResponse)
{-# DEPRECATED disrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsResponseStatus :: Lens.Lens' DeleteIPSetResponse Lude.Int
disrsResponseStatus = Lens.lens (responseStatus :: DeleteIPSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteIPSetResponse)
{-# DEPRECATED disrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
