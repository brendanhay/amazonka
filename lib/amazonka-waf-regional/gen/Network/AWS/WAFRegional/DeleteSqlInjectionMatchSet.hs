{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'SqlInjectionMatchSet' . You can't delete a @SqlInjectionMatchSet@ if it's still used in any @Rules@ or if it still contains any 'SqlInjectionMatchTuple' objects.
--
-- If you just want to remove a @SqlInjectionMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @SqlInjectionMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @SqlInjectionMatchSet@ to remove filters, if any. For more information, see 'UpdateSqlInjectionMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteSqlInjectionMatchSet@ request.
--
--
--     * Submit a @DeleteSqlInjectionMatchSet@ request.
module Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
  ( -- * Creating a request
    DeleteSqlInjectionMatchSet (..),
    mkDeleteSqlInjectionMatchSet,

    -- ** Request lenses
    dsimsSqlInjectionMatchSetId,
    dsimsChangeToken,

    -- * Destructuring the response
    DeleteSqlInjectionMatchSetResponse (..),
    mkDeleteSqlInjectionMatchSetResponse,

    -- ** Response lenses
    dsimsrsChangeToken,
    dsimsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | A request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteSqlInjectionMatchSet' smart constructor.
data DeleteSqlInjectionMatchSet = DeleteSqlInjectionMatchSet'
  { sqlInjectionMatchSetId ::
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

-- | Creates a value of 'DeleteSqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'sqlInjectionMatchSetId' - The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
mkDeleteSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  DeleteSqlInjectionMatchSet
mkDeleteSqlInjectionMatchSet pSqlInjectionMatchSetId_ pChangeToken_ =
  DeleteSqlInjectionMatchSet'
    { sqlInjectionMatchSetId =
        pSqlInjectionMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsSqlInjectionMatchSetId :: Lens.Lens' DeleteSqlInjectionMatchSet Lude.Text
dsimsSqlInjectionMatchSetId = Lens.lens (sqlInjectionMatchSetId :: DeleteSqlInjectionMatchSet -> Lude.Text) (\s a -> s {sqlInjectionMatchSetId = a} :: DeleteSqlInjectionMatchSet)
{-# DEPRECATED dsimsSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsChangeToken :: Lens.Lens' DeleteSqlInjectionMatchSet Lude.Text
dsimsChangeToken = Lens.lens (changeToken :: DeleteSqlInjectionMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: DeleteSqlInjectionMatchSet)
{-# DEPRECATED dsimsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest DeleteSqlInjectionMatchSet where
  type
    Rs DeleteSqlInjectionMatchSet =
      DeleteSqlInjectionMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSqlInjectionMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSqlInjectionMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.DeleteSqlInjectionMatchSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSqlInjectionMatchSet where
  toJSON DeleteSqlInjectionMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("SqlInjectionMatchSetId" Lude..= sqlInjectionMatchSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath DeleteSqlInjectionMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSqlInjectionMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to a request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteSqlInjectionMatchSetResponse' smart constructor.
data DeleteSqlInjectionMatchSetResponse = DeleteSqlInjectionMatchSetResponse'
  { changeToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSqlInjectionMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkDeleteSqlInjectionMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSqlInjectionMatchSetResponse
mkDeleteSqlInjectionMatchSetResponse pResponseStatus_ =
  DeleteSqlInjectionMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsrsChangeToken :: Lens.Lens' DeleteSqlInjectionMatchSetResponse (Lude.Maybe Lude.Text)
dsimsrsChangeToken = Lens.lens (changeToken :: DeleteSqlInjectionMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: DeleteSqlInjectionMatchSetResponse)
{-# DEPRECATED dsimsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsrsResponseStatus :: Lens.Lens' DeleteSqlInjectionMatchSetResponse Lude.Int
dsimsrsResponseStatus = Lens.lens (responseStatus :: DeleteSqlInjectionMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSqlInjectionMatchSetResponse)
{-# DEPRECATED dsimsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
