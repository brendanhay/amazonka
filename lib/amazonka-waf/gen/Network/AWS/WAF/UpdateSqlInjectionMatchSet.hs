{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'SqlInjectionMatchTuple' objects (filters) in a 'SqlInjectionMatchSet' . For each @SqlInjectionMatchTuple@ object, you specify the following values:
--
--
--     * @Action@ : Whether to insert the object into or delete the object from the array. To change a @SqlInjectionMatchTuple@ , you delete the existing object and add a new one.
--
--
--     * @FieldToMatch@ : The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header or custom query parameter, the name of the header or parameter.
--
--
--     * @TextTransformation@ : Which text transformation, if any, to perform on the web request before inspecting the request for snippets of malicious SQL code.
-- You can only specify a single type of TextTransformation.
--
--
-- You use @SqlInjectionMatchSet@ objects to specify which CloudFront requests that you want to allow, block, or count. For example, if you're receiving requests that contain snippets of SQL code in the query string and you want to block the requests, you can create a @SqlInjectionMatchSet@ with the applicable settings, and then configure AWS WAF to block the requests.
-- To create and configure a @SqlInjectionMatchSet@ , perform the following steps:
--
--     * Submit a 'CreateSqlInjectionMatchSet' request.
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateSqlInjectionMatchSet@ request to specify the parts of web requests that you want AWS WAF to inspect for snippets of SQL code.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateSqlInjectionMatchSet
  ( -- * Creating a request
    UpdateSqlInjectionMatchSet (..),
    mkUpdateSqlInjectionMatchSet,

    -- ** Request lenses
    usimsSqlInjectionMatchSetId,
    usimsChangeToken,
    usimsUpdates,

    -- * Destructuring the response
    UpdateSqlInjectionMatchSetResponse (..),
    mkUpdateSqlInjectionMatchSetResponse,

    -- ** Response lenses
    usimsrsChangeToken,
    usimsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | A request to update a 'SqlInjectionMatchSet' .
--
-- /See:/ 'mkUpdateSqlInjectionMatchSet' smart constructor.
data UpdateSqlInjectionMatchSet = UpdateSqlInjectionMatchSet'
  { sqlInjectionMatchSetId ::
      Lude.Text,
    changeToken :: Lude.Text,
    updates ::
      Lude.NonEmpty
        SqlInjectionMatchSetUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'sqlInjectionMatchSetId' - The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want to update. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
-- * 'updates' - An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert into or delete from a 'SqlInjectionMatchSet' . For more information, see the applicable data types:
--
--
--     * 'SqlInjectionMatchSetUpdate' : Contains @Action@ and @SqlInjectionMatchTuple@
--
--
--     * 'SqlInjectionMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
mkUpdateSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  -- | 'updates'
  Lude.NonEmpty SqlInjectionMatchSetUpdate ->
  UpdateSqlInjectionMatchSet
mkUpdateSqlInjectionMatchSet
  pSqlInjectionMatchSetId_
  pChangeToken_
  pUpdates_ =
    UpdateSqlInjectionMatchSet'
      { sqlInjectionMatchSetId =
          pSqlInjectionMatchSetId_,
        changeToken = pChangeToken_,
        updates = pUpdates_
      }

-- | The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want to update. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsSqlInjectionMatchSetId :: Lens.Lens' UpdateSqlInjectionMatchSet Lude.Text
usimsSqlInjectionMatchSetId = Lens.lens (sqlInjectionMatchSetId :: UpdateSqlInjectionMatchSet -> Lude.Text) (\s a -> s {sqlInjectionMatchSetId = a} :: UpdateSqlInjectionMatchSet)
{-# DEPRECATED usimsSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsChangeToken :: Lens.Lens' UpdateSqlInjectionMatchSet Lude.Text
usimsChangeToken = Lens.lens (changeToken :: UpdateSqlInjectionMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateSqlInjectionMatchSet)
{-# DEPRECATED usimsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert into or delete from a 'SqlInjectionMatchSet' . For more information, see the applicable data types:
--
--
--     * 'SqlInjectionMatchSetUpdate' : Contains @Action@ and @SqlInjectionMatchTuple@
--
--
--     * 'SqlInjectionMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsUpdates :: Lens.Lens' UpdateSqlInjectionMatchSet (Lude.NonEmpty SqlInjectionMatchSetUpdate)
usimsUpdates = Lens.lens (updates :: UpdateSqlInjectionMatchSet -> Lude.NonEmpty SqlInjectionMatchSetUpdate) (\s a -> s {updates = a} :: UpdateSqlInjectionMatchSet)
{-# DEPRECATED usimsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

instance Lude.AWSRequest UpdateSqlInjectionMatchSet where
  type
    Rs UpdateSqlInjectionMatchSet =
      UpdateSqlInjectionMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSqlInjectionMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSqlInjectionMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.UpdateSqlInjectionMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSqlInjectionMatchSet where
  toJSON UpdateSqlInjectionMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("SqlInjectionMatchSetId" Lude..= sqlInjectionMatchSetId),
            Lude.Just ("ChangeToken" Lude..= changeToken),
            Lude.Just ("Updates" Lude..= updates)
          ]
      )

instance Lude.ToPath UpdateSqlInjectionMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSqlInjectionMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to an 'UpdateSqlInjectionMatchSets' request.
--
-- /See:/ 'mkUpdateSqlInjectionMatchSetResponse' smart constructor.
data UpdateSqlInjectionMatchSetResponse = UpdateSqlInjectionMatchSetResponse'
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

-- | Creates a value of 'UpdateSqlInjectionMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateSqlInjectionMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSqlInjectionMatchSetResponse
mkUpdateSqlInjectionMatchSetResponse pResponseStatus_ =
  UpdateSqlInjectionMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsrsChangeToken :: Lens.Lens' UpdateSqlInjectionMatchSetResponse (Lude.Maybe Lude.Text)
usimsrsChangeToken = Lens.lens (changeToken :: UpdateSqlInjectionMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateSqlInjectionMatchSetResponse)
{-# DEPRECATED usimsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usimsrsResponseStatus :: Lens.Lens' UpdateSqlInjectionMatchSetResponse Lude.Int
usimsrsResponseStatus = Lens.lens (responseStatus :: UpdateSqlInjectionMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSqlInjectionMatchSetResponse)
{-# DEPRECATED usimsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
