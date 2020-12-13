{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateXSSMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'XssMatchTuple' objects (filters) in an 'XssMatchSet' . For each @XssMatchTuple@ object, you specify the following values:
--
--
--     * @Action@ : Whether to insert the object into or delete the object from the array. To change an @XssMatchTuple@ , you delete the existing object and add a new one.
--
--
--     * @FieldToMatch@ : The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header or custom query parameter, the name of the header or parameter.
--
--
--     * @TextTransformation@ : Which text transformation, if any, to perform on the web request before inspecting the request for cross-site scripting attacks.
-- You can only specify a single type of TextTransformation.
--
--
-- You use @XssMatchSet@ objects to specify which CloudFront requests that you want to allow, block, or count. For example, if you're receiving requests that contain cross-site scripting attacks in the request body and you want to block the requests, you can create an @XssMatchSet@ with the applicable settings, and then configure AWS WAF to block the requests.
-- To create and configure an @XssMatchSet@ , perform the following steps:
--
--     * Submit a 'CreateXssMatchSet' request.
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateXssMatchSet@ request to specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateXSSMatchSet
  ( -- * Creating a request
    UpdateXSSMatchSet (..),
    mkUpdateXSSMatchSet,

    -- ** Request lenses
    uxmsXSSMatchSetId,
    uxmsUpdates,
    uxmsChangeToken,

    -- * Destructuring the response
    UpdateXSSMatchSetResponse (..),
    mkUpdateXSSMatchSetResponse,

    -- ** Response lenses
    uxmsrsChangeToken,
    uxmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | A request to update an 'XssMatchSet' .
--
-- /See:/ 'mkUpdateXSSMatchSet' smart constructor.
data UpdateXSSMatchSet = UpdateXSSMatchSet'
  { -- | The @XssMatchSetId@ of the @XssMatchSet@ that you want to update. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
    xssMatchSetId :: Lude.Text,
    -- | An array of @XssMatchSetUpdate@ objects that you want to insert into or delete from an 'XssMatchSet' . For more information, see the applicable data types:
    --
    --
    --     * 'XssMatchSetUpdate' : Contains @Action@ and @XssMatchTuple@
    --
    --
    --     * 'XssMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    updates :: Lude.NonEmpty XSSMatchSetUpdate,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateXSSMatchSet' with the minimum fields required to make a request.
--
-- * 'xssMatchSetId' - The @XssMatchSetId@ of the @XssMatchSet@ that you want to update. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
-- * 'updates' - An array of @XssMatchSetUpdate@ objects that you want to insert into or delete from an 'XssMatchSet' . For more information, see the applicable data types:
--
--
--     * 'XssMatchSetUpdate' : Contains @Action@ and @XssMatchTuple@
--
--
--     * 'XssMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkUpdateXSSMatchSet ::
  -- | 'xssMatchSetId'
  Lude.Text ->
  -- | 'updates'
  Lude.NonEmpty XSSMatchSetUpdate ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateXSSMatchSet
mkUpdateXSSMatchSet pXSSMatchSetId_ pUpdates_ pChangeToken_ =
  UpdateXSSMatchSet'
    { xssMatchSetId = pXSSMatchSetId_,
      updates = pUpdates_,
      changeToken = pChangeToken_
    }

-- | The @XssMatchSetId@ of the @XssMatchSet@ that you want to update. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- /Note:/ Consider using 'xssMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsXSSMatchSetId :: Lens.Lens' UpdateXSSMatchSet Lude.Text
uxmsXSSMatchSetId = Lens.lens (xssMatchSetId :: UpdateXSSMatchSet -> Lude.Text) (\s a -> s {xssMatchSetId = a} :: UpdateXSSMatchSet)
{-# DEPRECATED uxmsXSSMatchSetId "Use generic-lens or generic-optics with 'xssMatchSetId' instead." #-}

-- | An array of @XssMatchSetUpdate@ objects that you want to insert into or delete from an 'XssMatchSet' . For more information, see the applicable data types:
--
--
--     * 'XssMatchSetUpdate' : Contains @Action@ and @XssMatchTuple@
--
--
--     * 'XssMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsUpdates :: Lens.Lens' UpdateXSSMatchSet (Lude.NonEmpty XSSMatchSetUpdate)
uxmsUpdates = Lens.lens (updates :: UpdateXSSMatchSet -> Lude.NonEmpty XSSMatchSetUpdate) (\s a -> s {updates = a} :: UpdateXSSMatchSet)
{-# DEPRECATED uxmsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsChangeToken :: Lens.Lens' UpdateXSSMatchSet Lude.Text
uxmsChangeToken = Lens.lens (changeToken :: UpdateXSSMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateXSSMatchSet)
{-# DEPRECATED uxmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateXSSMatchSet where
  type Rs UpdateXSSMatchSet = UpdateXSSMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateXSSMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateXSSMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.UpdateXssMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateXSSMatchSet where
  toJSON UpdateXSSMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("XssMatchSetId" Lude..= xssMatchSetId),
            Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateXSSMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateXSSMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to an 'UpdateXssMatchSets' request.
--
-- /See:/ 'mkUpdateXSSMatchSetResponse' smart constructor.
data UpdateXSSMatchSetResponse = UpdateXSSMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateXSSMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateXSSMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateXSSMatchSetResponse
mkUpdateXSSMatchSetResponse pResponseStatus_ =
  UpdateXSSMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsrsChangeToken :: Lens.Lens' UpdateXSSMatchSetResponse (Lude.Maybe Lude.Text)
uxmsrsChangeToken = Lens.lens (changeToken :: UpdateXSSMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateXSSMatchSetResponse)
{-# DEPRECATED uxmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uxmsrsResponseStatus :: Lens.Lens' UpdateXSSMatchSetResponse Lude.Int
uxmsrsResponseStatus = Lens.lens (responseStatus :: UpdateXSSMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateXSSMatchSetResponse)
{-# DEPRECATED uxmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
