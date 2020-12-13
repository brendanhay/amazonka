{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'RegexMatchTuple' objects (filters) in a 'RegexMatchSet' . For each @RegexMatchSetUpdate@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @RegexMatchSetUpdate@ object, you delete the existing object and add a new one.
--
--
--     * The part of a web request that you want AWS WAF to inspectupdate, such as a query string or the value of the @User-Agent@ header.
--
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
-- For example, you can create a @RegexPatternSet@ that matches any requests with @User-Agent@ headers that contain the string @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
-- To create and configure a @RegexMatchSet@ , perform the following steps:
--
--     * Create a @RegexMatchSet.@ For more information, see 'CreateRegexMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexMatchSet@ request.
--
--
--     * Submit an @UpdateRegexMatchSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the identifier of the @RegexPatternSet@ that contain the regular expression patters you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateRegexMatchSet
  ( -- * Creating a request
    UpdateRegexMatchSet (..),
    mkUpdateRegexMatchSet,

    -- ** Request lenses
    urmsUpdates,
    urmsChangeToken,
    urmsRegexMatchSetId,

    -- * Destructuring the response
    UpdateRegexMatchSetResponse (..),
    mkUpdateRegexMatchSetResponse,

    -- ** Response lenses
    urmsrsChangeToken,
    urmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkUpdateRegexMatchSet' smart constructor.
data UpdateRegexMatchSet = UpdateRegexMatchSet'
  { -- | An array of @RegexMatchSetUpdate@ objects that you want to insert into or delete from a 'RegexMatchSet' . For more information, see 'RegexMatchTuple' .
    updates :: Lude.NonEmpty RegexMatchSetUpdate,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text,
    -- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to update. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
    regexMatchSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRegexMatchSet' with the minimum fields required to make a request.
--
-- * 'updates' - An array of @RegexMatchSetUpdate@ objects that you want to insert into or delete from a 'RegexMatchSet' . For more information, see 'RegexMatchTuple' .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'regexMatchSetId' - The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to update. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
mkUpdateRegexMatchSet ::
  -- | 'updates'
  Lude.NonEmpty RegexMatchSetUpdate ->
  -- | 'changeToken'
  Lude.Text ->
  -- | 'regexMatchSetId'
  Lude.Text ->
  UpdateRegexMatchSet
mkUpdateRegexMatchSet pUpdates_ pChangeToken_ pRegexMatchSetId_ =
  UpdateRegexMatchSet'
    { updates = pUpdates_,
      changeToken = pChangeToken_,
      regexMatchSetId = pRegexMatchSetId_
    }

-- | An array of @RegexMatchSetUpdate@ objects that you want to insert into or delete from a 'RegexMatchSet' . For more information, see 'RegexMatchTuple' .
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsUpdates :: Lens.Lens' UpdateRegexMatchSet (Lude.NonEmpty RegexMatchSetUpdate)
urmsUpdates = Lens.lens (updates :: UpdateRegexMatchSet -> Lude.NonEmpty RegexMatchSetUpdate) (\s a -> s {updates = a} :: UpdateRegexMatchSet)
{-# DEPRECATED urmsUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsChangeToken :: Lens.Lens' UpdateRegexMatchSet Lude.Text
urmsChangeToken = Lens.lens (changeToken :: UpdateRegexMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateRegexMatchSet)
{-# DEPRECATED urmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to update. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- /Note:/ Consider using 'regexMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsRegexMatchSetId :: Lens.Lens' UpdateRegexMatchSet Lude.Text
urmsRegexMatchSetId = Lens.lens (regexMatchSetId :: UpdateRegexMatchSet -> Lude.Text) (\s a -> s {regexMatchSetId = a} :: UpdateRegexMatchSet)
{-# DEPRECATED urmsRegexMatchSetId "Use generic-lens or generic-optics with 'regexMatchSetId' instead." #-}

instance Lude.AWSRequest UpdateRegexMatchSet where
  type Rs UpdateRegexMatchSet = UpdateRegexMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRegexMatchSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRegexMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.UpdateRegexMatchSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRegexMatchSet where
  toJSON UpdateRegexMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken),
            Lude.Just ("RegexMatchSetId" Lude..= regexMatchSetId)
          ]
      )

instance Lude.ToPath UpdateRegexMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRegexMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRegexMatchSetResponse' smart constructor.
data UpdateRegexMatchSetResponse = UpdateRegexMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRegexMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateRegexMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRegexMatchSetResponse
mkUpdateRegexMatchSetResponse pResponseStatus_ =
  UpdateRegexMatchSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsrsChangeToken :: Lens.Lens' UpdateRegexMatchSetResponse (Lude.Maybe Lude.Text)
urmsrsChangeToken = Lens.lens (changeToken :: UpdateRegexMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateRegexMatchSetResponse)
{-# DEPRECATED urmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urmsrsResponseStatus :: Lens.Lens' UpdateRegexMatchSetResponse Lude.Int
urmsrsResponseStatus = Lens.lens (responseStatus :: UpdateRegexMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRegexMatchSetResponse)
{-# DEPRECATED urmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
