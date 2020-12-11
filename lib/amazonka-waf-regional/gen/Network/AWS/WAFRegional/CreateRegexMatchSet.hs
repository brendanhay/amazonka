{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateRegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'RegexMatchSet' . You then use 'UpdateRegexMatchSet' to identify the part of a web request that you want AWS WAF to inspect, such as the values of the @User-Agent@ header or the query string. For example, you can create a @RegexMatchSet@ that contains a @RegexMatchTuple@ that looks for any requests with @User-Agent@ headers that match a @RegexPatternSet@ with pattern @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @RegexMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRegexMatchSet@ request.
--
--
--     * Submit a @CreateRegexMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexMatchSet@ request.
--
--
--     * Submit an 'UpdateRegexMatchSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value, using a @RegexPatternSet@ , that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateRegexMatchSet
  ( -- * Creating a request
    CreateRegexMatchSet (..),
    mkCreateRegexMatchSet,

    -- ** Request lenses
    crmsName,
    crmsChangeToken,

    -- * Destructuring the response
    CreateRegexMatchSetResponse (..),
    mkCreateRegexMatchSetResponse,

    -- ** Response lenses
    crmsrsRegexMatchSet,
    crmsrsChangeToken,
    crmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkCreateRegexMatchSet' smart constructor.
data CreateRegexMatchSet = CreateRegexMatchSet'
  { name :: Lude.Text,
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

-- | Creates a value of 'CreateRegexMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'name' - A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
mkCreateRegexMatchSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateRegexMatchSet
mkCreateRegexMatchSet pName_ pChangeToken_ =
  CreateRegexMatchSet' {name = pName_, changeToken = pChangeToken_}

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsName :: Lens.Lens' CreateRegexMatchSet Lude.Text
crmsName = Lens.lens (name :: CreateRegexMatchSet -> Lude.Text) (\s a -> s {name = a} :: CreateRegexMatchSet)
{-# DEPRECATED crmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsChangeToken :: Lens.Lens' CreateRegexMatchSet Lude.Text
crmsChangeToken = Lens.lens (changeToken :: CreateRegexMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateRegexMatchSet)
{-# DEPRECATED crmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateRegexMatchSet where
  type Rs CreateRegexMatchSet = CreateRegexMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRegexMatchSetResponse'
            Lude.<$> (x Lude..?> "RegexMatchSet")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRegexMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.CreateRegexMatchSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRegexMatchSet where
  toJSON CreateRegexMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateRegexMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRegexMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRegexMatchSetResponse' smart constructor.
data CreateRegexMatchSetResponse = CreateRegexMatchSetResponse'
  { regexMatchSet ::
      Lude.Maybe RegexMatchSet,
    changeToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateRegexMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'regexMatchSet' - A 'RegexMatchSet' that contains no @RegexMatchTuple@ objects.
-- * 'responseStatus' - The response status code.
mkCreateRegexMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRegexMatchSetResponse
mkCreateRegexMatchSetResponse pResponseStatus_ =
  CreateRegexMatchSetResponse'
    { regexMatchSet = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'RegexMatchSet' that contains no @RegexMatchTuple@ objects.
--
-- /Note:/ Consider using 'regexMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsrsRegexMatchSet :: Lens.Lens' CreateRegexMatchSetResponse (Lude.Maybe RegexMatchSet)
crmsrsRegexMatchSet = Lens.lens (regexMatchSet :: CreateRegexMatchSetResponse -> Lude.Maybe RegexMatchSet) (\s a -> s {regexMatchSet = a} :: CreateRegexMatchSetResponse)
{-# DEPRECATED crmsrsRegexMatchSet "Use generic-lens or generic-optics with 'regexMatchSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsrsChangeToken :: Lens.Lens' CreateRegexMatchSetResponse (Lude.Maybe Lude.Text)
crmsrsChangeToken = Lens.lens (changeToken :: CreateRegexMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateRegexMatchSetResponse)
{-# DEPRECATED crmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crmsrsResponseStatus :: Lens.Lens' CreateRegexMatchSetResponse Lude.Int
crmsrsResponseStatus = Lens.lens (responseStatus :: CreateRegexMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRegexMatchSetResponse)
{-# DEPRECATED crmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
