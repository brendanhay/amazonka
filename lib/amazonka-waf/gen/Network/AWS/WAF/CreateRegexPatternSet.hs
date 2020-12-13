{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateRegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @RegexPatternSet@ . You then use 'UpdateRegexPatternSet' to specify the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @RegexPatternSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRegexPatternSet@ request.
--
--
--     * Submit a @CreateRegexPatternSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
--
--     * Submit an 'UpdateRegexPatternSet' request to specify the string that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateRegexPatternSet
  ( -- * Creating a request
    CreateRegexPatternSet (..),
    mkCreateRegexPatternSet,

    -- ** Request lenses
    crpsName,
    crpsChangeToken,

    -- * Destructuring the response
    CreateRegexPatternSetResponse (..),
    mkCreateRegexPatternSetResponse,

    -- ** Response lenses
    crpsrsRegexPatternSet,
    crpsrsChangeToken,
    crpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkCreateRegexPatternSet' smart constructor.
data CreateRegexPatternSet = CreateRegexPatternSet'
  { -- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
    name :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRegexPatternSet' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkCreateRegexPatternSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateRegexPatternSet
mkCreateRegexPatternSet pName_ pChangeToken_ =
  CreateRegexPatternSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsName :: Lens.Lens' CreateRegexPatternSet Lude.Text
crpsName = Lens.lens (name :: CreateRegexPatternSet -> Lude.Text) (\s a -> s {name = a} :: CreateRegexPatternSet)
{-# DEPRECATED crpsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsChangeToken :: Lens.Lens' CreateRegexPatternSet Lude.Text
crpsChangeToken = Lens.lens (changeToken :: CreateRegexPatternSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateRegexPatternSet)
{-# DEPRECATED crpsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateRegexPatternSet where
  type Rs CreateRegexPatternSet = CreateRegexPatternSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRegexPatternSetResponse'
            Lude.<$> (x Lude..?> "RegexPatternSet")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRegexPatternSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.CreateRegexPatternSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRegexPatternSet where
  toJSON CreateRegexPatternSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateRegexPatternSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRegexPatternSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRegexPatternSetResponse' smart constructor.
data CreateRegexPatternSetResponse = CreateRegexPatternSetResponse'
  { -- | A 'RegexPatternSet' that contains no objects.
    regexPatternSet :: Lude.Maybe RegexPatternSet,
    -- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRegexPatternSetResponse' with the minimum fields required to make a request.
--
-- * 'regexPatternSet' - A 'RegexPatternSet' that contains no objects.
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkCreateRegexPatternSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRegexPatternSetResponse
mkCreateRegexPatternSetResponse pResponseStatus_ =
  CreateRegexPatternSetResponse'
    { regexPatternSet = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'RegexPatternSet' that contains no objects.
--
-- /Note:/ Consider using 'regexPatternSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrsRegexPatternSet :: Lens.Lens' CreateRegexPatternSetResponse (Lude.Maybe RegexPatternSet)
crpsrsRegexPatternSet = Lens.lens (regexPatternSet :: CreateRegexPatternSetResponse -> Lude.Maybe RegexPatternSet) (\s a -> s {regexPatternSet = a} :: CreateRegexPatternSetResponse)
{-# DEPRECATED crpsrsRegexPatternSet "Use generic-lens or generic-optics with 'regexPatternSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrsChangeToken :: Lens.Lens' CreateRegexPatternSetResponse (Lude.Maybe Lude.Text)
crpsrsChangeToken = Lens.lens (changeToken :: CreateRegexPatternSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateRegexPatternSetResponse)
{-# DEPRECATED crpsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crpsrsResponseStatus :: Lens.Lens' CreateRegexPatternSetResponse Lude.Int
crpsrsResponseStatus = Lens.lens (responseStatus :: CreateRegexPatternSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRegexPatternSetResponse)
{-# DEPRECATED crpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
