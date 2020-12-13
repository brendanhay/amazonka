{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateXSSMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an 'XssMatchSet' , which you use to allow, block, or count requests that contain cross-site scripting attacks in the specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.
--
-- To create and configure an @XssMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateXssMatchSet@ request.
--
--
--     * Submit a @CreateXssMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateXssMatchSet' request.
--
--
--     * Submit an 'UpdateXssMatchSet' request to specify the parts of web requests in which you want to allow, block, or count cross-site scripting attacks.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateXSSMatchSet
  ( -- * Creating a request
    CreateXSSMatchSet (..),
    mkCreateXSSMatchSet,

    -- ** Request lenses
    cxmsName,
    cxmsChangeToken,

    -- * Destructuring the response
    CreateXSSMatchSetResponse (..),
    mkCreateXSSMatchSetResponse,

    -- ** Response lenses
    cxmsrsXSSMatchSet,
    cxmsrsChangeToken,
    cxmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | A request to create an 'XssMatchSet' .
--
-- /See:/ 'mkCreateXSSMatchSet' smart constructor.
data CreateXSSMatchSet = CreateXSSMatchSet'
  { -- | A friendly name or description for the 'XssMatchSet' that you're creating. You can't change @Name@ after you create the @XssMatchSet@ .
    name :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateXSSMatchSet' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description for the 'XssMatchSet' that you're creating. You can't change @Name@ after you create the @XssMatchSet@ .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkCreateXSSMatchSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateXSSMatchSet
mkCreateXSSMatchSet pName_ pChangeToken_ =
  CreateXSSMatchSet' {name = pName_, changeToken = pChangeToken_}

-- | A friendly name or description for the 'XssMatchSet' that you're creating. You can't change @Name@ after you create the @XssMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsName :: Lens.Lens' CreateXSSMatchSet Lude.Text
cxmsName = Lens.lens (name :: CreateXSSMatchSet -> Lude.Text) (\s a -> s {name = a} :: CreateXSSMatchSet)
{-# DEPRECATED cxmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsChangeToken :: Lens.Lens' CreateXSSMatchSet Lude.Text
cxmsChangeToken = Lens.lens (changeToken :: CreateXSSMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateXSSMatchSet)
{-# DEPRECATED cxmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateXSSMatchSet where
  type Rs CreateXSSMatchSet = CreateXSSMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateXSSMatchSetResponse'
            Lude.<$> (x Lude..?> "XssMatchSet")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateXSSMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.CreateXssMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateXSSMatchSet where
  toJSON CreateXSSMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateXSSMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateXSSMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to a @CreateXssMatchSet@ request.
--
-- /See:/ 'mkCreateXSSMatchSetResponse' smart constructor.
data CreateXSSMatchSetResponse = CreateXSSMatchSetResponse'
  { -- | An 'XssMatchSet' .
    xssMatchSet :: Lude.Maybe XSSMatchSet,
    -- | The @ChangeToken@ that you used to submit the @CreateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateXSSMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'xssMatchSet' - An 'XssMatchSet' .
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkCreateXSSMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateXSSMatchSetResponse
mkCreateXSSMatchSetResponse pResponseStatus_ =
  CreateXSSMatchSetResponse'
    { xssMatchSet = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An 'XssMatchSet' .
--
-- /Note:/ Consider using 'xssMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsrsXSSMatchSet :: Lens.Lens' CreateXSSMatchSetResponse (Lude.Maybe XSSMatchSet)
cxmsrsXSSMatchSet = Lens.lens (xssMatchSet :: CreateXSSMatchSetResponse -> Lude.Maybe XSSMatchSet) (\s a -> s {xssMatchSet = a} :: CreateXSSMatchSetResponse)
{-# DEPRECATED cxmsrsXSSMatchSet "Use generic-lens or generic-optics with 'xssMatchSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsrsChangeToken :: Lens.Lens' CreateXSSMatchSetResponse (Lude.Maybe Lude.Text)
cxmsrsChangeToken = Lens.lens (changeToken :: CreateXSSMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateXSSMatchSetResponse)
{-# DEPRECATED cxmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cxmsrsResponseStatus :: Lens.Lens' CreateXSSMatchSetResponse Lude.Int
cxmsrsResponseStatus = Lens.lens (responseStatus :: CreateXSSMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateXSSMatchSetResponse)
{-# DEPRECATED cxmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
