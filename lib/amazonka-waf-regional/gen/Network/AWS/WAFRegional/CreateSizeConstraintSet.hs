{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateSizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @SizeConstraintSet@ . You then use 'UpdateSizeConstraintSet' to identify the part of a web request that you want AWS WAF to check for length, such as the length of the @User-Agent@ header or the length of the query string. For example, you can create a @SizeConstraintSet@ that matches any requests that have a query string that is longer than 100 bytes. You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @SizeConstraintSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateSizeConstraintSet@ request.
--
--
--     * Submit a @CreateSizeConstraintSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
--
--     * Submit an 'UpdateSizeConstraintSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateSizeConstraintSet
  ( -- * Creating a request
    CreateSizeConstraintSet (..),
    mkCreateSizeConstraintSet,

    -- ** Request lenses
    cscsName,
    cscsChangeToken,

    -- * Destructuring the response
    CreateSizeConstraintSetResponse (..),
    mkCreateSizeConstraintSetResponse,

    -- ** Response lenses
    cscsrsSizeConstraintSet,
    cscsrsChangeToken,
    cscsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkCreateSizeConstraintSet' smart constructor.
data CreateSizeConstraintSet = CreateSizeConstraintSet'
  { -- | A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
    name :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSizeConstraintSet' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkCreateSizeConstraintSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateSizeConstraintSet
mkCreateSizeConstraintSet pName_ pChangeToken_ =
  CreateSizeConstraintSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsName :: Lens.Lens' CreateSizeConstraintSet Lude.Text
cscsName = Lens.lens (name :: CreateSizeConstraintSet -> Lude.Text) (\s a -> s {name = a} :: CreateSizeConstraintSet)
{-# DEPRECATED cscsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsChangeToken :: Lens.Lens' CreateSizeConstraintSet Lude.Text
cscsChangeToken = Lens.lens (changeToken :: CreateSizeConstraintSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateSizeConstraintSet)
{-# DEPRECATED cscsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateSizeConstraintSet where
  type Rs CreateSizeConstraintSet = CreateSizeConstraintSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSizeConstraintSetResponse'
            Lude.<$> (x Lude..?> "SizeConstraintSet")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSizeConstraintSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.CreateSizeConstraintSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSizeConstraintSet where
  toJSON CreateSizeConstraintSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateSizeConstraintSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSizeConstraintSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSizeConstraintSetResponse' smart constructor.
data CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse'
  { -- | A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
    sizeConstraintSet :: Lude.Maybe SizeConstraintSet,
    -- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- * 'sizeConstraintSet' - A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkCreateSizeConstraintSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSizeConstraintSetResponse
mkCreateSizeConstraintSetResponse pResponseStatus_ =
  CreateSizeConstraintSetResponse'
    { sizeConstraintSet =
        Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
--
-- /Note:/ Consider using 'sizeConstraintSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrsSizeConstraintSet :: Lens.Lens' CreateSizeConstraintSetResponse (Lude.Maybe SizeConstraintSet)
cscsrsSizeConstraintSet = Lens.lens (sizeConstraintSet :: CreateSizeConstraintSetResponse -> Lude.Maybe SizeConstraintSet) (\s a -> s {sizeConstraintSet = a} :: CreateSizeConstraintSetResponse)
{-# DEPRECATED cscsrsSizeConstraintSet "Use generic-lens or generic-optics with 'sizeConstraintSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrsChangeToken :: Lens.Lens' CreateSizeConstraintSetResponse (Lude.Maybe Lude.Text)
cscsrsChangeToken = Lens.lens (changeToken :: CreateSizeConstraintSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateSizeConstraintSetResponse)
{-# DEPRECATED cscsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsrsResponseStatus :: Lens.Lens' CreateSizeConstraintSetResponse Lude.Int
cscsrsResponseStatus = Lens.lens (responseStatus :: CreateSizeConstraintSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSizeConstraintSetResponse)
{-# DEPRECATED cscsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
