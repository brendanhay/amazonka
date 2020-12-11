{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @ByteMatchSet@ . You then use 'UpdateByteMatchSet' to identify the part of a web request that you want AWS WAF to inspect, such as the values of the @User-Agent@ header or the query string. For example, you can create a @ByteMatchSet@ that matches any requests with @User-Agent@ headers that contain the string @BadBot@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @ByteMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateByteMatchSet@ request.
--
--
--     * Submit a @CreateByteMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateByteMatchSet@ request.
--
--
--     * Submit an 'UpdateByteMatchSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateByteMatchSet
  ( -- * Creating a request
    CreateByteMatchSet (..),
    mkCreateByteMatchSet,

    -- ** Request lenses
    cbmsName,
    cbmsChangeToken,

    -- * Destructuring the response
    CreateByteMatchSetResponse (..),
    mkCreateByteMatchSetResponse,

    -- ** Response lenses
    cbmsrsByteMatchSet,
    cbmsrsChangeToken,
    cbmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkCreateByteMatchSet' smart constructor.
data CreateByteMatchSet = CreateByteMatchSet'
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

-- | Creates a value of 'CreateByteMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'name' - A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
mkCreateByteMatchSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateByteMatchSet
mkCreateByteMatchSet pName_ pChangeToken_ =
  CreateByteMatchSet' {name = pName_, changeToken = pChangeToken_}

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsName :: Lens.Lens' CreateByteMatchSet Lude.Text
cbmsName = Lens.lens (name :: CreateByteMatchSet -> Lude.Text) (\s a -> s {name = a} :: CreateByteMatchSet)
{-# DEPRECATED cbmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsChangeToken :: Lens.Lens' CreateByteMatchSet Lude.Text
cbmsChangeToken = Lens.lens (changeToken :: CreateByteMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateByteMatchSet)
{-# DEPRECATED cbmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateByteMatchSet where
  type Rs CreateByteMatchSet = CreateByteMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateByteMatchSetResponse'
            Lude.<$> (x Lude..?> "ByteMatchSet")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateByteMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.CreateByteMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateByteMatchSet where
  toJSON CreateByteMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateByteMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateByteMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateByteMatchSetResponse' smart constructor.
data CreateByteMatchSetResponse = CreateByteMatchSetResponse'
  { byteMatchSet ::
      Lude.Maybe ByteMatchSet,
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

-- | Creates a value of 'CreateByteMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'byteMatchSet' - A 'ByteMatchSet' that contains no @ByteMatchTuple@ objects.
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkCreateByteMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateByteMatchSetResponse
mkCreateByteMatchSetResponse pResponseStatus_ =
  CreateByteMatchSetResponse'
    { byteMatchSet = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'ByteMatchSet' that contains no @ByteMatchTuple@ objects.
--
-- /Note:/ Consider using 'byteMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsrsByteMatchSet :: Lens.Lens' CreateByteMatchSetResponse (Lude.Maybe ByteMatchSet)
cbmsrsByteMatchSet = Lens.lens (byteMatchSet :: CreateByteMatchSetResponse -> Lude.Maybe ByteMatchSet) (\s a -> s {byteMatchSet = a} :: CreateByteMatchSetResponse)
{-# DEPRECATED cbmsrsByteMatchSet "Use generic-lens or generic-optics with 'byteMatchSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsrsChangeToken :: Lens.Lens' CreateByteMatchSetResponse (Lude.Maybe Lude.Text)
cbmsrsChangeToken = Lens.lens (changeToken :: CreateByteMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateByteMatchSetResponse)
{-# DEPRECATED cbmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbmsrsResponseStatus :: Lens.Lens' CreateByteMatchSetResponse Lude.Int
cbmsrsResponseStatus = Lens.lens (responseStatus :: CreateByteMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateByteMatchSetResponse)
{-# DEPRECATED cbmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
