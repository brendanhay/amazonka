{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an 'IPSet' , which you use to specify which web requests that you want to allow or block based on the IP addresses that the requests originate from. For example, if you're receiving a lot of requests from one or more individual IP addresses or one or more ranges of IP addresses and you want to block the requests, you can create an @IPSet@ that contains those IP addresses and then configure AWS WAF to block the requests.
--
-- To create and configure an @IPSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateIPSet@ request.
--
--
--     * Submit a @CreateIPSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateIPSet@ request to specify the IP addresses that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateIPSet
  ( -- * Creating a request
    CreateIPSet (..),
    mkCreateIPSet,

    -- ** Request lenses
    cisName,
    cisChangeToken,

    -- * Destructuring the response
    CreateIPSetResponse (..),
    mkCreateIPSetResponse,

    -- ** Response lenses
    cisrsChangeToken,
    cisrsIPSet,
    cisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
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

-- | Creates a value of 'CreateIPSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'name' - A friendly name or description of the 'IPSet' . You can't change @Name@ after you create the @IPSet@ .
mkCreateIPSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateIPSet
mkCreateIPSet pName_ pChangeToken_ =
  CreateIPSet' {name = pName_, changeToken = pChangeToken_}

-- | A friendly name or description of the 'IPSet' . You can't change @Name@ after you create the @IPSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisName :: Lens.Lens' CreateIPSet Lude.Text
cisName = Lens.lens (name :: CreateIPSet -> Lude.Text) (\s a -> s {name = a} :: CreateIPSet)
{-# DEPRECATED cisName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisChangeToken :: Lens.Lens' CreateIPSet Lude.Text
cisChangeToken = Lens.lens (changeToken :: CreateIPSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateIPSet)
{-# DEPRECATED cisChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateIPSet where
  type Rs CreateIPSet = CreateIPSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateIPSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken")
            Lude.<*> (x Lude..?> "IPSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateIPSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.CreateIPSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateIPSet where
  toJSON CreateIPSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateIPSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateIPSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { changeToken ::
      Lude.Maybe Lude.Text,
    ipSet :: Lude.Maybe IPSet,
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

-- | Creates a value of 'CreateIPSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'ipSet' - The 'IPSet' returned in the @CreateIPSet@ response.
-- * 'responseStatus' - The response status code.
mkCreateIPSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateIPSetResponse
mkCreateIPSetResponse pResponseStatus_ =
  CreateIPSetResponse'
    { changeToken = Lude.Nothing,
      ipSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisrsChangeToken :: Lens.Lens' CreateIPSetResponse (Lude.Maybe Lude.Text)
cisrsChangeToken = Lens.lens (changeToken :: CreateIPSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateIPSetResponse)
{-# DEPRECATED cisrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The 'IPSet' returned in the @CreateIPSet@ response.
--
-- /Note:/ Consider using 'ipSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisrsIPSet :: Lens.Lens' CreateIPSetResponse (Lude.Maybe IPSet)
cisrsIPSet = Lens.lens (ipSet :: CreateIPSetResponse -> Lude.Maybe IPSet) (\s a -> s {ipSet = a} :: CreateIPSetResponse)
{-# DEPRECATED cisrsIPSet "Use generic-lens or generic-optics with 'ipSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisrsResponseStatus :: Lens.Lens' CreateIPSetResponse Lude.Int
cisrsResponseStatus = Lens.lens (responseStatus :: CreateIPSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateIPSetResponse)
{-# DEPRECATED cisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
