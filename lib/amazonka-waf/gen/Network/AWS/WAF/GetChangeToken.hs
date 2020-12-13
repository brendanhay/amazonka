{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetChangeToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you want to create, update, or delete AWS WAF objects, get a change token and include the change token in the create, update, or delete request. Change tokens ensure that your application doesn't submit conflicting requests to AWS WAF.
--
-- Each create, update, or delete request must use a unique change token. If your application submits a @GetChangeToken@ request and then submits a second @GetChangeToken@ request before submitting a create, update, or delete request, the second @GetChangeToken@ request returns the same value as the first @GetChangeToken@ request.
-- When you use a change token in a create, update, or delete request, the status of the change token changes to @PENDING@ , which indicates that AWS WAF is propagating the change to all AWS WAF servers. Use @GetChangeTokenStatus@ to determine the status of your change token.
module Network.AWS.WAF.GetChangeToken
  ( -- * Creating a request
    GetChangeToken (..),
    mkGetChangeToken,

    -- * Destructuring the response
    GetChangeTokenResponse (..),
    mkGetChangeTokenResponse,

    -- ** Response lenses
    gctrsChangeToken,
    gctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetChangeToken' smart constructor.
data GetChangeToken = GetChangeToken'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetChangeToken' with the minimum fields required to make a request.
mkGetChangeToken ::
  GetChangeToken
mkGetChangeToken = GetChangeToken'

instance Lude.AWSRequest GetChangeToken where
  type Rs GetChangeToken = GetChangeTokenResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetChangeTokenResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetChangeToken where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetChangeToken" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetChangeToken where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetChangeToken where
  toPath = Lude.const "/"

instance Lude.ToQuery GetChangeToken where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetChangeTokenResponse' smart constructor.
data GetChangeTokenResponse = GetChangeTokenResponse'
  { -- | The @ChangeToken@ that you used in the request. Use this value in a @GetChangeTokenStatus@ request to get the current status of the request.
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetChangeTokenResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used in the request. Use this value in a @GetChangeTokenStatus@ request to get the current status of the request.
-- * 'responseStatus' - The response status code.
mkGetChangeTokenResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetChangeTokenResponse
mkGetChangeTokenResponse pResponseStatus_ =
  GetChangeTokenResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used in the request. Use this value in a @GetChangeTokenStatus@ request to get the current status of the request.
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctrsChangeToken :: Lens.Lens' GetChangeTokenResponse (Lude.Maybe Lude.Text)
gctrsChangeToken = Lens.lens (changeToken :: GetChangeTokenResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: GetChangeTokenResponse)
{-# DEPRECATED gctrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctrsResponseStatus :: Lens.Lens' GetChangeTokenResponse Lude.Int
gctrsResponseStatus = Lens.lens (responseStatus :: GetChangeTokenResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetChangeTokenResponse)
{-# DEPRECATED gctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
