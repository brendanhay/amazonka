{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetChangeTokenStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a @ChangeToken@ that you got by calling 'GetChangeToken' . @ChangeTokenStatus@ is one of the following values:
--
--
--     * @PROVISIONED@ : You requested the change token by calling @GetChangeToken@ , but you haven't used it yet in a call to create, update, or delete an AWS WAF object.
--
--
--     * @PENDING@ : AWS WAF is propagating the create, update, or delete request to all AWS WAF servers.
--
--
--     * @INSYNC@ : Propagation is complete.
module Network.AWS.WAFRegional.GetChangeTokenStatus
  ( -- * Creating a request
    GetChangeTokenStatus (..),
    mkGetChangeTokenStatus,

    -- ** Request lenses
    gctsChangeToken,

    -- * Destructuring the response
    GetChangeTokenStatusResponse (..),
    mkGetChangeTokenStatusResponse,

    -- ** Response lenses
    gctsrsChangeTokenStatus,
    gctsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetChangeTokenStatus' smart constructor.
newtype GetChangeTokenStatus = GetChangeTokenStatus'
  { changeToken ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetChangeTokenStatus' with the minimum fields required to make a request.
--
-- * 'changeToken' - The change token for which you want to get the status. This change token was previously returned in the @GetChangeToken@ response.
mkGetChangeTokenStatus ::
  -- | 'changeToken'
  Lude.Text ->
  GetChangeTokenStatus
mkGetChangeTokenStatus pChangeToken_ =
  GetChangeTokenStatus' {changeToken = pChangeToken_}

-- | The change token for which you want to get the status. This change token was previously returned in the @GetChangeToken@ response.
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsChangeToken :: Lens.Lens' GetChangeTokenStatus Lude.Text
gctsChangeToken = Lens.lens (changeToken :: GetChangeTokenStatus -> Lude.Text) (\s a -> s {changeToken = a} :: GetChangeTokenStatus)
{-# DEPRECATED gctsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest GetChangeTokenStatus where
  type Rs GetChangeTokenStatus = GetChangeTokenStatusResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetChangeTokenStatusResponse'
            Lude.<$> (x Lude..?> "ChangeTokenStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetChangeTokenStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.GetChangeTokenStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetChangeTokenStatus where
  toJSON GetChangeTokenStatus' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ChangeToken" Lude..= changeToken)])

instance Lude.ToPath GetChangeTokenStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetChangeTokenStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetChangeTokenStatusResponse' smart constructor.
data GetChangeTokenStatusResponse = GetChangeTokenStatusResponse'
  { changeTokenStatus ::
      Lude.Maybe ChangeTokenStatus,
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

-- | Creates a value of 'GetChangeTokenStatusResponse' with the minimum fields required to make a request.
--
-- * 'changeTokenStatus' - The status of the change token.
-- * 'responseStatus' - The response status code.
mkGetChangeTokenStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetChangeTokenStatusResponse
mkGetChangeTokenStatusResponse pResponseStatus_ =
  GetChangeTokenStatusResponse'
    { changeTokenStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the change token.
--
-- /Note:/ Consider using 'changeTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsrsChangeTokenStatus :: Lens.Lens' GetChangeTokenStatusResponse (Lude.Maybe ChangeTokenStatus)
gctsrsChangeTokenStatus = Lens.lens (changeTokenStatus :: GetChangeTokenStatusResponse -> Lude.Maybe ChangeTokenStatus) (\s a -> s {changeTokenStatus = a} :: GetChangeTokenStatusResponse)
{-# DEPRECATED gctsrsChangeTokenStatus "Use generic-lens or generic-optics with 'changeTokenStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gctsrsResponseStatus :: Lens.Lens' GetChangeTokenStatusResponse Lude.Int
gctsrsResponseStatus = Lens.lens (responseStatus :: GetChangeTokenStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetChangeTokenStatusResponse)
{-# DEPRECATED gctsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
