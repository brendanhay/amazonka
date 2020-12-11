{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetAccountBalance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAccountBalance@ operation retrieves the amount of money in your Amazon Mechanical Turk account.
module Network.AWS.MechanicalTurk.GetAccountBalance
  ( -- * Creating a request
    GetAccountBalance (..),
    mkGetAccountBalance,

    -- * Destructuring the response
    GetAccountBalanceResponse (..),
    mkGetAccountBalanceResponse,

    -- ** Response lenses
    gabrsAvailableBalance,
    gabrsOnHoldBalance,
    gabrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAccountBalance' smart constructor.
data GetAccountBalance = GetAccountBalance'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountBalance' with the minimum fields required to make a request.
mkGetAccountBalance ::
  GetAccountBalance
mkGetAccountBalance = GetAccountBalance'

instance Lude.AWSRequest GetAccountBalance where
  type Rs GetAccountBalance = GetAccountBalanceResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAccountBalanceResponse'
            Lude.<$> (x Lude..?> "AvailableBalance")
            Lude.<*> (x Lude..?> "OnHoldBalance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountBalance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.GetAccountBalance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAccountBalance where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetAccountBalance where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccountBalance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAccountBalanceResponse' smart constructor.
data GetAccountBalanceResponse = GetAccountBalanceResponse'
  { availableBalance ::
      Lude.Maybe Lude.Text,
    onHoldBalance :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetAccountBalanceResponse' with the minimum fields required to make a request.
--
-- * 'availableBalance' - Undocumented field.
-- * 'onHoldBalance' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetAccountBalanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountBalanceResponse
mkGetAccountBalanceResponse pResponseStatus_ =
  GetAccountBalanceResponse'
    { availableBalance = Lude.Nothing,
      onHoldBalance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'availableBalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrsAvailableBalance :: Lens.Lens' GetAccountBalanceResponse (Lude.Maybe Lude.Text)
gabrsAvailableBalance = Lens.lens (availableBalance :: GetAccountBalanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {availableBalance = a} :: GetAccountBalanceResponse)
{-# DEPRECATED gabrsAvailableBalance "Use generic-lens or generic-optics with 'availableBalance' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'onHoldBalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrsOnHoldBalance :: Lens.Lens' GetAccountBalanceResponse (Lude.Maybe Lude.Text)
gabrsOnHoldBalance = Lens.lens (onHoldBalance :: GetAccountBalanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {onHoldBalance = a} :: GetAccountBalanceResponse)
{-# DEPRECATED gabrsOnHoldBalance "Use generic-lens or generic-optics with 'onHoldBalance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gabrsResponseStatus :: Lens.Lens' GetAccountBalanceResponse Lude.Int
gabrsResponseStatus = Lens.lens (responseStatus :: GetAccountBalanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountBalanceResponse)
{-# DEPRECATED gabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
