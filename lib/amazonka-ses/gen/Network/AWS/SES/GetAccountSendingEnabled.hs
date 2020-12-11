{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetAccountSendingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the email sending status of the Amazon SES account for the current region.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetAccountSendingEnabled
  ( -- * Creating a request
    GetAccountSendingEnabled (..),
    mkGetAccountSendingEnabled,

    -- * Destructuring the response
    GetAccountSendingEnabledResponse (..),
    mkGetAccountSendingEnabledResponse,

    -- ** Response lenses
    gasersEnabled,
    gasersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkGetAccountSendingEnabled' smart constructor.
data GetAccountSendingEnabled = GetAccountSendingEnabled'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountSendingEnabled' with the minimum fields required to make a request.
mkGetAccountSendingEnabled ::
  GetAccountSendingEnabled
mkGetAccountSendingEnabled = GetAccountSendingEnabled'

instance Lude.AWSRequest GetAccountSendingEnabled where
  type Rs GetAccountSendingEnabled = GetAccountSendingEnabledResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetAccountSendingEnabledResult"
      ( \s h x ->
          GetAccountSendingEnabledResponse'
            Lude.<$> (x Lude..@? "Enabled") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccountSendingEnabled where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccountSendingEnabled where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccountSendingEnabled where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GetAccountSendingEnabled" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | Represents a request to return the email sending status for your Amazon SES account in the current AWS Region.
--
-- /See:/ 'mkGetAccountSendingEnabledResponse' smart constructor.
data GetAccountSendingEnabledResponse = GetAccountSendingEnabledResponse'
  { enabled ::
      Lude.Maybe Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccountSendingEnabledResponse' with the minimum fields required to make a request.
--
-- * 'enabled' - Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
-- * 'responseStatus' - The response status code.
mkGetAccountSendingEnabledResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccountSendingEnabledResponse
mkGetAccountSendingEnabledResponse pResponseStatus_ =
  GetAccountSendingEnabledResponse'
    { enabled = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasersEnabled :: Lens.Lens' GetAccountSendingEnabledResponse (Lude.Maybe Lude.Bool)
gasersEnabled = Lens.lens (enabled :: GetAccountSendingEnabledResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: GetAccountSendingEnabledResponse)
{-# DEPRECATED gasersEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasersResponseStatus :: Lens.Lens' GetAccountSendingEnabledResponse Lude.Int
gasersResponseStatus = Lens.lens (responseStatus :: GetAccountSendingEnabledResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccountSendingEnabledResponse)
{-# DEPRECATED gasersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
