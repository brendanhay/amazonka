{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateAccountSendingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables email sending across your entire Amazon SES account in the current AWS Region. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending across your Amazon SES account in a given AWS Region when reputation metrics (such as your bounce or complaint rates) reach certain thresholds.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateAccountSendingEnabled
  ( -- * Creating a request
    UpdateAccountSendingEnabled (..),
    mkUpdateAccountSendingEnabled,

    -- ** Request lenses
    uaseEnabled,

    -- * Destructuring the response
    UpdateAccountSendingEnabledResponse (..),
    mkUpdateAccountSendingEnabledResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to enable or disable the email sending capabilities for your entire Amazon SES account.
--
-- /See:/ 'mkUpdateAccountSendingEnabled' smart constructor.
newtype UpdateAccountSendingEnabled = UpdateAccountSendingEnabled'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccountSendingEnabled' with the minimum fields required to make a request.
--
-- * 'enabled' - Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
mkUpdateAccountSendingEnabled ::
  UpdateAccountSendingEnabled
mkUpdateAccountSendingEnabled =
  UpdateAccountSendingEnabled' {enabled = Lude.Nothing}

-- | Describes whether email sending is enabled or disabled for your Amazon SES account in the current AWS Region.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaseEnabled :: Lens.Lens' UpdateAccountSendingEnabled (Lude.Maybe Lude.Bool)
uaseEnabled = Lens.lens (enabled :: UpdateAccountSendingEnabled -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UpdateAccountSendingEnabled)
{-# DEPRECATED uaseEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.AWSRequest UpdateAccountSendingEnabled where
  type
    Rs UpdateAccountSendingEnabled =
      UpdateAccountSendingEnabledResponse
  request = Req.postQuery sesService
  response = Res.receiveNull UpdateAccountSendingEnabledResponse'

instance Lude.ToHeaders UpdateAccountSendingEnabled where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateAccountSendingEnabled where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAccountSendingEnabled where
  toQuery UpdateAccountSendingEnabled' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateAccountSendingEnabled" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Enabled" Lude.=: enabled
      ]

-- | /See:/ 'mkUpdateAccountSendingEnabledResponse' smart constructor.
data UpdateAccountSendingEnabledResponse = UpdateAccountSendingEnabledResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccountSendingEnabledResponse' with the minimum fields required to make a request.
mkUpdateAccountSendingEnabledResponse ::
  UpdateAccountSendingEnabledResponse
mkUpdateAccountSendingEnabledResponse =
  UpdateAccountSendingEnabledResponse'
