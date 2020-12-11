{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (an email address or a domain), enables or disables whether Amazon SES forwards bounce and complaint notifications as email. Feedback forwarding can only be disabled when Amazon Simple Notification Service (Amazon SNS) topics are specified for both bounces and complaints.
--
-- You can execute this operation no more than once per second.
-- For more information about using notifications with Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
  ( -- * Creating a request
    SetIdentityFeedbackForwardingEnabled (..),
    mkSetIdentityFeedbackForwardingEnabled,

    -- ** Request lenses
    siffeIdentity,
    siffeForwardingEnabled,

    -- * Destructuring the response
    SetIdentityFeedbackForwardingEnabledResponse (..),
    mkSetIdentityFeedbackForwardingEnabledResponse,

    -- ** Response lenses
    siffersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to enable or disable whether Amazon SES forwards you bounce and complaint notifications through email. For information about email feedback forwarding, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-email.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityFeedbackForwardingEnabled' smart constructor.
data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled'
  { identity ::
      Lude.Text,
    forwardingEnabled ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityFeedbackForwardingEnabled' with the minimum fields required to make a request.
--
-- * 'forwardingEnabled' - Sets whether Amazon SES will forward bounce and complaint notifications as email. @true@ specifies that Amazon SES will forward bounce and complaint notifications as email, in addition to any Amazon SNS topic publishing otherwise specified. @false@ specifies that Amazon SES will publish bounce and complaint notifications only through Amazon SNS. This value can only be set to @false@ when Amazon SNS topics are set for both @Bounce@ and @Complaint@ notification types.
-- * 'identity' - The identity for which to set bounce and complaint notification forwarding. Examples: @user@example.com@ , @example.com@ .
mkSetIdentityFeedbackForwardingEnabled ::
  -- | 'identity'
  Lude.Text ->
  -- | 'forwardingEnabled'
  Lude.Bool ->
  SetIdentityFeedbackForwardingEnabled
mkSetIdentityFeedbackForwardingEnabled
  pIdentity_
  pForwardingEnabled_ =
    SetIdentityFeedbackForwardingEnabled'
      { identity = pIdentity_,
        forwardingEnabled = pForwardingEnabled_
      }

-- | The identity for which to set bounce and complaint notification forwarding. Examples: @user@example.com@ , @example.com@ .
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siffeIdentity :: Lens.Lens' SetIdentityFeedbackForwardingEnabled Lude.Text
siffeIdentity = Lens.lens (identity :: SetIdentityFeedbackForwardingEnabled -> Lude.Text) (\s a -> s {identity = a} :: SetIdentityFeedbackForwardingEnabled)
{-# DEPRECATED siffeIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | Sets whether Amazon SES will forward bounce and complaint notifications as email. @true@ specifies that Amazon SES will forward bounce and complaint notifications as email, in addition to any Amazon SNS topic publishing otherwise specified. @false@ specifies that Amazon SES will publish bounce and complaint notifications only through Amazon SNS. This value can only be set to @false@ when Amazon SNS topics are set for both @Bounce@ and @Complaint@ notification types.
--
-- /Note:/ Consider using 'forwardingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siffeForwardingEnabled :: Lens.Lens' SetIdentityFeedbackForwardingEnabled Lude.Bool
siffeForwardingEnabled = Lens.lens (forwardingEnabled :: SetIdentityFeedbackForwardingEnabled -> Lude.Bool) (\s a -> s {forwardingEnabled = a} :: SetIdentityFeedbackForwardingEnabled)
{-# DEPRECATED siffeForwardingEnabled "Use generic-lens or generic-optics with 'forwardingEnabled' instead." #-}

instance Lude.AWSRequest SetIdentityFeedbackForwardingEnabled where
  type
    Rs SetIdentityFeedbackForwardingEnabled =
      SetIdentityFeedbackForwardingEnabledResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SetIdentityFeedbackForwardingEnabledResult"
      ( \s h x ->
          SetIdentityFeedbackForwardingEnabledResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetIdentityFeedbackForwardingEnabled where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetIdentityFeedbackForwardingEnabled where
  toPath = Lude.const "/"

instance Lude.ToQuery SetIdentityFeedbackForwardingEnabled where
  toQuery SetIdentityFeedbackForwardingEnabled' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SetIdentityFeedbackForwardingEnabled" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identity" Lude.=: identity,
        "ForwardingEnabled" Lude.=: forwardingEnabled
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityFeedbackForwardingEnabledResponse' smart constructor.
newtype SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'SetIdentityFeedbackForwardingEnabledResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetIdentityFeedbackForwardingEnabledResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetIdentityFeedbackForwardingEnabledResponse
mkSetIdentityFeedbackForwardingEnabledResponse pResponseStatus_ =
  SetIdentityFeedbackForwardingEnabledResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siffersResponseStatus :: Lens.Lens' SetIdentityFeedbackForwardingEnabledResponse Lude.Int
siffersResponseStatus = Lens.lens (responseStatus :: SetIdentityFeedbackForwardingEnabledResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetIdentityFeedbackForwardingEnabledResponse)
{-# DEPRECATED siffersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
