{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityDkimEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables Easy DKIM signing of email sent from an identity. If Easy DKIM signing is enabled for a domain, then Amazon SES uses DKIM to sign all email that it sends from addresses on that domain. If Easy DKIM signing is enabled for an email address, then Amazon SES uses DKIM to sign all email it sends from that address.
--
-- You can enable DKIM signing for an identity at any time after you start the verification process for the identity, even if the verification process isn't complete.
-- You can execute this operation no more than once per second.
-- For more information about Easy DKIM signing, go to the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
module Network.AWS.SES.SetIdentityDkimEnabled
  ( -- * Creating a request
    SetIdentityDkimEnabled (..),
    mkSetIdentityDkimEnabled,

    -- ** Request lenses
    sideIdentity,
    sideDkimEnabled,

    -- * Destructuring the response
    SetIdentityDkimEnabledResponse (..),
    mkSetIdentityDkimEnabledResponse,

    -- ** Response lenses
    sidersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to enable or disable Amazon SES Easy DKIM signing for an identity. For more information about setting up Easy DKIM, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityDkimEnabled' smart constructor.
data SetIdentityDkimEnabled = SetIdentityDkimEnabled'
  { -- | The identity for which DKIM signing should be enabled or disabled.
    identity :: Lude.Text,
    -- | Sets whether DKIM signing is enabled for an identity. Set to @true@ to enable DKIM signing for this identity; @false@ to disable it.
    dkimEnabled :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityDkimEnabled' with the minimum fields required to make a request.
--
-- * 'identity' - The identity for which DKIM signing should be enabled or disabled.
-- * 'dkimEnabled' - Sets whether DKIM signing is enabled for an identity. Set to @true@ to enable DKIM signing for this identity; @false@ to disable it.
mkSetIdentityDkimEnabled ::
  -- | 'identity'
  Lude.Text ->
  -- | 'dkimEnabled'
  Lude.Bool ->
  SetIdentityDkimEnabled
mkSetIdentityDkimEnabled pIdentity_ pDkimEnabled_ =
  SetIdentityDkimEnabled'
    { identity = pIdentity_,
      dkimEnabled = pDkimEnabled_
    }

-- | The identity for which DKIM signing should be enabled or disabled.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sideIdentity :: Lens.Lens' SetIdentityDkimEnabled Lude.Text
sideIdentity = Lens.lens (identity :: SetIdentityDkimEnabled -> Lude.Text) (\s a -> s {identity = a} :: SetIdentityDkimEnabled)
{-# DEPRECATED sideIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | Sets whether DKIM signing is enabled for an identity. Set to @true@ to enable DKIM signing for this identity; @false@ to disable it.
--
-- /Note:/ Consider using 'dkimEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sideDkimEnabled :: Lens.Lens' SetIdentityDkimEnabled Lude.Bool
sideDkimEnabled = Lens.lens (dkimEnabled :: SetIdentityDkimEnabled -> Lude.Bool) (\s a -> s {dkimEnabled = a} :: SetIdentityDkimEnabled)
{-# DEPRECATED sideDkimEnabled "Use generic-lens or generic-optics with 'dkimEnabled' instead." #-}

instance Lude.AWSRequest SetIdentityDkimEnabled where
  type Rs SetIdentityDkimEnabled = SetIdentityDkimEnabledResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SetIdentityDkimEnabledResult"
      ( \s h x ->
          SetIdentityDkimEnabledResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetIdentityDkimEnabled where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetIdentityDkimEnabled where
  toPath = Lude.const "/"

instance Lude.ToQuery SetIdentityDkimEnabled where
  toQuery SetIdentityDkimEnabled' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetIdentityDkimEnabled" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identity" Lude.=: identity,
        "DkimEnabled" Lude.=: dkimEnabled
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityDkimEnabledResponse' smart constructor.
newtype SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityDkimEnabledResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetIdentityDkimEnabledResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetIdentityDkimEnabledResponse
mkSetIdentityDkimEnabledResponse pResponseStatus_ =
  SetIdentityDkimEnabledResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sidersResponseStatus :: Lens.Lens' SetIdentityDkimEnabledResponse Lude.Int
sidersResponseStatus = Lens.lens (responseStatus :: SetIdentityDkimEnabledResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetIdentityDkimEnabledResponse)
{-# DEPRECATED sidersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
