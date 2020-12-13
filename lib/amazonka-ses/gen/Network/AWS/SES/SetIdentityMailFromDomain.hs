{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SetIdentityMailFromDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables the custom MAIL FROM domain setup for a verified identity (an email address or a domain).
--
-- /Important:/ To send emails using the specified MAIL FROM domain, you must add an MX record to your MAIL FROM domain's DNS settings. If you want your emails to pass Sender Policy Framework (SPF) checks, you must also add or update an SPF record. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-set.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SetIdentityMailFromDomain
  ( -- * Creating a request
    SetIdentityMailFromDomain (..),
    mkSetIdentityMailFromDomain,

    -- ** Request lenses
    simfdMailFromDomain,
    simfdBehaviorOnMXFailure,
    simfdIdentity,

    -- * Destructuring the response
    SetIdentityMailFromDomainResponse (..),
    mkSetIdentityMailFromDomainResponse,

    -- ** Response lenses
    simfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to enable or disable the Amazon SES custom MAIL FROM domain setup for a verified identity. For information about using a custom MAIL FROM domain, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkSetIdentityMailFromDomain' smart constructor.
data SetIdentityMailFromDomain = SetIdentityMailFromDomain'
  { -- | The custom MAIL FROM domain that you want the verified identity to use. The MAIL FROM domain must 1) be a subdomain of the verified identity, 2) not be used in a "From" address if the MAIL FROM domain is the destination of email feedback forwarding (for more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> ), and 3) not be used to receive emails. A value of @null@ disables the custom MAIL FROM setting for the identity.
    mailFromDomain :: Lude.Maybe Lude.Text,
    -- | The action that you want Amazon SES to take if it cannot successfully read the required MX record when you send an email. If you choose @UseDefaultValue@ , Amazon SES will use amazonses.com (or a subdomain of that) as the MAIL FROM domain. If you choose @RejectMessage@ , Amazon SES will return a @MailFromDomainNotVerified@ error and not send the email.
    --
    -- The action specified in @BehaviorOnMXFailure@ is taken when the custom MAIL FROM domain setup is in the @Pending@ , @Failed@ , and @TemporaryFailure@ states.
    behaviorOnMXFailure :: Lude.Maybe BehaviorOnMXFailure,
    -- | The verified identity for which you want to enable or disable the specified custom MAIL FROM domain.
    identity :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityMailFromDomain' with the minimum fields required to make a request.
--
-- * 'mailFromDomain' - The custom MAIL FROM domain that you want the verified identity to use. The MAIL FROM domain must 1) be a subdomain of the verified identity, 2) not be used in a "From" address if the MAIL FROM domain is the destination of email feedback forwarding (for more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> ), and 3) not be used to receive emails. A value of @null@ disables the custom MAIL FROM setting for the identity.
-- * 'behaviorOnMXFailure' - The action that you want Amazon SES to take if it cannot successfully read the required MX record when you send an email. If you choose @UseDefaultValue@ , Amazon SES will use amazonses.com (or a subdomain of that) as the MAIL FROM domain. If you choose @RejectMessage@ , Amazon SES will return a @MailFromDomainNotVerified@ error and not send the email.
--
-- The action specified in @BehaviorOnMXFailure@ is taken when the custom MAIL FROM domain setup is in the @Pending@ , @Failed@ , and @TemporaryFailure@ states.
-- * 'identity' - The verified identity for which you want to enable or disable the specified custom MAIL FROM domain.
mkSetIdentityMailFromDomain ::
  -- | 'identity'
  Lude.Text ->
  SetIdentityMailFromDomain
mkSetIdentityMailFromDomain pIdentity_ =
  SetIdentityMailFromDomain'
    { mailFromDomain = Lude.Nothing,
      behaviorOnMXFailure = Lude.Nothing,
      identity = pIdentity_
    }

-- | The custom MAIL FROM domain that you want the verified identity to use. The MAIL FROM domain must 1) be a subdomain of the verified identity, 2) not be used in a "From" address if the MAIL FROM domain is the destination of email feedback forwarding (for more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> ), and 3) not be used to receive emails. A value of @null@ disables the custom MAIL FROM setting for the identity.
--
-- /Note:/ Consider using 'mailFromDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simfdMailFromDomain :: Lens.Lens' SetIdentityMailFromDomain (Lude.Maybe Lude.Text)
simfdMailFromDomain = Lens.lens (mailFromDomain :: SetIdentityMailFromDomain -> Lude.Maybe Lude.Text) (\s a -> s {mailFromDomain = a} :: SetIdentityMailFromDomain)
{-# DEPRECATED simfdMailFromDomain "Use generic-lens or generic-optics with 'mailFromDomain' instead." #-}

-- | The action that you want Amazon SES to take if it cannot successfully read the required MX record when you send an email. If you choose @UseDefaultValue@ , Amazon SES will use amazonses.com (or a subdomain of that) as the MAIL FROM domain. If you choose @RejectMessage@ , Amazon SES will return a @MailFromDomainNotVerified@ error and not send the email.
--
-- The action specified in @BehaviorOnMXFailure@ is taken when the custom MAIL FROM domain setup is in the @Pending@ , @Failed@ , and @TemporaryFailure@ states.
--
-- /Note:/ Consider using 'behaviorOnMXFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simfdBehaviorOnMXFailure :: Lens.Lens' SetIdentityMailFromDomain (Lude.Maybe BehaviorOnMXFailure)
simfdBehaviorOnMXFailure = Lens.lens (behaviorOnMXFailure :: SetIdentityMailFromDomain -> Lude.Maybe BehaviorOnMXFailure) (\s a -> s {behaviorOnMXFailure = a} :: SetIdentityMailFromDomain)
{-# DEPRECATED simfdBehaviorOnMXFailure "Use generic-lens or generic-optics with 'behaviorOnMXFailure' instead." #-}

-- | The verified identity for which you want to enable or disable the specified custom MAIL FROM domain.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simfdIdentity :: Lens.Lens' SetIdentityMailFromDomain Lude.Text
simfdIdentity = Lens.lens (identity :: SetIdentityMailFromDomain -> Lude.Text) (\s a -> s {identity = a} :: SetIdentityMailFromDomain)
{-# DEPRECATED simfdIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Lude.AWSRequest SetIdentityMailFromDomain where
  type
    Rs SetIdentityMailFromDomain =
      SetIdentityMailFromDomainResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "SetIdentityMailFromDomainResult"
      ( \s h x ->
          SetIdentityMailFromDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetIdentityMailFromDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetIdentityMailFromDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery SetIdentityMailFromDomain where
  toQuery SetIdentityMailFromDomain' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetIdentityMailFromDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "MailFromDomain" Lude.=: mailFromDomain,
        "BehaviorOnMXFailure" Lude.=: behaviorOnMXFailure,
        "Identity" Lude.=: identity
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkSetIdentityMailFromDomainResponse' smart constructor.
newtype SetIdentityMailFromDomainResponse = SetIdentityMailFromDomainResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityMailFromDomainResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetIdentityMailFromDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetIdentityMailFromDomainResponse
mkSetIdentityMailFromDomainResponse pResponseStatus_ =
  SetIdentityMailFromDomainResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simfdrsResponseStatus :: Lens.Lens' SetIdentityMailFromDomainResponse Lude.Int
simfdrsResponseStatus = Lens.lens (responseStatus :: SetIdentityMailFromDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetIdentityMailFromDomainResponse)
{-# DEPRECATED simfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
