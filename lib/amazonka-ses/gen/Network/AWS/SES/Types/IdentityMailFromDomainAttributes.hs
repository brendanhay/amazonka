{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityMailFromDomainAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityMailFromDomainAttributes
  ( IdentityMailFromDomainAttributes (..),

    -- * Smart constructor
    mkIdentityMailFromDomainAttributes,

    -- * Lenses
    imfdaMailFromDomain,
    imfdaMailFromDomainStatus,
    imfdaBehaviorOnMXFailure,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.BehaviorOnMXFailure
import Network.AWS.SES.Types.CustomMailFromStatus

-- | Represents the custom MAIL FROM domain attributes of a verified identity (email address or domain).
--
-- /See:/ 'mkIdentityMailFromDomainAttributes' smart constructor.
data IdentityMailFromDomainAttributes = IdentityMailFromDomainAttributes'
  { mailFromDomain ::
      Lude.Text,
    mailFromDomainStatus ::
      CustomMailFromStatus,
    behaviorOnMXFailure ::
      BehaviorOnMXFailure
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityMailFromDomainAttributes' with the minimum fields required to make a request.
--
-- * 'behaviorOnMXFailure' - The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email.
--
-- The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
-- * 'mailFromDomain' - The custom MAIL FROM domain that the identity is configured to use.
-- * 'mailFromDomainStatus' - The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
mkIdentityMailFromDomainAttributes ::
  -- | 'mailFromDomain'
  Lude.Text ->
  -- | 'mailFromDomainStatus'
  CustomMailFromStatus ->
  -- | 'behaviorOnMXFailure'
  BehaviorOnMXFailure ->
  IdentityMailFromDomainAttributes
mkIdentityMailFromDomainAttributes
  pMailFromDomain_
  pMailFromDomainStatus_
  pBehaviorOnMXFailure_ =
    IdentityMailFromDomainAttributes'
      { mailFromDomain =
          pMailFromDomain_,
        mailFromDomainStatus = pMailFromDomainStatus_,
        behaviorOnMXFailure = pBehaviorOnMXFailure_
      }

-- | The custom MAIL FROM domain that the identity is configured to use.
--
-- /Note:/ Consider using 'mailFromDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfdaMailFromDomain :: Lens.Lens' IdentityMailFromDomainAttributes Lude.Text
imfdaMailFromDomain = Lens.lens (mailFromDomain :: IdentityMailFromDomainAttributes -> Lude.Text) (\s a -> s {mailFromDomain = a} :: IdentityMailFromDomainAttributes)
{-# DEPRECATED imfdaMailFromDomain "Use generic-lens or generic-optics with 'mailFromDomain' instead." #-}

-- | The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
--
-- /Note:/ Consider using 'mailFromDomainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfdaMailFromDomainStatus :: Lens.Lens' IdentityMailFromDomainAttributes CustomMailFromStatus
imfdaMailFromDomainStatus = Lens.lens (mailFromDomainStatus :: IdentityMailFromDomainAttributes -> CustomMailFromStatus) (\s a -> s {mailFromDomainStatus = a} :: IdentityMailFromDomainAttributes)
{-# DEPRECATED imfdaMailFromDomainStatus "Use generic-lens or generic-optics with 'mailFromDomainStatus' instead." #-}

-- | The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email.
--
-- The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
--
-- /Note:/ Consider using 'behaviorOnMXFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfdaBehaviorOnMXFailure :: Lens.Lens' IdentityMailFromDomainAttributes BehaviorOnMXFailure
imfdaBehaviorOnMXFailure = Lens.lens (behaviorOnMXFailure :: IdentityMailFromDomainAttributes -> BehaviorOnMXFailure) (\s a -> s {behaviorOnMXFailure = a} :: IdentityMailFromDomainAttributes)
{-# DEPRECATED imfdaBehaviorOnMXFailure "Use generic-lens or generic-optics with 'behaviorOnMXFailure' instead." #-}

instance Lude.FromXML IdentityMailFromDomainAttributes where
  parseXML x =
    IdentityMailFromDomainAttributes'
      Lude.<$> (x Lude..@ "MailFromDomain")
      Lude.<*> (x Lude..@ "MailFromDomainStatus")
      Lude.<*> (x Lude..@ "BehaviorOnMXFailure")
