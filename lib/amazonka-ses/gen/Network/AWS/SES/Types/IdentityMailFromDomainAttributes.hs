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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.BehaviorOnMXFailure as Types
import qualified Network.AWS.SES.Types.CustomMailFromStatus as Types
import qualified Network.AWS.SES.Types.MailFromDomain as Types

-- | Represents the custom MAIL FROM domain attributes of a verified identity (email address or domain).
--
-- /See:/ 'mkIdentityMailFromDomainAttributes' smart constructor.
data IdentityMailFromDomainAttributes = IdentityMailFromDomainAttributes'
  { -- | The custom MAIL FROM domain that the identity is configured to use.
    mailFromDomain :: Types.MailFromDomain,
    -- | The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
    mailFromDomainStatus :: Types.CustomMailFromStatus,
    -- | The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email.
    --
    -- The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
    behaviorOnMXFailure :: Types.BehaviorOnMXFailure
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IdentityMailFromDomainAttributes' value with any optional fields omitted.
mkIdentityMailFromDomainAttributes ::
  -- | 'mailFromDomain'
  Types.MailFromDomain ->
  -- | 'mailFromDomainStatus'
  Types.CustomMailFromStatus ->
  -- | 'behaviorOnMXFailure'
  Types.BehaviorOnMXFailure ->
  IdentityMailFromDomainAttributes
mkIdentityMailFromDomainAttributes
  mailFromDomain
  mailFromDomainStatus
  behaviorOnMXFailure =
    IdentityMailFromDomainAttributes'
      { mailFromDomain,
        mailFromDomainStatus,
        behaviorOnMXFailure
      }

-- | The custom MAIL FROM domain that the identity is configured to use.
--
-- /Note:/ Consider using 'mailFromDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfdaMailFromDomain :: Lens.Lens' IdentityMailFromDomainAttributes Types.MailFromDomain
imfdaMailFromDomain = Lens.field @"mailFromDomain"
{-# DEPRECATED imfdaMailFromDomain "Use generic-lens or generic-optics with 'mailFromDomain' instead." #-}

-- | The state that indicates whether Amazon SES has successfully read the MX record required for custom MAIL FROM domain setup. If the state is @Success@ , Amazon SES uses the specified custom MAIL FROM domain when the verified identity sends an email. All other states indicate that Amazon SES takes the action described by @BehaviorOnMXFailure@ .
--
-- /Note:/ Consider using 'mailFromDomainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfdaMailFromDomainStatus :: Lens.Lens' IdentityMailFromDomainAttributes Types.CustomMailFromStatus
imfdaMailFromDomainStatus = Lens.field @"mailFromDomainStatus"
{-# DEPRECATED imfdaMailFromDomainStatus "Use generic-lens or generic-optics with 'mailFromDomainStatus' instead." #-}

-- | The action that Amazon SES takes if it cannot successfully read the required MX record when you send an email. A value of @UseDefaultValue@ indicates that if Amazon SES cannot read the required MX record, it uses amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value of @RejectMessage@ indicates that if Amazon SES cannot read the required MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and does not send the email.
--
-- The custom MAIL FROM setup states that result in this behavior are @Pending@ , @Failed@ , and @TemporaryFailure@ .
--
-- /Note:/ Consider using 'behaviorOnMXFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imfdaBehaviorOnMXFailure :: Lens.Lens' IdentityMailFromDomainAttributes Types.BehaviorOnMXFailure
imfdaBehaviorOnMXFailure = Lens.field @"behaviorOnMXFailure"
{-# DEPRECATED imfdaBehaviorOnMXFailure "Use generic-lens or generic-optics with 'behaviorOnMXFailure' instead." #-}

instance Core.FromXML IdentityMailFromDomainAttributes where
  parseXML x =
    IdentityMailFromDomainAttributes'
      Core.<$> (x Core..@ "MailFromDomain")
      Core.<*> (x Core..@ "MailFromDomainStatus")
      Core.<*> (x Core..@ "BehaviorOnMXFailure")
