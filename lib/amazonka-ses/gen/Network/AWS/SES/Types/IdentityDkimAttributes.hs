{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityDkimAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityDkimAttributes
  ( IdentityDkimAttributes (..),

    -- * Smart constructor
    mkIdentityDkimAttributes,

    -- * Lenses
    idaDkimEnabled,
    idaDkimVerificationStatus,
    idaDkimTokens,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.VerificationStatus as Types
import qualified Network.AWS.SES.Types.VerificationToken as Types

-- | Represents the DKIM attributes of a verified email address or a domain.
--
-- /See:/ 'mkIdentityDkimAttributes' smart constructor.
data IdentityDkimAttributes = IdentityDkimAttributes'
  { -- | Is true if DKIM signing is enabled for email sent from the identity. It's false otherwise. The default value is true.
    dkimEnabled :: Core.Bool,
    -- | Describes whether Amazon SES has successfully verified the DKIM DNS records (tokens) published in the domain name's DNS. (This only applies to domain identities, not email address identities.)
    dkimVerificationStatus :: Types.VerificationStatus,
    -- | A set of character strings that represent the domain's identity. Using these tokens, you need to create DNS CNAME records that point to DKIM public keys that are hosted by Amazon SES. Amazon Web Services eventually detects that you've updated your DNS records. This detection process might take up to 72 hours. After successful detection, Amazon SES is able to DKIM-sign email originating from that domain. (This only applies to domain identities, not email address identities.)
    --
    -- For more information about creating DNS records using DKIM tokens, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
    dkimTokens :: Core.Maybe [Types.VerificationToken]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IdentityDkimAttributes' value with any optional fields omitted.
mkIdentityDkimAttributes ::
  -- | 'dkimEnabled'
  Core.Bool ->
  -- | 'dkimVerificationStatus'
  Types.VerificationStatus ->
  IdentityDkimAttributes
mkIdentityDkimAttributes dkimEnabled dkimVerificationStatus =
  IdentityDkimAttributes'
    { dkimEnabled,
      dkimVerificationStatus,
      dkimTokens = Core.Nothing
    }

-- | Is true if DKIM signing is enabled for email sent from the identity. It's false otherwise. The default value is true.
--
-- /Note:/ Consider using 'dkimEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idaDkimEnabled :: Lens.Lens' IdentityDkimAttributes Core.Bool
idaDkimEnabled = Lens.field @"dkimEnabled"
{-# DEPRECATED idaDkimEnabled "Use generic-lens or generic-optics with 'dkimEnabled' instead." #-}

-- | Describes whether Amazon SES has successfully verified the DKIM DNS records (tokens) published in the domain name's DNS. (This only applies to domain identities, not email address identities.)
--
-- /Note:/ Consider using 'dkimVerificationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idaDkimVerificationStatus :: Lens.Lens' IdentityDkimAttributes Types.VerificationStatus
idaDkimVerificationStatus = Lens.field @"dkimVerificationStatus"
{-# DEPRECATED idaDkimVerificationStatus "Use generic-lens or generic-optics with 'dkimVerificationStatus' instead." #-}

-- | A set of character strings that represent the domain's identity. Using these tokens, you need to create DNS CNAME records that point to DKIM public keys that are hosted by Amazon SES. Amazon Web Services eventually detects that you've updated your DNS records. This detection process might take up to 72 hours. After successful detection, Amazon SES is able to DKIM-sign email originating from that domain. (This only applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'dkimTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idaDkimTokens :: Lens.Lens' IdentityDkimAttributes (Core.Maybe [Types.VerificationToken])
idaDkimTokens = Lens.field @"dkimTokens"
{-# DEPRECATED idaDkimTokens "Use generic-lens or generic-optics with 'dkimTokens' instead." #-}

instance Core.FromXML IdentityDkimAttributes where
  parseXML x =
    IdentityDkimAttributes'
      Core.<$> (x Core..@ "DkimEnabled")
      Core.<*> (x Core..@ "DkimVerificationStatus")
      Core.<*> (x Core..@? "DkimTokens" Core..<@> Core.parseXMLList "member")
