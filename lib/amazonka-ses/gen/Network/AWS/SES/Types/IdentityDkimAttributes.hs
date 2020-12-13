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
    idaDkimTokens,
    idaDkimEnabled,
    idaDkimVerificationStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.VerificationStatus

-- | Represents the DKIM attributes of a verified email address or a domain.
--
-- /See:/ 'mkIdentityDkimAttributes' smart constructor.
data IdentityDkimAttributes = IdentityDkimAttributes'
  { -- | A set of character strings that represent the domain's identity. Using these tokens, you need to create DNS CNAME records that point to DKIM public keys that are hosted by Amazon SES. Amazon Web Services eventually detects that you've updated your DNS records. This detection process might take up to 72 hours. After successful detection, Amazon SES is able to DKIM-sign email originating from that domain. (This only applies to domain identities, not email address identities.)
    --
    -- For more information about creating DNS records using DKIM tokens, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
    dkimTokens :: Lude.Maybe [Lude.Text],
    -- | Is true if DKIM signing is enabled for email sent from the identity. It's false otherwise. The default value is true.
    dkimEnabled :: Lude.Bool,
    -- | Describes whether Amazon SES has successfully verified the DKIM DNS records (tokens) published in the domain name's DNS. (This only applies to domain identities, not email address identities.)
    dkimVerificationStatus :: VerificationStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityDkimAttributes' with the minimum fields required to make a request.
--
-- * 'dkimTokens' - A set of character strings that represent the domain's identity. Using these tokens, you need to create DNS CNAME records that point to DKIM public keys that are hosted by Amazon SES. Amazon Web Services eventually detects that you've updated your DNS records. This detection process might take up to 72 hours. After successful detection, Amazon SES is able to DKIM-sign email originating from that domain. (This only applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
-- * 'dkimEnabled' - Is true if DKIM signing is enabled for email sent from the identity. It's false otherwise. The default value is true.
-- * 'dkimVerificationStatus' - Describes whether Amazon SES has successfully verified the DKIM DNS records (tokens) published in the domain name's DNS. (This only applies to domain identities, not email address identities.)
mkIdentityDkimAttributes ::
  -- | 'dkimEnabled'
  Lude.Bool ->
  -- | 'dkimVerificationStatus'
  VerificationStatus ->
  IdentityDkimAttributes
mkIdentityDkimAttributes pDkimEnabled_ pDkimVerificationStatus_ =
  IdentityDkimAttributes'
    { dkimTokens = Lude.Nothing,
      dkimEnabled = pDkimEnabled_,
      dkimVerificationStatus = pDkimVerificationStatus_
    }

-- | A set of character strings that represent the domain's identity. Using these tokens, you need to create DNS CNAME records that point to DKIM public keys that are hosted by Amazon SES. Amazon Web Services eventually detects that you've updated your DNS records. This detection process might take up to 72 hours. After successful detection, Amazon SES is able to DKIM-sign email originating from that domain. (This only applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'dkimTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idaDkimTokens :: Lens.Lens' IdentityDkimAttributes (Lude.Maybe [Lude.Text])
idaDkimTokens = Lens.lens (dkimTokens :: IdentityDkimAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {dkimTokens = a} :: IdentityDkimAttributes)
{-# DEPRECATED idaDkimTokens "Use generic-lens or generic-optics with 'dkimTokens' instead." #-}

-- | Is true if DKIM signing is enabled for email sent from the identity. It's false otherwise. The default value is true.
--
-- /Note:/ Consider using 'dkimEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idaDkimEnabled :: Lens.Lens' IdentityDkimAttributes Lude.Bool
idaDkimEnabled = Lens.lens (dkimEnabled :: IdentityDkimAttributes -> Lude.Bool) (\s a -> s {dkimEnabled = a} :: IdentityDkimAttributes)
{-# DEPRECATED idaDkimEnabled "Use generic-lens or generic-optics with 'dkimEnabled' instead." #-}

-- | Describes whether Amazon SES has successfully verified the DKIM DNS records (tokens) published in the domain name's DNS. (This only applies to domain identities, not email address identities.)
--
-- /Note:/ Consider using 'dkimVerificationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idaDkimVerificationStatus :: Lens.Lens' IdentityDkimAttributes VerificationStatus
idaDkimVerificationStatus = Lens.lens (dkimVerificationStatus :: IdentityDkimAttributes -> VerificationStatus) (\s a -> s {dkimVerificationStatus = a} :: IdentityDkimAttributes)
{-# DEPRECATED idaDkimVerificationStatus "Use generic-lens or generic-optics with 'dkimVerificationStatus' instead." #-}

instance Lude.FromXML IdentityDkimAttributes where
  parseXML x =
    IdentityDkimAttributes'
      Lude.<$> ( x Lude..@? "DkimTokens" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@ "DkimEnabled")
      Lude.<*> (x Lude..@ "DkimVerificationStatus")
