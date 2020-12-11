-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityVerificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityVerificationAttributes
  ( IdentityVerificationAttributes (..),

    -- * Smart constructor
    mkIdentityVerificationAttributes,

    -- * Lenses
    ivaVerificationToken,
    ivaVerificationStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.VerificationStatus

-- | Represents the verification attributes of a single identity.
--
-- /See:/ 'mkIdentityVerificationAttributes' smart constructor.
data IdentityVerificationAttributes = IdentityVerificationAttributes'
  { verificationToken ::
      Lude.Maybe Lude.Text,
    verificationStatus ::
      VerificationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityVerificationAttributes' with the minimum fields required to make a request.
--
-- * 'verificationStatus' - The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
-- * 'verificationToken' - The verification token for a domain identity. Null for email address identities.
mkIdentityVerificationAttributes ::
  -- | 'verificationStatus'
  VerificationStatus ->
  IdentityVerificationAttributes
mkIdentityVerificationAttributes pVerificationStatus_ =
  IdentityVerificationAttributes'
    { verificationToken = Lude.Nothing,
      verificationStatus = pVerificationStatus_
    }

-- | The verification token for a domain identity. Null for email address identities.
--
-- /Note:/ Consider using 'verificationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivaVerificationToken :: Lens.Lens' IdentityVerificationAttributes (Lude.Maybe Lude.Text)
ivaVerificationToken = Lens.lens (verificationToken :: IdentityVerificationAttributes -> Lude.Maybe Lude.Text) (\s a -> s {verificationToken = a} :: IdentityVerificationAttributes)
{-# DEPRECATED ivaVerificationToken "Use generic-lens or generic-optics with 'verificationToken' instead." #-}

-- | The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
--
-- /Note:/ Consider using 'verificationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivaVerificationStatus :: Lens.Lens' IdentityVerificationAttributes VerificationStatus
ivaVerificationStatus = Lens.lens (verificationStatus :: IdentityVerificationAttributes -> VerificationStatus) (\s a -> s {verificationStatus = a} :: IdentityVerificationAttributes)
{-# DEPRECATED ivaVerificationStatus "Use generic-lens or generic-optics with 'verificationStatus' instead." #-}

instance Lude.FromXML IdentityVerificationAttributes where
  parseXML x =
    IdentityVerificationAttributes'
      Lude.<$> (x Lude..@? "VerificationToken")
      Lude.<*> (x Lude..@ "VerificationStatus")
