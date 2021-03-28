{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.IdentityVerificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.IdentityVerificationAttributes
  ( IdentityVerificationAttributes (..)
  -- * Smart constructor
  , mkIdentityVerificationAttributes
  -- * Lenses
  , ivaVerificationStatus
  , ivaVerificationToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.VerificationStatus as Types
import qualified Network.AWS.SES.Types.VerificationToken as Types

-- | Represents the verification attributes of a single identity.
--
-- /See:/ 'mkIdentityVerificationAttributes' smart constructor.
data IdentityVerificationAttributes = IdentityVerificationAttributes'
  { verificationStatus :: Types.VerificationStatus
    -- ^ The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
  , verificationToken :: Core.Maybe Types.VerificationToken
    -- ^ The verification token for a domain identity. Null for email address identities.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IdentityVerificationAttributes' value with any optional fields omitted.
mkIdentityVerificationAttributes
    :: Types.VerificationStatus -- ^ 'verificationStatus'
    -> IdentityVerificationAttributes
mkIdentityVerificationAttributes verificationStatus
  = IdentityVerificationAttributes'{verificationStatus,
                                    verificationToken = Core.Nothing}

-- | The verification status of the identity: "Pending", "Success", "Failed", or "TemporaryFailure".
--
-- /Note:/ Consider using 'verificationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivaVerificationStatus :: Lens.Lens' IdentityVerificationAttributes Types.VerificationStatus
ivaVerificationStatus = Lens.field @"verificationStatus"
{-# INLINEABLE ivaVerificationStatus #-}
{-# DEPRECATED verificationStatus "Use generic-lens or generic-optics with 'verificationStatus' instead"  #-}

-- | The verification token for a domain identity. Null for email address identities.
--
-- /Note:/ Consider using 'verificationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivaVerificationToken :: Lens.Lens' IdentityVerificationAttributes (Core.Maybe Types.VerificationToken)
ivaVerificationToken = Lens.field @"verificationToken"
{-# INLINEABLE ivaVerificationToken #-}
{-# DEPRECATED verificationToken "Use generic-lens or generic-optics with 'verificationToken' instead"  #-}

instance Core.FromXML IdentityVerificationAttributes where
        parseXML x
          = IdentityVerificationAttributes' Core.<$>
              (x Core..@ "VerificationStatus") Core.<*>
                x Core..@? "VerificationToken"
