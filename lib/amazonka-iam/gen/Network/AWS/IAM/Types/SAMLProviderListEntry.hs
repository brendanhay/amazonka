{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SAMLProviderListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.SAMLProviderListEntry
  ( SAMLProviderListEntry (..)
  -- * Smart constructor
  , mkSAMLProviderListEntry
  -- * Lenses
  , samlpleArn
  , samlpleCreateDate
  , samlpleValidUntil
  ) where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the list of SAML providers for this account.
--
-- /See:/ 'mkSAMLProviderListEntry' smart constructor.
data SAMLProviderListEntry = SAMLProviderListEntry'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the SAML provider.
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the SAML provider was created.
  , validUntil :: Core.Maybe Core.UTCTime
    -- ^ The expiration date and time for the SAML provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SAMLProviderListEntry' value with any optional fields omitted.
mkSAMLProviderListEntry
    :: SAMLProviderListEntry
mkSAMLProviderListEntry
  = SAMLProviderListEntry'{arn = Core.Nothing,
                           createDate = Core.Nothing, validUntil = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the SAML provider.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlpleArn :: Lens.Lens' SAMLProviderListEntry (Core.Maybe Types.Arn)
samlpleArn = Lens.field @"arn"
{-# INLINEABLE samlpleArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time when the SAML provider was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlpleCreateDate :: Lens.Lens' SAMLProviderListEntry (Core.Maybe Core.UTCTime)
samlpleCreateDate = Lens.field @"createDate"
{-# INLINEABLE samlpleCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | The expiration date and time for the SAML provider.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlpleValidUntil :: Lens.Lens' SAMLProviderListEntry (Core.Maybe Core.UTCTime)
samlpleValidUntil = Lens.field @"validUntil"
{-# INLINEABLE samlpleValidUntil #-}
{-# DEPRECATED validUntil "Use generic-lens or generic-optics with 'validUntil' instead"  #-}

instance Core.FromXML SAMLProviderListEntry where
        parseXML x
          = SAMLProviderListEntry' Core.<$>
              (x Core..@? "Arn") Core.<*> x Core..@? "CreateDate" Core.<*>
                x Core..@? "ValidUntil"
