{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DomainValidationRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.DomainValidationRecord
  ( DomainValidationRecord (..)
  -- * Smart constructor
  , mkDomainValidationRecord
  -- * Lenses
  , dvrDomainName
  , dvrResourceRecord
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DomainName as Types
import qualified Network.AWS.Lightsail.Types.ResourceRecord as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the domain validation records of an Amazon Lightsail SSL/TLS certificate.
--
-- /See:/ 'mkDomainValidationRecord' smart constructor.
data DomainValidationRecord = DomainValidationRecord'
  { domainName :: Core.Maybe Types.DomainName
    -- ^ The domain name of the certificate validation record. For example, @example.com@ or @www.example.com@ .
  , resourceRecord :: Core.Maybe Types.ResourceRecord
    -- ^ An object that describes the DNS records to add to your domain's DNS to validate it for the certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainValidationRecord' value with any optional fields omitted.
mkDomainValidationRecord
    :: DomainValidationRecord
mkDomainValidationRecord
  = DomainValidationRecord'{domainName = Core.Nothing,
                            resourceRecord = Core.Nothing}

-- | The domain name of the certificate validation record. For example, @example.com@ or @www.example.com@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrDomainName :: Lens.Lens' DomainValidationRecord (Core.Maybe Types.DomainName)
dvrDomainName = Lens.field @"domainName"
{-# INLINEABLE dvrDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | An object that describes the DNS records to add to your domain's DNS to validate it for the certificate.
--
-- /Note:/ Consider using 'resourceRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrResourceRecord :: Lens.Lens' DomainValidationRecord (Core.Maybe Types.ResourceRecord)
dvrResourceRecord = Lens.field @"resourceRecord"
{-# INLINEABLE dvrResourceRecord #-}
{-# DEPRECATED resourceRecord "Use generic-lens or generic-optics with 'resourceRecord' instead"  #-}

instance Core.FromJSON DomainValidationRecord where
        parseJSON
          = Core.withObject "DomainValidationRecord" Core.$
              \ x ->
                DomainValidationRecord' Core.<$>
                  (x Core..:? "domainName") Core.<*> x Core..:? "resourceRecord"
