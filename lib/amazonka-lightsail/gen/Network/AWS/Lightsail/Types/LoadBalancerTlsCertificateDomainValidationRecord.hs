{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
  ( LoadBalancerTlsCertificateDomainValidationRecord (..)
  -- * Smart constructor
  , mkLoadBalancerTlsCertificateDomainValidationRecord
  -- * Lenses
  , lbtcdvrDomainName
  , lbtcdvrName
  , lbtcdvrType
  , lbtcdvrValidationStatus
  , lbtcdvrValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DomainName as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the validation record of each domain name in the SSL/TLS certificate.
--
-- /See:/ 'mkLoadBalancerTlsCertificateDomainValidationRecord' smart constructor.
data LoadBalancerTlsCertificateDomainValidationRecord = LoadBalancerTlsCertificateDomainValidationRecord'
  { domainName :: Core.Maybe Types.DomainName
    -- ^ The domain name against which your SSL/TLS certificate was validated.
  , name :: Core.Maybe Types.NonEmptyString
    -- ^ A fully qualified domain name in the certificate. For example, @example.com@ .
  , type' :: Core.Maybe Types.NonEmptyString
    -- ^ The type of validation record. For example, @CNAME@ for domain validation.
  , validationStatus :: Core.Maybe Types.LoadBalancerTlsCertificateDomainStatus
    -- ^ The validation status. Valid values are listed below.
  , value :: Core.Maybe Types.NonEmptyString
    -- ^ The value for that type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerTlsCertificateDomainValidationRecord' value with any optional fields omitted.
mkLoadBalancerTlsCertificateDomainValidationRecord
    :: LoadBalancerTlsCertificateDomainValidationRecord
mkLoadBalancerTlsCertificateDomainValidationRecord
  = LoadBalancerTlsCertificateDomainValidationRecord'{domainName =
                                                        Core.Nothing,
                                                      name = Core.Nothing, type' = Core.Nothing,
                                                      validationStatus = Core.Nothing,
                                                      value = Core.Nothing}

-- | The domain name against which your SSL/TLS certificate was validated.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrDomainName :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Types.DomainName)
lbtcdvrDomainName = Lens.field @"domainName"
{-# INLINEABLE lbtcdvrDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | A fully qualified domain name in the certificate. For example, @example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrName :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Types.NonEmptyString)
lbtcdvrName = Lens.field @"name"
{-# INLINEABLE lbtcdvrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of validation record. For example, @CNAME@ for domain validation.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrType :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Types.NonEmptyString)
lbtcdvrType = Lens.field @"type'"
{-# INLINEABLE lbtcdvrType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The validation status. Valid values are listed below.
--
-- /Note:/ Consider using 'validationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrValidationStatus :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Types.LoadBalancerTlsCertificateDomainStatus)
lbtcdvrValidationStatus = Lens.field @"validationStatus"
{-# INLINEABLE lbtcdvrValidationStatus #-}
{-# DEPRECATED validationStatus "Use generic-lens or generic-optics with 'validationStatus' instead"  #-}

-- | The value for that type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrValue :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Core.Maybe Types.NonEmptyString)
lbtcdvrValue = Lens.field @"value"
{-# INLINEABLE lbtcdvrValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON
           LoadBalancerTlsCertificateDomainValidationRecord
         where
        parseJSON
          = Core.withObject
              "LoadBalancerTlsCertificateDomainValidationRecord"
              Core.$
              \ x ->
                LoadBalancerTlsCertificateDomainValidationRecord' Core.<$>
                  (x Core..:? "domainName") Core.<*> x Core..:? "name" Core.<*>
                    x Core..:? "type"
                    Core.<*> x Core..:? "validationStatus"
                    Core.<*> x Core..:? "value"
