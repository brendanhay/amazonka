{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord
  ( LoadBalancerTLSCertificateDomainValidationRecord (..),

    -- * Smart constructor
    mkLoadBalancerTLSCertificateDomainValidationRecord,

    -- * Lenses
    lbtcdvrValue,
    lbtcdvrDomainName,
    lbtcdvrName,
    lbtcdvrValidationStatus,
    lbtcdvrType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus
import qualified Network.AWS.Prelude as Lude

-- | Describes the validation record of each domain name in the SSL/TLS certificate.
--
-- /See:/ 'mkLoadBalancerTLSCertificateDomainValidationRecord' smart constructor.
data LoadBalancerTLSCertificateDomainValidationRecord = LoadBalancerTLSCertificateDomainValidationRecord'
  { -- | The value for that type.
    value :: Lude.Maybe Lude.Text,
    -- | The domain name against which your SSL/TLS certificate was validated.
    domainName :: Lude.Maybe Lude.Text,
    -- | A fully qualified domain name in the certificate. For example, @example.com@ .
    name :: Lude.Maybe Lude.Text,
    -- | The validation status. Valid values are listed below.
    validationStatus :: Lude.Maybe LoadBalancerTLSCertificateDomainStatus,
    -- | The type of validation record. For example, @CNAME@ for domain validation.
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerTLSCertificateDomainValidationRecord' with the minimum fields required to make a request.
--
-- * 'value' - The value for that type.
-- * 'domainName' - The domain name against which your SSL/TLS certificate was validated.
-- * 'name' - A fully qualified domain name in the certificate. For example, @example.com@ .
-- * 'validationStatus' - The validation status. Valid values are listed below.
-- * 'type'' - The type of validation record. For example, @CNAME@ for domain validation.
mkLoadBalancerTLSCertificateDomainValidationRecord ::
  LoadBalancerTLSCertificateDomainValidationRecord
mkLoadBalancerTLSCertificateDomainValidationRecord =
  LoadBalancerTLSCertificateDomainValidationRecord'
    { value =
        Lude.Nothing,
      domainName = Lude.Nothing,
      name = Lude.Nothing,
      validationStatus = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The value for that type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrValue :: Lens.Lens' LoadBalancerTLSCertificateDomainValidationRecord (Lude.Maybe Lude.Text)
lbtcdvrValue = Lens.lens (value :: LoadBalancerTLSCertificateDomainValidationRecord -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: LoadBalancerTLSCertificateDomainValidationRecord)
{-# DEPRECATED lbtcdvrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The domain name against which your SSL/TLS certificate was validated.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrDomainName :: Lens.Lens' LoadBalancerTLSCertificateDomainValidationRecord (Lude.Maybe Lude.Text)
lbtcdvrDomainName = Lens.lens (domainName :: LoadBalancerTLSCertificateDomainValidationRecord -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: LoadBalancerTLSCertificateDomainValidationRecord)
{-# DEPRECATED lbtcdvrDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A fully qualified domain name in the certificate. For example, @example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrName :: Lens.Lens' LoadBalancerTLSCertificateDomainValidationRecord (Lude.Maybe Lude.Text)
lbtcdvrName = Lens.lens (name :: LoadBalancerTLSCertificateDomainValidationRecord -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LoadBalancerTLSCertificateDomainValidationRecord)
{-# DEPRECATED lbtcdvrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The validation status. Valid values are listed below.
--
-- /Note:/ Consider using 'validationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrValidationStatus :: Lens.Lens' LoadBalancerTLSCertificateDomainValidationRecord (Lude.Maybe LoadBalancerTLSCertificateDomainStatus)
lbtcdvrValidationStatus = Lens.lens (validationStatus :: LoadBalancerTLSCertificateDomainValidationRecord -> Lude.Maybe LoadBalancerTLSCertificateDomainStatus) (\s a -> s {validationStatus = a} :: LoadBalancerTLSCertificateDomainValidationRecord)
{-# DEPRECATED lbtcdvrValidationStatus "Use generic-lens or generic-optics with 'validationStatus' instead." #-}

-- | The type of validation record. For example, @CNAME@ for domain validation.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvrType :: Lens.Lens' LoadBalancerTLSCertificateDomainValidationRecord (Lude.Maybe Lude.Text)
lbtcdvrType = Lens.lens (type' :: LoadBalancerTLSCertificateDomainValidationRecord -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: LoadBalancerTLSCertificateDomainValidationRecord)
{-# DEPRECATED lbtcdvrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance
  Lude.FromJSON
    LoadBalancerTLSCertificateDomainValidationRecord
  where
  parseJSON =
    Lude.withObject
      "LoadBalancerTLSCertificateDomainValidationRecord"
      ( \x ->
          LoadBalancerTLSCertificateDomainValidationRecord'
            Lude.<$> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "domainName")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "validationStatus")
            Lude.<*> (x Lude..:? "type")
      )
