{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
  ( LoadBalancerTLSCertificateDomainValidationOption (..),

    -- * Smart constructor
    mkLoadBalancerTLSCertificateDomainValidationOption,

    -- * Lenses
    lbtcdvoDomainName,
    lbtcdvoValidationStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the domain names on an SSL/TLS certificate that you will use to validate domain ownership.
--
-- /See:/ 'mkLoadBalancerTLSCertificateDomainValidationOption' smart constructor.
data LoadBalancerTLSCertificateDomainValidationOption = LoadBalancerTLSCertificateDomainValidationOption'
  { -- | The fully qualified domain name in the certificate request.
    domainName :: Lude.Maybe Lude.Text,
    -- | The status of the domain validation. Valid values are listed below.
    validationStatus :: Lude.Maybe LoadBalancerTLSCertificateDomainStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerTLSCertificateDomainValidationOption' with the minimum fields required to make a request.
--
-- * 'domainName' - The fully qualified domain name in the certificate request.
-- * 'validationStatus' - The status of the domain validation. Valid values are listed below.
mkLoadBalancerTLSCertificateDomainValidationOption ::
  LoadBalancerTLSCertificateDomainValidationOption
mkLoadBalancerTLSCertificateDomainValidationOption =
  LoadBalancerTLSCertificateDomainValidationOption'
    { domainName =
        Lude.Nothing,
      validationStatus = Lude.Nothing
    }

-- | The fully qualified domain name in the certificate request.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvoDomainName :: Lens.Lens' LoadBalancerTLSCertificateDomainValidationOption (Lude.Maybe Lude.Text)
lbtcdvoDomainName = Lens.lens (domainName :: LoadBalancerTLSCertificateDomainValidationOption -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: LoadBalancerTLSCertificateDomainValidationOption)
{-# DEPRECATED lbtcdvoDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The status of the domain validation. Valid values are listed below.
--
-- /Note:/ Consider using 'validationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcdvoValidationStatus :: Lens.Lens' LoadBalancerTLSCertificateDomainValidationOption (Lude.Maybe LoadBalancerTLSCertificateDomainStatus)
lbtcdvoValidationStatus = Lens.lens (validationStatus :: LoadBalancerTLSCertificateDomainValidationOption -> Lude.Maybe LoadBalancerTLSCertificateDomainStatus) (\s a -> s {validationStatus = a} :: LoadBalancerTLSCertificateDomainValidationOption)
{-# DEPRECATED lbtcdvoValidationStatus "Use generic-lens or generic-optics with 'validationStatus' instead." #-}

instance
  Lude.FromJSON
    LoadBalancerTLSCertificateDomainValidationOption
  where
  parseJSON =
    Lude.withObject
      "LoadBalancerTLSCertificateDomainValidationOption"
      ( \x ->
          LoadBalancerTLSCertificateDomainValidationOption'
            Lude.<$> (x Lude..:? "domainName") Lude.<*> (x Lude..:? "validationStatus")
      )
