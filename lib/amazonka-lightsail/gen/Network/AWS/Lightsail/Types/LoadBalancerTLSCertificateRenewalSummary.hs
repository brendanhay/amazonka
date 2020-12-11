-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
  ( LoadBalancerTLSCertificateRenewalSummary (..),

    -- * Smart constructor
    mkLoadBalancerTLSCertificateRenewalSummary,

    -- * Lenses
    lbtcrsRenewalStatus,
    lbtcrsDomainValidationOptions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the status of Lightsail's managed renewal for the certificate.
--
-- The renewal status of the certificate.
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names in the certificate. No further action is required.
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names in the certificate. You must take action to validate these domain names or the certificate won't be renewed. If you used DNS validation, check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
--
-- /See:/ 'mkLoadBalancerTLSCertificateRenewalSummary' smart constructor.
data LoadBalancerTLSCertificateRenewalSummary = LoadBalancerTLSCertificateRenewalSummary'
  { renewalStatus ::
      Lude.Maybe
        LoadBalancerTLSCertificateRenewalStatus,
    domainValidationOptions ::
      Lude.Maybe
        [LoadBalancerTLSCertificateDomainValidationOption]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerTLSCertificateRenewalSummary' with the minimum fields required to make a request.
--
-- * 'domainValidationOptions' - Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
-- * 'renewalStatus' - The renewal status of the certificate.
--
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
mkLoadBalancerTLSCertificateRenewalSummary ::
  LoadBalancerTLSCertificateRenewalSummary
mkLoadBalancerTLSCertificateRenewalSummary =
  LoadBalancerTLSCertificateRenewalSummary'
    { renewalStatus =
        Lude.Nothing,
      domainValidationOptions = Lude.Nothing
    }

-- | The renewal status of the certificate.
--
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
--
-- /Note:/ Consider using 'renewalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcrsRenewalStatus :: Lens.Lens' LoadBalancerTLSCertificateRenewalSummary (Lude.Maybe LoadBalancerTLSCertificateRenewalStatus)
lbtcrsRenewalStatus = Lens.lens (renewalStatus :: LoadBalancerTLSCertificateRenewalSummary -> Lude.Maybe LoadBalancerTLSCertificateRenewalStatus) (\s a -> s {renewalStatus = a} :: LoadBalancerTLSCertificateRenewalSummary)
{-# DEPRECATED lbtcrsRenewalStatus "Use generic-lens or generic-optics with 'renewalStatus' instead." #-}

-- | Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
--
-- /Note:/ Consider using 'domainValidationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcrsDomainValidationOptions :: Lens.Lens' LoadBalancerTLSCertificateRenewalSummary (Lude.Maybe [LoadBalancerTLSCertificateDomainValidationOption])
lbtcrsDomainValidationOptions = Lens.lens (domainValidationOptions :: LoadBalancerTLSCertificateRenewalSummary -> Lude.Maybe [LoadBalancerTLSCertificateDomainValidationOption]) (\s a -> s {domainValidationOptions = a} :: LoadBalancerTLSCertificateRenewalSummary)
{-# DEPRECATED lbtcrsDomainValidationOptions "Use generic-lens or generic-optics with 'domainValidationOptions' instead." #-}

instance Lude.FromJSON LoadBalancerTLSCertificateRenewalSummary where
  parseJSON =
    Lude.withObject
      "LoadBalancerTLSCertificateRenewalSummary"
      ( \x ->
          LoadBalancerTLSCertificateRenewalSummary'
            Lude.<$> (x Lude..:? "renewalStatus")
            Lude.<*> (x Lude..:? "domainValidationOptions" Lude..!= Lude.mempty)
      )
