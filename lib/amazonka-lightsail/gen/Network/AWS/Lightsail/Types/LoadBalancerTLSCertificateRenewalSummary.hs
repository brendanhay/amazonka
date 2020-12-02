{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
import Network.AWS.Prelude

-- | Contains information about the status of Lightsail's managed renewal for the certificate.
--
--
-- The renewal status of the certificate.
--
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names in the certificate. No further action is required.
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names in the certificate. You must take action to validate these domain names or the certificate won't be renewed. If you used DNS validation, check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
--
--
-- /See:/ 'loadBalancerTLSCertificateRenewalSummary' smart constructor.
data LoadBalancerTLSCertificateRenewalSummary = LoadBalancerTLSCertificateRenewalSummary'
  { _lbtcrsRenewalStatus ::
      !( Maybe
           LoadBalancerTLSCertificateRenewalStatus
       ),
    _lbtcrsDomainValidationOptions ::
      !( Maybe
           [LoadBalancerTLSCertificateDomainValidationOption]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerTLSCertificateRenewalSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtcrsRenewalStatus' - The renewal status of the certificate. The following renewal status are possible:     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.      * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.      * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
-- * 'lbtcrsDomainValidationOptions' - Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
loadBalancerTLSCertificateRenewalSummary ::
  LoadBalancerTLSCertificateRenewalSummary
loadBalancerTLSCertificateRenewalSummary =
  LoadBalancerTLSCertificateRenewalSummary'
    { _lbtcrsRenewalStatus =
        Nothing,
      _lbtcrsDomainValidationOptions = Nothing
    }

-- | The renewal status of the certificate. The following renewal status are possible:     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.      * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.      * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
lbtcrsRenewalStatus :: Lens' LoadBalancerTLSCertificateRenewalSummary (Maybe LoadBalancerTLSCertificateRenewalStatus)
lbtcrsRenewalStatus = lens _lbtcrsRenewalStatus (\s a -> s {_lbtcrsRenewalStatus = a})

-- | Contains information about the validation of each domain name in the certificate, as it pertains to Lightsail's managed renewal. This is different from the initial validation that occurs as a result of the RequestCertificate request.
lbtcrsDomainValidationOptions :: Lens' LoadBalancerTLSCertificateRenewalSummary [LoadBalancerTLSCertificateDomainValidationOption]
lbtcrsDomainValidationOptions = lens _lbtcrsDomainValidationOptions (\s a -> s {_lbtcrsDomainValidationOptions = a}) . _Default . _Coerce

instance FromJSON LoadBalancerTLSCertificateRenewalSummary where
  parseJSON =
    withObject
      "LoadBalancerTLSCertificateRenewalSummary"
      ( \x ->
          LoadBalancerTLSCertificateRenewalSummary'
            <$> (x .:? "renewalStatus")
            <*> (x .:? "domainValidationOptions" .!= mempty)
      )

instance Hashable LoadBalancerTLSCertificateRenewalSummary

instance NFData LoadBalancerTLSCertificateRenewalSummary
