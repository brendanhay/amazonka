{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus
import Network.AWS.Prelude

-- | Contains information about the domain names on an SSL/TLS certificate that you will use to validate domain ownership.
--
--
--
-- /See:/ 'loadBalancerTLSCertificateDomainValidationOption' smart constructor.
data LoadBalancerTLSCertificateDomainValidationOption = LoadBalancerTLSCertificateDomainValidationOption'
  { _lbtcdvoDomainName ::
      !( Maybe
           Text
       ),
    _lbtcdvoValidationStatus ::
      !( Maybe
           LoadBalancerTLSCertificateDomainStatus
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LoadBalancerTLSCertificateDomainValidationOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtcdvoDomainName' - The fully qualified domain name in the certificate request.
--
-- * 'lbtcdvoValidationStatus' - The status of the domain validation. Valid values are listed below.
loadBalancerTLSCertificateDomainValidationOption ::
  LoadBalancerTLSCertificateDomainValidationOption
loadBalancerTLSCertificateDomainValidationOption =
  LoadBalancerTLSCertificateDomainValidationOption'
    { _lbtcdvoDomainName =
        Nothing,
      _lbtcdvoValidationStatus = Nothing
    }

-- | The fully qualified domain name in the certificate request.
lbtcdvoDomainName :: Lens' LoadBalancerTLSCertificateDomainValidationOption (Maybe Text)
lbtcdvoDomainName = lens _lbtcdvoDomainName (\s a -> s {_lbtcdvoDomainName = a})

-- | The status of the domain validation. Valid values are listed below.
lbtcdvoValidationStatus :: Lens' LoadBalancerTLSCertificateDomainValidationOption (Maybe LoadBalancerTLSCertificateDomainStatus)
lbtcdvoValidationStatus = lens _lbtcdvoValidationStatus (\s a -> s {_lbtcdvoValidationStatus = a})

instance FromJSON LoadBalancerTLSCertificateDomainValidationOption where
  parseJSON =
    withObject
      "LoadBalancerTLSCertificateDomainValidationOption"
      ( \x ->
          LoadBalancerTLSCertificateDomainValidationOption'
            <$> (x .:? "domainName") <*> (x .:? "validationStatus")
      )

instance Hashable LoadBalancerTLSCertificateDomainValidationOption

instance NFData LoadBalancerTLSCertificateDomainValidationOption
