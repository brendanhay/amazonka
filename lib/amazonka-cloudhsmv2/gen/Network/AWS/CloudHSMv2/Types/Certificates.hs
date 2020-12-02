{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Certificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Certificates where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains one or more certificates or a certificate signing request (CSR).
--
--
--
-- /See:/ 'certificates' smart constructor.
data Certificates = Certificates'
  { _cManufacturerHardwareCertificate ::
      !(Maybe Text),
    _cClusterCSR :: !(Maybe Text),
    _cHSMCertificate :: !(Maybe Text),
    _cClusterCertificate :: !(Maybe Text),
    _cAWSHardwareCertificate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Certificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cManufacturerHardwareCertificate' - The HSM hardware certificate issued (signed) by the hardware manufacturer.
--
-- * 'cClusterCSR' - The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
--
-- * 'cHSMCertificate' - The HSM certificate issued (signed) by the HSM hardware.
--
-- * 'cClusterCertificate' - The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
--
-- * 'cAWSHardwareCertificate' - The HSM hardware certificate issued (signed) by AWS CloudHSM.
certificates ::
  Certificates
certificates =
  Certificates'
    { _cManufacturerHardwareCertificate = Nothing,
      _cClusterCSR = Nothing,
      _cHSMCertificate = Nothing,
      _cClusterCertificate = Nothing,
      _cAWSHardwareCertificate = Nothing
    }

-- | The HSM hardware certificate issued (signed) by the hardware manufacturer.
cManufacturerHardwareCertificate :: Lens' Certificates (Maybe Text)
cManufacturerHardwareCertificate = lens _cManufacturerHardwareCertificate (\s a -> s {_cManufacturerHardwareCertificate = a})

-- | The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
cClusterCSR :: Lens' Certificates (Maybe Text)
cClusterCSR = lens _cClusterCSR (\s a -> s {_cClusterCSR = a})

-- | The HSM certificate issued (signed) by the HSM hardware.
cHSMCertificate :: Lens' Certificates (Maybe Text)
cHSMCertificate = lens _cHSMCertificate (\s a -> s {_cHSMCertificate = a})

-- | The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
cClusterCertificate :: Lens' Certificates (Maybe Text)
cClusterCertificate = lens _cClusterCertificate (\s a -> s {_cClusterCertificate = a})

-- | The HSM hardware certificate issued (signed) by AWS CloudHSM.
cAWSHardwareCertificate :: Lens' Certificates (Maybe Text)
cAWSHardwareCertificate = lens _cAWSHardwareCertificate (\s a -> s {_cAWSHardwareCertificate = a})

instance FromJSON Certificates where
  parseJSON =
    withObject
      "Certificates"
      ( \x ->
          Certificates'
            <$> (x .:? "ManufacturerHardwareCertificate")
            <*> (x .:? "ClusterCsr")
            <*> (x .:? "HsmCertificate")
            <*> (x .:? "ClusterCertificate")
            <*> (x .:? "AwsHardwareCertificate")
      )

instance Hashable Certificates

instance NFData Certificates
