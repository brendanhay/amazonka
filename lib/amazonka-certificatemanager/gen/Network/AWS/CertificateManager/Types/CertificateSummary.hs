{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure is returned in the response object of 'ListCertificates' action.
--
--
--
-- /See:/ 'certificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { _csCertificateARN ::
      !(Maybe Text),
    _csDomainName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCertificateARN' - Amazon Resource Name (ARN) of the certificate. This is of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'csDomainName' - Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
certificateSummary ::
  CertificateSummary
certificateSummary =
  CertificateSummary'
    { _csCertificateARN = Nothing,
      _csDomainName = Nothing
    }

-- | Amazon Resource Name (ARN) of the certificate. This is of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
csCertificateARN :: Lens' CertificateSummary (Maybe Text)
csCertificateARN = lens _csCertificateARN (\s a -> s {_csCertificateARN = a})

-- | Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
csDomainName :: Lens' CertificateSummary (Maybe Text)
csDomainName = lens _csDomainName (\s a -> s {_csDomainName = a})

instance FromJSON CertificateSummary where
  parseJSON =
    withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            <$> (x .:? "CertificateArn") <*> (x .:? "DomainName")
      )

instance Hashable CertificateSummary

instance NFData CertificateSummary
