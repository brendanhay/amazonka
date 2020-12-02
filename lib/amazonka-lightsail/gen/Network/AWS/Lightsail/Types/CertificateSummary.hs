{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CertificateSummary where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.Certificate
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes an Amazon Lightsail SSL/TLS certificate.
--
--
--
-- /See:/ 'certificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { _cCertificateDetail ::
      !(Maybe Certificate),
    _cCertificateName :: !(Maybe Text),
    _cCertificateARN :: !(Maybe Text),
    _cDomainName :: !(Maybe Text),
    _cTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateDetail' - An object that describes a certificate in detail.
--
-- * 'cCertificateName' - The name of the certificate.
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) of the certificate.
--
-- * 'cDomainName' - The domain name of the certificate.
--
-- * 'cTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
certificateSummary ::
  CertificateSummary
certificateSummary =
  CertificateSummary'
    { _cCertificateDetail = Nothing,
      _cCertificateName = Nothing,
      _cCertificateARN = Nothing,
      _cDomainName = Nothing,
      _cTags = Nothing
    }

-- | An object that describes a certificate in detail.
cCertificateDetail :: Lens' CertificateSummary (Maybe Certificate)
cCertificateDetail = lens _cCertificateDetail (\s a -> s {_cCertificateDetail = a})

-- | The name of the certificate.
cCertificateName :: Lens' CertificateSummary (Maybe Text)
cCertificateName = lens _cCertificateName (\s a -> s {_cCertificateName = a})

-- | The Amazon Resource Name (ARN) of the certificate.
cCertificateARN :: Lens' CertificateSummary (Maybe Text)
cCertificateARN = lens _cCertificateARN (\s a -> s {_cCertificateARN = a})

-- | The domain name of the certificate.
cDomainName :: Lens' CertificateSummary (Maybe Text)
cDomainName = lens _cDomainName (\s a -> s {_cDomainName = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
cTags :: Lens' CertificateSummary [Tag]
cTags = lens _cTags (\s a -> s {_cTags = a}) . _Default . _Coerce

instance FromJSON CertificateSummary where
  parseJSON =
    withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            <$> (x .:? "certificateDetail")
            <*> (x .:? "certificateName")
            <*> (x .:? "certificateArn")
            <*> (x .:? "domainName")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable CertificateSummary

instance NFData CertificateSummary
