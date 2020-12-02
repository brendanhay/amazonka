{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateDescription where

import Network.AWS.IoT.Types.CertificateMode
import Network.AWS.IoT.Types.CertificateStatus
import Network.AWS.IoT.Types.CertificateValidity
import Network.AWS.IoT.Types.TransferData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a certificate.
--
--
--
-- /See:/ 'certificateDescription' smart constructor.
data CertificateDescription = CertificateDescription'
  { _cdStatus ::
      !(Maybe CertificateStatus),
    _cdOwnedBy :: !(Maybe Text),
    _cdLastModifiedDate :: !(Maybe POSIX),
    _cdCaCertificateId :: !(Maybe Text),
    _cdPreviousOwnedBy :: !(Maybe Text),
    _cdCertificatePem :: !(Maybe Text),
    _cdCertificateARN :: !(Maybe Text),
    _cdCertificateId :: !(Maybe Text),
    _cdCertificateMode ::
      !(Maybe CertificateMode),
    _cdValidity :: !(Maybe CertificateValidity),
    _cdCreationDate :: !(Maybe POSIX),
    _cdGenerationId :: !(Maybe Text),
    _cdTransferData :: !(Maybe TransferData),
    _cdCustomerVersion :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdStatus' - The status of the certificate.
--
-- * 'cdOwnedBy' - The ID of the AWS account that owns the certificate.
--
-- * 'cdLastModifiedDate' - The date and time the certificate was last modified.
--
-- * 'cdCaCertificateId' - The certificate ID of the CA certificate used to sign this certificate.
--
-- * 'cdPreviousOwnedBy' - The ID of the AWS account of the previous owner of the certificate.
--
-- * 'cdCertificatePem' - The certificate data, in PEM format.
--
-- * 'cdCertificateARN' - The ARN of the certificate.
--
-- * 'cdCertificateId' - The ID of the certificate.
--
-- * 'cdCertificateMode' - The mode of the certificate.
--
-- * 'cdValidity' - When the certificate is valid.
--
-- * 'cdCreationDate' - The date and time the certificate was created.
--
-- * 'cdGenerationId' - The generation ID of the certificate.
--
-- * 'cdTransferData' - The transfer data.
--
-- * 'cdCustomerVersion' - The customer version of the certificate.
certificateDescription ::
  CertificateDescription
certificateDescription =
  CertificateDescription'
    { _cdStatus = Nothing,
      _cdOwnedBy = Nothing,
      _cdLastModifiedDate = Nothing,
      _cdCaCertificateId = Nothing,
      _cdPreviousOwnedBy = Nothing,
      _cdCertificatePem = Nothing,
      _cdCertificateARN = Nothing,
      _cdCertificateId = Nothing,
      _cdCertificateMode = Nothing,
      _cdValidity = Nothing,
      _cdCreationDate = Nothing,
      _cdGenerationId = Nothing,
      _cdTransferData = Nothing,
      _cdCustomerVersion = Nothing
    }

-- | The status of the certificate.
cdStatus :: Lens' CertificateDescription (Maybe CertificateStatus)
cdStatus = lens _cdStatus (\s a -> s {_cdStatus = a})

-- | The ID of the AWS account that owns the certificate.
cdOwnedBy :: Lens' CertificateDescription (Maybe Text)
cdOwnedBy = lens _cdOwnedBy (\s a -> s {_cdOwnedBy = a})

-- | The date and time the certificate was last modified.
cdLastModifiedDate :: Lens' CertificateDescription (Maybe UTCTime)
cdLastModifiedDate = lens _cdLastModifiedDate (\s a -> s {_cdLastModifiedDate = a}) . mapping _Time

-- | The certificate ID of the CA certificate used to sign this certificate.
cdCaCertificateId :: Lens' CertificateDescription (Maybe Text)
cdCaCertificateId = lens _cdCaCertificateId (\s a -> s {_cdCaCertificateId = a})

-- | The ID of the AWS account of the previous owner of the certificate.
cdPreviousOwnedBy :: Lens' CertificateDescription (Maybe Text)
cdPreviousOwnedBy = lens _cdPreviousOwnedBy (\s a -> s {_cdPreviousOwnedBy = a})

-- | The certificate data, in PEM format.
cdCertificatePem :: Lens' CertificateDescription (Maybe Text)
cdCertificatePem = lens _cdCertificatePem (\s a -> s {_cdCertificatePem = a})

-- | The ARN of the certificate.
cdCertificateARN :: Lens' CertificateDescription (Maybe Text)
cdCertificateARN = lens _cdCertificateARN (\s a -> s {_cdCertificateARN = a})

-- | The ID of the certificate.
cdCertificateId :: Lens' CertificateDescription (Maybe Text)
cdCertificateId = lens _cdCertificateId (\s a -> s {_cdCertificateId = a})

-- | The mode of the certificate.
cdCertificateMode :: Lens' CertificateDescription (Maybe CertificateMode)
cdCertificateMode = lens _cdCertificateMode (\s a -> s {_cdCertificateMode = a})

-- | When the certificate is valid.
cdValidity :: Lens' CertificateDescription (Maybe CertificateValidity)
cdValidity = lens _cdValidity (\s a -> s {_cdValidity = a})

-- | The date and time the certificate was created.
cdCreationDate :: Lens' CertificateDescription (Maybe UTCTime)
cdCreationDate = lens _cdCreationDate (\s a -> s {_cdCreationDate = a}) . mapping _Time

-- | The generation ID of the certificate.
cdGenerationId :: Lens' CertificateDescription (Maybe Text)
cdGenerationId = lens _cdGenerationId (\s a -> s {_cdGenerationId = a})

-- | The transfer data.
cdTransferData :: Lens' CertificateDescription (Maybe TransferData)
cdTransferData = lens _cdTransferData (\s a -> s {_cdTransferData = a})

-- | The customer version of the certificate.
cdCustomerVersion :: Lens' CertificateDescription (Maybe Natural)
cdCustomerVersion = lens _cdCustomerVersion (\s a -> s {_cdCustomerVersion = a}) . mapping _Nat

instance FromJSON CertificateDescription where
  parseJSON =
    withObject
      "CertificateDescription"
      ( \x ->
          CertificateDescription'
            <$> (x .:? "status")
            <*> (x .:? "ownedBy")
            <*> (x .:? "lastModifiedDate")
            <*> (x .:? "caCertificateId")
            <*> (x .:? "previousOwnedBy")
            <*> (x .:? "certificatePem")
            <*> (x .:? "certificateArn")
            <*> (x .:? "certificateId")
            <*> (x .:? "certificateMode")
            <*> (x .:? "validity")
            <*> (x .:? "creationDate")
            <*> (x .:? "generationId")
            <*> (x .:? "transferData")
            <*> (x .:? "customerVersion")
      )

instance Hashable CertificateDescription

instance NFData CertificateDescription
