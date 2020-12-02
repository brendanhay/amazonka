{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServerCertificateMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServerCertificateMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a server certificate without its certificate body, certificate chain, and private key.
--
--
-- This data type is used as a response element in the 'UploadServerCertificate' and 'ListServerCertificates' operations.
--
--
-- /See:/ 'serverCertificateMetadata' smart constructor.
data ServerCertificateMetadata = ServerCertificateMetadata'
  { _scmUploadDate ::
      !(Maybe ISO8601),
    _scmExpiration :: !(Maybe ISO8601),
    _scmPath :: !Text,
    _scmServerCertificateName :: !Text,
    _scmServerCertificateId :: !Text,
    _scmARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerCertificateMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmUploadDate' - The date when the server certificate was uploaded.
--
-- * 'scmExpiration' - The date on which the certificate is set to expire.
--
-- * 'scmPath' - The path to the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'scmServerCertificateName' - The name that identifies the server certificate.
--
-- * 'scmServerCertificateId' - The stable and unique string identifying the server certificate. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'scmARN' - The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
serverCertificateMetadata ::
  -- | 'scmPath'
  Text ->
  -- | 'scmServerCertificateName'
  Text ->
  -- | 'scmServerCertificateId'
  Text ->
  -- | 'scmARN'
  Text ->
  ServerCertificateMetadata
serverCertificateMetadata
  pPath_
  pServerCertificateName_
  pServerCertificateId_
  pARN_ =
    ServerCertificateMetadata'
      { _scmUploadDate = Nothing,
        _scmExpiration = Nothing,
        _scmPath = pPath_,
        _scmServerCertificateName = pServerCertificateName_,
        _scmServerCertificateId = pServerCertificateId_,
        _scmARN = pARN_
      }

-- | The date when the server certificate was uploaded.
scmUploadDate :: Lens' ServerCertificateMetadata (Maybe UTCTime)
scmUploadDate = lens _scmUploadDate (\s a -> s {_scmUploadDate = a}) . mapping _Time

-- | The date on which the certificate is set to expire.
scmExpiration :: Lens' ServerCertificateMetadata (Maybe UTCTime)
scmExpiration = lens _scmExpiration (\s a -> s {_scmExpiration = a}) . mapping _Time

-- | The path to the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
scmPath :: Lens' ServerCertificateMetadata Text
scmPath = lens _scmPath (\s a -> s {_scmPath = a})

-- | The name that identifies the server certificate.
scmServerCertificateName :: Lens' ServerCertificateMetadata Text
scmServerCertificateName = lens _scmServerCertificateName (\s a -> s {_scmServerCertificateName = a})

-- | The stable and unique string identifying the server certificate. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
scmServerCertificateId :: Lens' ServerCertificateMetadata Text
scmServerCertificateId = lens _scmServerCertificateId (\s a -> s {_scmServerCertificateId = a})

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
scmARN :: Lens' ServerCertificateMetadata Text
scmARN = lens _scmARN (\s a -> s {_scmARN = a})

instance FromXML ServerCertificateMetadata where
  parseXML x =
    ServerCertificateMetadata'
      <$> (x .@? "UploadDate")
      <*> (x .@? "Expiration")
      <*> (x .@ "Path")
      <*> (x .@ "ServerCertificateName")
      <*> (x .@ "ServerCertificateId")
      <*> (x .@ "Arn")

instance Hashable ServerCertificateMetadata

instance NFData ServerCertificateMetadata
