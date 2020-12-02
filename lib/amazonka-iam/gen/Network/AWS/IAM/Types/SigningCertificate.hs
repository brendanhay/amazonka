{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SigningCertificate where

import Network.AWS.IAM.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an X.509 signing certificate.
--
--
-- This data type is used as a response element in the 'UploadSigningCertificate' and 'ListSigningCertificates' operations.
--
--
-- /See:/ 'signingCertificate' smart constructor.
data SigningCertificate = SigningCertificate'
  { _scUploadDate ::
      !(Maybe ISO8601),
    _scUserName :: !Text,
    _scCertificateId :: !Text,
    _scCertificateBody :: !Text,
    _scStatus :: !StatusType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SigningCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scUploadDate' - The date when the signing certificate was uploaded.
--
-- * 'scUserName' - The name of the user the signing certificate is associated with.
--
-- * 'scCertificateId' - The ID for the signing certificate.
--
-- * 'scCertificateBody' - The contents of the signing certificate.
--
-- * 'scStatus' - The status of the signing certificate. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
signingCertificate ::
  -- | 'scUserName'
  Text ->
  -- | 'scCertificateId'
  Text ->
  -- | 'scCertificateBody'
  Text ->
  -- | 'scStatus'
  StatusType ->
  SigningCertificate
signingCertificate
  pUserName_
  pCertificateId_
  pCertificateBody_
  pStatus_ =
    SigningCertificate'
      { _scUploadDate = Nothing,
        _scUserName = pUserName_,
        _scCertificateId = pCertificateId_,
        _scCertificateBody = pCertificateBody_,
        _scStatus = pStatus_
      }

-- | The date when the signing certificate was uploaded.
scUploadDate :: Lens' SigningCertificate (Maybe UTCTime)
scUploadDate = lens _scUploadDate (\s a -> s {_scUploadDate = a}) . mapping _Time

-- | The name of the user the signing certificate is associated with.
scUserName :: Lens' SigningCertificate Text
scUserName = lens _scUserName (\s a -> s {_scUserName = a})

-- | The ID for the signing certificate.
scCertificateId :: Lens' SigningCertificate Text
scCertificateId = lens _scCertificateId (\s a -> s {_scCertificateId = a})

-- | The contents of the signing certificate.
scCertificateBody :: Lens' SigningCertificate Text
scCertificateBody = lens _scCertificateBody (\s a -> s {_scCertificateBody = a})

-- | The status of the signing certificate. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
scStatus :: Lens' SigningCertificate StatusType
scStatus = lens _scStatus (\s a -> s {_scStatus = a})

instance FromXML SigningCertificate where
  parseXML x =
    SigningCertificate'
      <$> (x .@? "UploadDate")
      <*> (x .@ "UserName")
      <*> (x .@ "CertificateId")
      <*> (x .@ "CertificateBody")
      <*> (x .@ "Status")

instance Hashable SigningCertificate

instance NFData SigningCertificate
