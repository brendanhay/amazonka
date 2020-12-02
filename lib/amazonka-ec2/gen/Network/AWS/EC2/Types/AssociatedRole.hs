{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociatedRole where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the associated IAM roles.
--
--
--
-- /See:/ 'associatedRole' smart constructor.
data AssociatedRole = AssociatedRole'
  { _arCertificateS3BucketName ::
      !(Maybe Text),
    _arCertificateS3ObjectKey :: !(Maybe Text),
    _arEncryptionKMSKeyId :: !(Maybe Text),
    _arAssociatedRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociatedRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arCertificateS3BucketName' - The name of the Amazon S3 bucket in which the Amazon S3 object is stored.
--
-- * 'arCertificateS3ObjectKey' - The key of the Amazon S3 object ey where the certificate, certificate chain, and encrypted private key bundle is stored. The object key is formated as follows: @certificate_arn@ /@role_arn@ .
--
-- * 'arEncryptionKMSKeyId' - The ID of the KMS customer master key (CMK) used to encrypt the private key.
--
-- * 'arAssociatedRoleARN' - The ARN of the associated IAM role.
associatedRole ::
  AssociatedRole
associatedRole =
  AssociatedRole'
    { _arCertificateS3BucketName = Nothing,
      _arCertificateS3ObjectKey = Nothing,
      _arEncryptionKMSKeyId = Nothing,
      _arAssociatedRoleARN = Nothing
    }

-- | The name of the Amazon S3 bucket in which the Amazon S3 object is stored.
arCertificateS3BucketName :: Lens' AssociatedRole (Maybe Text)
arCertificateS3BucketName = lens _arCertificateS3BucketName (\s a -> s {_arCertificateS3BucketName = a})

-- | The key of the Amazon S3 object ey where the certificate, certificate chain, and encrypted private key bundle is stored. The object key is formated as follows: @certificate_arn@ /@role_arn@ .
arCertificateS3ObjectKey :: Lens' AssociatedRole (Maybe Text)
arCertificateS3ObjectKey = lens _arCertificateS3ObjectKey (\s a -> s {_arCertificateS3ObjectKey = a})

-- | The ID of the KMS customer master key (CMK) used to encrypt the private key.
arEncryptionKMSKeyId :: Lens' AssociatedRole (Maybe Text)
arEncryptionKMSKeyId = lens _arEncryptionKMSKeyId (\s a -> s {_arEncryptionKMSKeyId = a})

-- | The ARN of the associated IAM role.
arAssociatedRoleARN :: Lens' AssociatedRole (Maybe Text)
arAssociatedRoleARN = lens _arAssociatedRoleARN (\s a -> s {_arAssociatedRoleARN = a})

instance FromXML AssociatedRole where
  parseXML x =
    AssociatedRole'
      <$> (x .@? "certificateS3BucketName")
      <*> (x .@? "certificateS3ObjectKey")
      <*> (x .@? "encryptionKmsKeyId")
      <*> (x .@? "associatedRoleArn")

instance Hashable AssociatedRole

instance NFData AssociatedRole
