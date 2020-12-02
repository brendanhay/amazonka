{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateEnclaveCertificateIAMRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Identity and Access Management (IAM) role with an AWS Certificate Manager (ACM) certificate. This enables the certificate to be used by the ACM for Nitro Enclaves application inside an enclave. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave-refapp.html AWS Certificate Manager for Nitro Enclaves> in the /AWS Nitro Enclaves User Guide/ .
--
--
-- When the IAM role is associated with the ACM certificate, places the certificate, certificate chain, and encrypted private key in an Amazon S3 bucket that only the associated IAM role can access. The private key of the certificate is encrypted with an AWS-managed KMS customer master (CMK) that has an attached attestation-based CMK policy.
--
-- To enable the IAM role to access the Amazon S3 object, you must grant it permission to call @s3:GetObject@ on the Amazon S3 bucket returned by the command. To enable the IAM role to access the AWS KMS CMK, you must grant it permission to call @kms:Decrypt@ on AWS KMS CMK returned by the command. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave-refapp.html#add-policy Grant the role permission to access the certificate and encryption key> in the /AWS Nitro Enclaves User Guide/ .
module Network.AWS.EC2.AssociateEnclaveCertificateIAMRole
  ( -- * Creating a Request
    associateEnclaveCertificateIAMRole,
    AssociateEnclaveCertificateIAMRole,

    -- * Request Lenses
    aecirCertificateARN,
    aecirDryRun,
    aecirRoleARN,

    -- * Destructuring the Response
    associateEnclaveCertificateIAMRoleResponse,
    AssociateEnclaveCertificateIAMRoleResponse,

    -- * Response Lenses
    aecirrsCertificateS3BucketName,
    aecirrsCertificateS3ObjectKey,
    aecirrsEncryptionKMSKeyId,
    aecirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateEnclaveCertificateIAMRole' smart constructor.
data AssociateEnclaveCertificateIAMRole = AssociateEnclaveCertificateIAMRole'
  { _aecirCertificateARN ::
      !(Maybe Text),
    _aecirDryRun ::
      !(Maybe Bool),
    _aecirRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateEnclaveCertificateIAMRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aecirCertificateARN' - The ARN of the ACM certificate with which to associate the IAM role.
--
-- * 'aecirDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'aecirRoleARN' - The ARN of the IAM role to associate with the ACM certificate. You can associate up to 16 IAM roles with an ACM certificate.
associateEnclaveCertificateIAMRole ::
  AssociateEnclaveCertificateIAMRole
associateEnclaveCertificateIAMRole =
  AssociateEnclaveCertificateIAMRole'
    { _aecirCertificateARN =
        Nothing,
      _aecirDryRun = Nothing,
      _aecirRoleARN = Nothing
    }

-- | The ARN of the ACM certificate with which to associate the IAM role.
aecirCertificateARN :: Lens' AssociateEnclaveCertificateIAMRole (Maybe Text)
aecirCertificateARN = lens _aecirCertificateARN (\s a -> s {_aecirCertificateARN = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
aecirDryRun :: Lens' AssociateEnclaveCertificateIAMRole (Maybe Bool)
aecirDryRun = lens _aecirDryRun (\s a -> s {_aecirDryRun = a})

-- | The ARN of the IAM role to associate with the ACM certificate. You can associate up to 16 IAM roles with an ACM certificate.
aecirRoleARN :: Lens' AssociateEnclaveCertificateIAMRole (Maybe Text)
aecirRoleARN = lens _aecirRoleARN (\s a -> s {_aecirRoleARN = a})

instance AWSRequest AssociateEnclaveCertificateIAMRole where
  type
    Rs AssociateEnclaveCertificateIAMRole =
      AssociateEnclaveCertificateIAMRoleResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          AssociateEnclaveCertificateIAMRoleResponse'
            <$> (x .@? "certificateS3BucketName")
            <*> (x .@? "certificateS3ObjectKey")
            <*> (x .@? "encryptionKmsKeyId")
            <*> (pure (fromEnum s))
      )

instance Hashable AssociateEnclaveCertificateIAMRole

instance NFData AssociateEnclaveCertificateIAMRole

instance ToHeaders AssociateEnclaveCertificateIAMRole where
  toHeaders = const mempty

instance ToPath AssociateEnclaveCertificateIAMRole where
  toPath = const "/"

instance ToQuery AssociateEnclaveCertificateIAMRole where
  toQuery AssociateEnclaveCertificateIAMRole' {..} =
    mconcat
      [ "Action" =: ("AssociateEnclaveCertificateIamRole" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "CertificateArn" =: _aecirCertificateARN,
        "DryRun" =: _aecirDryRun,
        "RoleArn" =: _aecirRoleARN
      ]

-- | /See:/ 'associateEnclaveCertificateIAMRoleResponse' smart constructor.
data AssociateEnclaveCertificateIAMRoleResponse = AssociateEnclaveCertificateIAMRoleResponse'
  { _aecirrsCertificateS3BucketName ::
      !( Maybe
           Text
       ),
    _aecirrsCertificateS3ObjectKey ::
      !( Maybe
           Text
       ),
    _aecirrsEncryptionKMSKeyId ::
      !( Maybe
           Text
       ),
    _aecirrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AssociateEnclaveCertificateIAMRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aecirrsCertificateS3BucketName' - The name of the Amazon S3 bucket to which the certificate was uploaded.
--
-- * 'aecirrsCertificateS3ObjectKey' - The Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored. The object key is formatted as follows: @certificate_arn@ /@role_arn@ .
--
-- * 'aecirrsEncryptionKMSKeyId' - The ID of the AWS KMS CMK used to encrypt the private key of the certificate.
--
-- * 'aecirrsResponseStatus' - -- | The response status code.
associateEnclaveCertificateIAMRoleResponse ::
  -- | 'aecirrsResponseStatus'
  Int ->
  AssociateEnclaveCertificateIAMRoleResponse
associateEnclaveCertificateIAMRoleResponse pResponseStatus_ =
  AssociateEnclaveCertificateIAMRoleResponse'
    { _aecirrsCertificateS3BucketName =
        Nothing,
      _aecirrsCertificateS3ObjectKey = Nothing,
      _aecirrsEncryptionKMSKeyId = Nothing,
      _aecirrsResponseStatus = pResponseStatus_
    }

-- | The name of the Amazon S3 bucket to which the certificate was uploaded.
aecirrsCertificateS3BucketName :: Lens' AssociateEnclaveCertificateIAMRoleResponse (Maybe Text)
aecirrsCertificateS3BucketName = lens _aecirrsCertificateS3BucketName (\s a -> s {_aecirrsCertificateS3BucketName = a})

-- | The Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored. The object key is formatted as follows: @certificate_arn@ /@role_arn@ .
aecirrsCertificateS3ObjectKey :: Lens' AssociateEnclaveCertificateIAMRoleResponse (Maybe Text)
aecirrsCertificateS3ObjectKey = lens _aecirrsCertificateS3ObjectKey (\s a -> s {_aecirrsCertificateS3ObjectKey = a})

-- | The ID of the AWS KMS CMK used to encrypt the private key of the certificate.
aecirrsEncryptionKMSKeyId :: Lens' AssociateEnclaveCertificateIAMRoleResponse (Maybe Text)
aecirrsEncryptionKMSKeyId = lens _aecirrsEncryptionKMSKeyId (\s a -> s {_aecirrsEncryptionKMSKeyId = a})

-- | -- | The response status code.
aecirrsResponseStatus :: Lens' AssociateEnclaveCertificateIAMRoleResponse Int
aecirrsResponseStatus = lens _aecirrsResponseStatus (\s a -> s {_aecirrsResponseStatus = a})

instance NFData AssociateEnclaveCertificateIAMRoleResponse
