{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- When the IAM role is associated with the ACM certificate, places the certificate, certificate chain, and encrypted private key in an Amazon S3 bucket that only the associated IAM role can access. The private key of the certificate is encrypted with an AWS-managed KMS customer master (CMK) that has an attached attestation-based CMK policy.
-- To enable the IAM role to access the Amazon S3 object, you must grant it permission to call @s3:GetObject@ on the Amazon S3 bucket returned by the command. To enable the IAM role to access the AWS KMS CMK, you must grant it permission to call @kms:Decrypt@ on AWS KMS CMK returned by the command. For more information, see <https://docs.aws.amazon.com/enclaves/latest/user/nitro-enclave-refapp.html#add-policy Grant the role permission to access the certificate and encryption key> in the /AWS Nitro Enclaves User Guide/ .
module Network.AWS.EC2.AssociateEnclaveCertificateIAMRole
  ( -- * Creating a request
    AssociateEnclaveCertificateIAMRole (..),
    mkAssociateEnclaveCertificateIAMRole,

    -- ** Request lenses
    aecirCertificateARN,
    aecirDryRun,
    aecirRoleARN,

    -- * Destructuring the response
    AssociateEnclaveCertificateIAMRoleResponse (..),
    mkAssociateEnclaveCertificateIAMRoleResponse,

    -- ** Response lenses
    aecirrsCertificateS3BucketName,
    aecirrsCertificateS3ObjectKey,
    aecirrsEncryptionKMSKeyId,
    aecirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateEnclaveCertificateIAMRole' smart constructor.
data AssociateEnclaveCertificateIAMRole = AssociateEnclaveCertificateIAMRole'
  { certificateARN ::
      Lude.Maybe Lude.Text,
    dryRun ::
      Lude.Maybe Lude.Bool,
    roleARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateEnclaveCertificateIAMRole' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the ACM certificate with which to associate the IAM role.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'roleARN' - The ARN of the IAM role to associate with the ACM certificate. You can associate up to 16 IAM roles with an ACM certificate.
mkAssociateEnclaveCertificateIAMRole ::
  AssociateEnclaveCertificateIAMRole
mkAssociateEnclaveCertificateIAMRole =
  AssociateEnclaveCertificateIAMRole'
    { certificateARN =
        Lude.Nothing,
      dryRun = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The ARN of the ACM certificate with which to associate the IAM role.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirCertificateARN :: Lens.Lens' AssociateEnclaveCertificateIAMRole (Lude.Maybe Lude.Text)
aecirCertificateARN = Lens.lens (certificateARN :: AssociateEnclaveCertificateIAMRole -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: AssociateEnclaveCertificateIAMRole)
{-# DEPRECATED aecirCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirDryRun :: Lens.Lens' AssociateEnclaveCertificateIAMRole (Lude.Maybe Lude.Bool)
aecirDryRun = Lens.lens (dryRun :: AssociateEnclaveCertificateIAMRole -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateEnclaveCertificateIAMRole)
{-# DEPRECATED aecirDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ARN of the IAM role to associate with the ACM certificate. You can associate up to 16 IAM roles with an ACM certificate.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirRoleARN :: Lens.Lens' AssociateEnclaveCertificateIAMRole (Lude.Maybe Lude.Text)
aecirRoleARN = Lens.lens (roleARN :: AssociateEnclaveCertificateIAMRole -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: AssociateEnclaveCertificateIAMRole)
{-# DEPRECATED aecirRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AssociateEnclaveCertificateIAMRole where
  type
    Rs AssociateEnclaveCertificateIAMRole =
      AssociateEnclaveCertificateIAMRoleResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateEnclaveCertificateIAMRoleResponse'
            Lude.<$> (x Lude..@? "certificateS3BucketName")
            Lude.<*> (x Lude..@? "certificateS3ObjectKey")
            Lude.<*> (x Lude..@? "encryptionKmsKeyId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateEnclaveCertificateIAMRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateEnclaveCertificateIAMRole where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateEnclaveCertificateIAMRole where
  toQuery AssociateEnclaveCertificateIAMRole' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AssociateEnclaveCertificateIamRole" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CertificateArn" Lude.=: certificateARN,
        "DryRun" Lude.=: dryRun,
        "RoleArn" Lude.=: roleARN
      ]

-- | /See:/ 'mkAssociateEnclaveCertificateIAMRoleResponse' smart constructor.
data AssociateEnclaveCertificateIAMRoleResponse = AssociateEnclaveCertificateIAMRoleResponse'
  { certificateS3BucketName ::
      Lude.Maybe
        Lude.Text,
    certificateS3ObjectKey ::
      Lude.Maybe
        Lude.Text,
    encryptionKMSKeyId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateEnclaveCertificateIAMRoleResponse' with the minimum fields required to make a request.
--
-- * 'certificateS3BucketName' - The name of the Amazon S3 bucket to which the certificate was uploaded.
-- * 'certificateS3ObjectKey' - The Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored. The object key is formatted as follows: @certificate_arn@ /@role_arn@ .
-- * 'encryptionKMSKeyId' - The ID of the AWS KMS CMK used to encrypt the private key of the certificate.
-- * 'responseStatus' - The response status code.
mkAssociateEnclaveCertificateIAMRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateEnclaveCertificateIAMRoleResponse
mkAssociateEnclaveCertificateIAMRoleResponse pResponseStatus_ =
  AssociateEnclaveCertificateIAMRoleResponse'
    { certificateS3BucketName =
        Lude.Nothing,
      certificateS3ObjectKey = Lude.Nothing,
      encryptionKMSKeyId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the Amazon S3 bucket to which the certificate was uploaded.
--
-- /Note:/ Consider using 'certificateS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrsCertificateS3BucketName :: Lens.Lens' AssociateEnclaveCertificateIAMRoleResponse (Lude.Maybe Lude.Text)
aecirrsCertificateS3BucketName = Lens.lens (certificateS3BucketName :: AssociateEnclaveCertificateIAMRoleResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateS3BucketName = a} :: AssociateEnclaveCertificateIAMRoleResponse)
{-# DEPRECATED aecirrsCertificateS3BucketName "Use generic-lens or generic-optics with 'certificateS3BucketName' instead." #-}

-- | The Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored. The object key is formatted as follows: @certificate_arn@ /@role_arn@ .
--
-- /Note:/ Consider using 'certificateS3ObjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrsCertificateS3ObjectKey :: Lens.Lens' AssociateEnclaveCertificateIAMRoleResponse (Lude.Maybe Lude.Text)
aecirrsCertificateS3ObjectKey = Lens.lens (certificateS3ObjectKey :: AssociateEnclaveCertificateIAMRoleResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateS3ObjectKey = a} :: AssociateEnclaveCertificateIAMRoleResponse)
{-# DEPRECATED aecirrsCertificateS3ObjectKey "Use generic-lens or generic-optics with 'certificateS3ObjectKey' instead." #-}

-- | The ID of the AWS KMS CMK used to encrypt the private key of the certificate.
--
-- /Note:/ Consider using 'encryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrsEncryptionKMSKeyId :: Lens.Lens' AssociateEnclaveCertificateIAMRoleResponse (Lude.Maybe Lude.Text)
aecirrsEncryptionKMSKeyId = Lens.lens (encryptionKMSKeyId :: AssociateEnclaveCertificateIAMRoleResponse -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKMSKeyId = a} :: AssociateEnclaveCertificateIAMRoleResponse)
{-# DEPRECATED aecirrsEncryptionKMSKeyId "Use generic-lens or generic-optics with 'encryptionKMSKeyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrsResponseStatus :: Lens.Lens' AssociateEnclaveCertificateIAMRoleResponse Lude.Int
aecirrsResponseStatus = Lens.lens (responseStatus :: AssociateEnclaveCertificateIAMRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateEnclaveCertificateIAMRoleResponse)
{-# DEPRECATED aecirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
