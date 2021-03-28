{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateEnclaveCertificateIamRole
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
module Network.AWS.EC2.AssociateEnclaveCertificateIamRole
    (
    -- * Creating a request
      AssociateEnclaveCertificateIamRole (..)
    , mkAssociateEnclaveCertificateIamRole
    -- ** Request lenses
    , aecirCertificateArn
    , aecirDryRun
    , aecirRoleArn

    -- * Destructuring the response
    , AssociateEnclaveCertificateIamRoleResponse (..)
    , mkAssociateEnclaveCertificateIamRoleResponse
    -- ** Response lenses
    , aecirrrsCertificateS3BucketName
    , aecirrrsCertificateS3ObjectKey
    , aecirrrsEncryptionKmsKeyId
    , aecirrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateEnclaveCertificateIamRole' smart constructor.
data AssociateEnclaveCertificateIamRole = AssociateEnclaveCertificateIamRole'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The ARN of the ACM certificate with which to associate the IAM role.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the IAM role to associate with the ACM certificate. You can associate up to 16 IAM roles with an ACM certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateEnclaveCertificateIamRole' value with any optional fields omitted.
mkAssociateEnclaveCertificateIamRole
    :: AssociateEnclaveCertificateIamRole
mkAssociateEnclaveCertificateIamRole
  = AssociateEnclaveCertificateIamRole'{certificateArn =
                                          Core.Nothing,
                                        dryRun = Core.Nothing, roleArn = Core.Nothing}

-- | The ARN of the ACM certificate with which to associate the IAM role.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirCertificateArn :: Lens.Lens' AssociateEnclaveCertificateIamRole (Core.Maybe Types.CertificateArn)
aecirCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE aecirCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirDryRun :: Lens.Lens' AssociateEnclaveCertificateIamRole (Core.Maybe Core.Bool)
aecirDryRun = Lens.field @"dryRun"
{-# INLINEABLE aecirDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ARN of the IAM role to associate with the ACM certificate. You can associate up to 16 IAM roles with an ACM certificate.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirRoleArn :: Lens.Lens' AssociateEnclaveCertificateIamRole (Core.Maybe Types.RoleArn)
aecirRoleArn = Lens.field @"roleArn"
{-# INLINEABLE aecirRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery AssociateEnclaveCertificateIamRole where
        toQuery AssociateEnclaveCertificateIamRole{..}
          = Core.toQueryPair "Action"
              ("AssociateEnclaveCertificateIamRole" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CertificateArn")
                certificateArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "RoleArn") roleArn

instance Core.ToHeaders AssociateEnclaveCertificateIamRole where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssociateEnclaveCertificateIamRole where
        type Rs AssociateEnclaveCertificateIamRole =
             AssociateEnclaveCertificateIamRoleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 AssociateEnclaveCertificateIamRoleResponse' Core.<$>
                   (x Core..@? "certificateS3BucketName") Core.<*>
                     x Core..@? "certificateS3ObjectKey"
                     Core.<*> x Core..@? "encryptionKmsKeyId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateEnclaveCertificateIamRoleResponse' smart constructor.
data AssociateEnclaveCertificateIamRoleResponse = AssociateEnclaveCertificateIamRoleResponse'
  { certificateS3BucketName :: Core.Maybe Core.Text
    -- ^ The name of the Amazon S3 bucket to which the certificate was uploaded.
  , certificateS3ObjectKey :: Core.Maybe Core.Text
    -- ^ The Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored. The object key is formatted as follows: @certificate_arn@ /@role_arn@ .
  , encryptionKmsKeyId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS KMS CMK used to encrypt the private key of the certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateEnclaveCertificateIamRoleResponse' value with any optional fields omitted.
mkAssociateEnclaveCertificateIamRoleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateEnclaveCertificateIamRoleResponse
mkAssociateEnclaveCertificateIamRoleResponse responseStatus
  = AssociateEnclaveCertificateIamRoleResponse'{certificateS3BucketName
                                                  = Core.Nothing,
                                                certificateS3ObjectKey = Core.Nothing,
                                                encryptionKmsKeyId = Core.Nothing, responseStatus}

-- | The name of the Amazon S3 bucket to which the certificate was uploaded.
--
-- /Note:/ Consider using 'certificateS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrrsCertificateS3BucketName :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse (Core.Maybe Core.Text)
aecirrrsCertificateS3BucketName = Lens.field @"certificateS3BucketName"
{-# INLINEABLE aecirrrsCertificateS3BucketName #-}
{-# DEPRECATED certificateS3BucketName "Use generic-lens or generic-optics with 'certificateS3BucketName' instead"  #-}

-- | The Amazon S3 object key where the certificate, certificate chain, and encrypted private key bundle are stored. The object key is formatted as follows: @certificate_arn@ /@role_arn@ .
--
-- /Note:/ Consider using 'certificateS3ObjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrrsCertificateS3ObjectKey :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse (Core.Maybe Core.Text)
aecirrrsCertificateS3ObjectKey = Lens.field @"certificateS3ObjectKey"
{-# INLINEABLE aecirrrsCertificateS3ObjectKey #-}
{-# DEPRECATED certificateS3ObjectKey "Use generic-lens or generic-optics with 'certificateS3ObjectKey' instead"  #-}

-- | The ID of the AWS KMS CMK used to encrypt the private key of the certificate.
--
-- /Note:/ Consider using 'encryptionKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrrsEncryptionKmsKeyId :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse (Core.Maybe Core.Text)
aecirrrsEncryptionKmsKeyId = Lens.field @"encryptionKmsKeyId"
{-# INLINEABLE aecirrrsEncryptionKmsKeyId #-}
{-# DEPRECATED encryptionKmsKeyId "Use generic-lens or generic-optics with 'encryptionKmsKeyId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aecirrrsResponseStatus :: Lens.Lens' AssociateEnclaveCertificateIamRoleResponse Core.Int
aecirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aecirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
