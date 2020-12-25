{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociatedRole
  ( AssociatedRole (..),

    -- * Smart constructor
    mkAssociatedRole,

    -- * Lenses
    arAssociatedRoleArn,
    arCertificateS3BucketName,
    arCertificateS3ObjectKey,
    arEncryptionKmsKeyId,
  )
where

import qualified Network.AWS.EC2.Types.AssociatedRoleArn as Types
import qualified Network.AWS.EC2.Types.CertificateS3BucketName as Types
import qualified Network.AWS.EC2.Types.CertificateS3ObjectKey as Types
import qualified Network.AWS.EC2.Types.EncryptionKmsKeyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the associated IAM roles.
--
-- /See:/ 'mkAssociatedRole' smart constructor.
data AssociatedRole = AssociatedRole'
  { -- | The ARN of the associated IAM role.
    associatedRoleArn :: Core.Maybe Types.AssociatedRoleArn,
    -- | The name of the Amazon S3 bucket in which the Amazon S3 object is stored.
    certificateS3BucketName :: Core.Maybe Types.CertificateS3BucketName,
    -- | The key of the Amazon S3 object ey where the certificate, certificate chain, and encrypted private key bundle is stored. The object key is formated as follows: @certificate_arn@ /@role_arn@ .
    certificateS3ObjectKey :: Core.Maybe Types.CertificateS3ObjectKey,
    -- | The ID of the KMS customer master key (CMK) used to encrypt the private key.
    encryptionKmsKeyId :: Core.Maybe Types.EncryptionKmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociatedRole' value with any optional fields omitted.
mkAssociatedRole ::
  AssociatedRole
mkAssociatedRole =
  AssociatedRole'
    { associatedRoleArn = Core.Nothing,
      certificateS3BucketName = Core.Nothing,
      certificateS3ObjectKey = Core.Nothing,
      encryptionKmsKeyId = Core.Nothing
    }

-- | The ARN of the associated IAM role.
--
-- /Note:/ Consider using 'associatedRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAssociatedRoleArn :: Lens.Lens' AssociatedRole (Core.Maybe Types.AssociatedRoleArn)
arAssociatedRoleArn = Lens.field @"associatedRoleArn"
{-# DEPRECATED arAssociatedRoleArn "Use generic-lens or generic-optics with 'associatedRoleArn' instead." #-}

-- | The name of the Amazon S3 bucket in which the Amazon S3 object is stored.
--
-- /Note:/ Consider using 'certificateS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCertificateS3BucketName :: Lens.Lens' AssociatedRole (Core.Maybe Types.CertificateS3BucketName)
arCertificateS3BucketName = Lens.field @"certificateS3BucketName"
{-# DEPRECATED arCertificateS3BucketName "Use generic-lens or generic-optics with 'certificateS3BucketName' instead." #-}

-- | The key of the Amazon S3 object ey where the certificate, certificate chain, and encrypted private key bundle is stored. The object key is formated as follows: @certificate_arn@ /@role_arn@ .
--
-- /Note:/ Consider using 'certificateS3ObjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCertificateS3ObjectKey :: Lens.Lens' AssociatedRole (Core.Maybe Types.CertificateS3ObjectKey)
arCertificateS3ObjectKey = Lens.field @"certificateS3ObjectKey"
{-# DEPRECATED arCertificateS3ObjectKey "Use generic-lens or generic-optics with 'certificateS3ObjectKey' instead." #-}

-- | The ID of the KMS customer master key (CMK) used to encrypt the private key.
--
-- /Note:/ Consider using 'encryptionKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arEncryptionKmsKeyId :: Lens.Lens' AssociatedRole (Core.Maybe Types.EncryptionKmsKeyId)
arEncryptionKmsKeyId = Lens.field @"encryptionKmsKeyId"
{-# DEPRECATED arEncryptionKmsKeyId "Use generic-lens or generic-optics with 'encryptionKmsKeyId' instead." #-}

instance Core.FromXML AssociatedRole where
  parseXML x =
    AssociatedRole'
      Core.<$> (x Core..@? "associatedRoleArn")
      Core.<*> (x Core..@? "certificateS3BucketName")
      Core.<*> (x Core..@? "certificateS3ObjectKey")
      Core.<*> (x Core..@? "encryptionKmsKeyId")
