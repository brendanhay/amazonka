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
    arCertificateS3BucketName,
    arCertificateS3ObjectKey,
    arEncryptionKMSKeyId,
    arAssociatedRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the associated IAM roles.
--
-- /See:/ 'mkAssociatedRole' smart constructor.
data AssociatedRole = AssociatedRole'
  { certificateS3BucketName ::
      Lude.Maybe Lude.Text,
    certificateS3ObjectKey :: Lude.Maybe Lude.Text,
    encryptionKMSKeyId :: Lude.Maybe Lude.Text,
    associatedRoleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociatedRole' with the minimum fields required to make a request.
--
-- * 'associatedRoleARN' - The ARN of the associated IAM role.
-- * 'certificateS3BucketName' - The name of the Amazon S3 bucket in which the Amazon S3 object is stored.
-- * 'certificateS3ObjectKey' - The key of the Amazon S3 object ey where the certificate, certificate chain, and encrypted private key bundle is stored. The object key is formated as follows: @certificate_arn@ /@role_arn@ .
-- * 'encryptionKMSKeyId' - The ID of the KMS customer master key (CMK) used to encrypt the private key.
mkAssociatedRole ::
  AssociatedRole
mkAssociatedRole =
  AssociatedRole'
    { certificateS3BucketName = Lude.Nothing,
      certificateS3ObjectKey = Lude.Nothing,
      encryptionKMSKeyId = Lude.Nothing,
      associatedRoleARN = Lude.Nothing
    }

-- | The name of the Amazon S3 bucket in which the Amazon S3 object is stored.
--
-- /Note:/ Consider using 'certificateS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCertificateS3BucketName :: Lens.Lens' AssociatedRole (Lude.Maybe Lude.Text)
arCertificateS3BucketName = Lens.lens (certificateS3BucketName :: AssociatedRole -> Lude.Maybe Lude.Text) (\s a -> s {certificateS3BucketName = a} :: AssociatedRole)
{-# DEPRECATED arCertificateS3BucketName "Use generic-lens or generic-optics with 'certificateS3BucketName' instead." #-}

-- | The key of the Amazon S3 object ey where the certificate, certificate chain, and encrypted private key bundle is stored. The object key is formated as follows: @certificate_arn@ /@role_arn@ .
--
-- /Note:/ Consider using 'certificateS3ObjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCertificateS3ObjectKey :: Lens.Lens' AssociatedRole (Lude.Maybe Lude.Text)
arCertificateS3ObjectKey = Lens.lens (certificateS3ObjectKey :: AssociatedRole -> Lude.Maybe Lude.Text) (\s a -> s {certificateS3ObjectKey = a} :: AssociatedRole)
{-# DEPRECATED arCertificateS3ObjectKey "Use generic-lens or generic-optics with 'certificateS3ObjectKey' instead." #-}

-- | The ID of the KMS customer master key (CMK) used to encrypt the private key.
--
-- /Note:/ Consider using 'encryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arEncryptionKMSKeyId :: Lens.Lens' AssociatedRole (Lude.Maybe Lude.Text)
arEncryptionKMSKeyId = Lens.lens (encryptionKMSKeyId :: AssociatedRole -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKMSKeyId = a} :: AssociatedRole)
{-# DEPRECATED arEncryptionKMSKeyId "Use generic-lens or generic-optics with 'encryptionKMSKeyId' instead." #-}

-- | The ARN of the associated IAM role.
--
-- /Note:/ Consider using 'associatedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAssociatedRoleARN :: Lens.Lens' AssociatedRole (Lude.Maybe Lude.Text)
arAssociatedRoleARN = Lens.lens (associatedRoleARN :: AssociatedRole -> Lude.Maybe Lude.Text) (\s a -> s {associatedRoleARN = a} :: AssociatedRole)
{-# DEPRECATED arAssociatedRoleARN "Use generic-lens or generic-optics with 'associatedRoleARN' instead." #-}

instance Lude.FromXML AssociatedRole where
  parseXML x =
    AssociatedRole'
      Lude.<$> (x Lude..@? "certificateS3BucketName")
      Lude.<*> (x Lude..@? "certificateS3ObjectKey")
      Lude.<*> (x Lude..@? "encryptionKmsKeyId")
      Lude.<*> (x Lude..@? "associatedRoleArn")
