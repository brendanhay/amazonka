{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociatedRole where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the associated IAM roles.
--
-- /See:/ 'newAssociatedRole' smart constructor.
data AssociatedRole = AssociatedRole'
  { -- | The key of the Amazon S3 object ey where the certificate, certificate
    -- chain, and encrypted private key bundle is stored. The object key is
    -- formated as follows: @role_arn@\/@certificate_arn@.
    certificateS3ObjectKey :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS customer master key (CMK) used to encrypt the private
    -- key.
    encryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket in which the Amazon S3 object is
    -- stored.
    certificateS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the associated IAM role.
    associatedRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociatedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateS3ObjectKey', 'associatedRole_certificateS3ObjectKey' - The key of the Amazon S3 object ey where the certificate, certificate
-- chain, and encrypted private key bundle is stored. The object key is
-- formated as follows: @role_arn@\/@certificate_arn@.
--
-- 'encryptionKmsKeyId', 'associatedRole_encryptionKmsKeyId' - The ID of the KMS customer master key (CMK) used to encrypt the private
-- key.
--
-- 'certificateS3BucketName', 'associatedRole_certificateS3BucketName' - The name of the Amazon S3 bucket in which the Amazon S3 object is
-- stored.
--
-- 'associatedRoleArn', 'associatedRole_associatedRoleArn' - The ARN of the associated IAM role.
newAssociatedRole ::
  AssociatedRole
newAssociatedRole =
  AssociatedRole'
    { certificateS3ObjectKey =
        Prelude.Nothing,
      encryptionKmsKeyId = Prelude.Nothing,
      certificateS3BucketName = Prelude.Nothing,
      associatedRoleArn = Prelude.Nothing
    }

-- | The key of the Amazon S3 object ey where the certificate, certificate
-- chain, and encrypted private key bundle is stored. The object key is
-- formated as follows: @role_arn@\/@certificate_arn@.
associatedRole_certificateS3ObjectKey :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_certificateS3ObjectKey = Lens.lens (\AssociatedRole' {certificateS3ObjectKey} -> certificateS3ObjectKey) (\s@AssociatedRole' {} a -> s {certificateS3ObjectKey = a} :: AssociatedRole)

-- | The ID of the KMS customer master key (CMK) used to encrypt the private
-- key.
associatedRole_encryptionKmsKeyId :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_encryptionKmsKeyId = Lens.lens (\AssociatedRole' {encryptionKmsKeyId} -> encryptionKmsKeyId) (\s@AssociatedRole' {} a -> s {encryptionKmsKeyId = a} :: AssociatedRole)

-- | The name of the Amazon S3 bucket in which the Amazon S3 object is
-- stored.
associatedRole_certificateS3BucketName :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_certificateS3BucketName = Lens.lens (\AssociatedRole' {certificateS3BucketName} -> certificateS3BucketName) (\s@AssociatedRole' {} a -> s {certificateS3BucketName = a} :: AssociatedRole)

-- | The ARN of the associated IAM role.
associatedRole_associatedRoleArn :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_associatedRoleArn = Lens.lens (\AssociatedRole' {associatedRoleArn} -> associatedRoleArn) (\s@AssociatedRole' {} a -> s {associatedRoleArn = a} :: AssociatedRole)

instance Prelude.FromXML AssociatedRole where
  parseXML x =
    AssociatedRole'
      Prelude.<$> (x Prelude..@? "certificateS3ObjectKey")
      Prelude.<*> (x Prelude..@? "encryptionKmsKeyId")
      Prelude.<*> (x Prelude..@? "certificateS3BucketName")
      Prelude.<*> (x Prelude..@? "associatedRoleArn")

instance Prelude.Hashable AssociatedRole

instance Prelude.NFData AssociatedRole
