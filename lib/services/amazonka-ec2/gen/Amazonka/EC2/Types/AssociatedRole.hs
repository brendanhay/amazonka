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
-- Module      : Amazonka.EC2.Types.AssociatedRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AssociatedRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the associated IAM roles.
--
-- /See:/ 'newAssociatedRole' smart constructor.
data AssociatedRole = AssociatedRole'
  { -- | The ARN of the associated IAM role.
    associatedRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket in which the Amazon S3 object is
    -- stored.
    certificateS3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The key of the Amazon S3 object ey where the certificate, certificate
    -- chain, and encrypted private key bundle is stored. The object key is
    -- formated as follows: @role_arn@\/@certificate_arn@.
    certificateS3ObjectKey :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS customer master key (CMK) used to encrypt the private
    -- key.
    encryptionKmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedRoleArn', 'associatedRole_associatedRoleArn' - The ARN of the associated IAM role.
--
-- 'certificateS3BucketName', 'associatedRole_certificateS3BucketName' - The name of the Amazon S3 bucket in which the Amazon S3 object is
-- stored.
--
-- 'certificateS3ObjectKey', 'associatedRole_certificateS3ObjectKey' - The key of the Amazon S3 object ey where the certificate, certificate
-- chain, and encrypted private key bundle is stored. The object key is
-- formated as follows: @role_arn@\/@certificate_arn@.
--
-- 'encryptionKmsKeyId', 'associatedRole_encryptionKmsKeyId' - The ID of the KMS customer master key (CMK) used to encrypt the private
-- key.
newAssociatedRole ::
  AssociatedRole
newAssociatedRole =
  AssociatedRole'
    { associatedRoleArn =
        Prelude.Nothing,
      certificateS3BucketName = Prelude.Nothing,
      certificateS3ObjectKey = Prelude.Nothing,
      encryptionKmsKeyId = Prelude.Nothing
    }

-- | The ARN of the associated IAM role.
associatedRole_associatedRoleArn :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_associatedRoleArn = Lens.lens (\AssociatedRole' {associatedRoleArn} -> associatedRoleArn) (\s@AssociatedRole' {} a -> s {associatedRoleArn = a} :: AssociatedRole)

-- | The name of the Amazon S3 bucket in which the Amazon S3 object is
-- stored.
associatedRole_certificateS3BucketName :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_certificateS3BucketName = Lens.lens (\AssociatedRole' {certificateS3BucketName} -> certificateS3BucketName) (\s@AssociatedRole' {} a -> s {certificateS3BucketName = a} :: AssociatedRole)

-- | The key of the Amazon S3 object ey where the certificate, certificate
-- chain, and encrypted private key bundle is stored. The object key is
-- formated as follows: @role_arn@\/@certificate_arn@.
associatedRole_certificateS3ObjectKey :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_certificateS3ObjectKey = Lens.lens (\AssociatedRole' {certificateS3ObjectKey} -> certificateS3ObjectKey) (\s@AssociatedRole' {} a -> s {certificateS3ObjectKey = a} :: AssociatedRole)

-- | The ID of the KMS customer master key (CMK) used to encrypt the private
-- key.
associatedRole_encryptionKmsKeyId :: Lens.Lens' AssociatedRole (Prelude.Maybe Prelude.Text)
associatedRole_encryptionKmsKeyId = Lens.lens (\AssociatedRole' {encryptionKmsKeyId} -> encryptionKmsKeyId) (\s@AssociatedRole' {} a -> s {encryptionKmsKeyId = a} :: AssociatedRole)

instance Data.FromXML AssociatedRole where
  parseXML x =
    AssociatedRole'
      Prelude.<$> (x Data..@? "associatedRoleArn")
      Prelude.<*> (x Data..@? "certificateS3BucketName")
      Prelude.<*> (x Data..@? "certificateS3ObjectKey")
      Prelude.<*> (x Data..@? "encryptionKmsKeyId")

instance Prelude.Hashable AssociatedRole where
  hashWithSalt _salt AssociatedRole' {..} =
    _salt
      `Prelude.hashWithSalt` associatedRoleArn
      `Prelude.hashWithSalt` certificateS3BucketName
      `Prelude.hashWithSalt` certificateS3ObjectKey
      `Prelude.hashWithSalt` encryptionKmsKeyId

instance Prelude.NFData AssociatedRole where
  rnf AssociatedRole' {..} =
    Prelude.rnf associatedRoleArn
      `Prelude.seq` Prelude.rnf certificateS3BucketName
      `Prelude.seq` Prelude.rnf certificateS3ObjectKey
      `Prelude.seq` Prelude.rnf encryptionKmsKeyId
