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
-- Module      : Network.AWS.Glacier.Types.Encryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Encryption where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.EncryptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the encryption used to store the job results
-- in Amazon S3.
--
-- /See:/ 'newEncryption' smart constructor.
data Encryption = Encryption'
  { -- | The server-side encryption algorithm used when storing job results in
    -- Amazon S3, for example @AES256@ or @aws:kms@.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The AWS KMS key ID to use for object encryption. All GET and PUT
    -- requests for an object protected by AWS KMS fail if not made by using
    -- Secure Sockets Layer (SSL) or Signature Version 4.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Optional. If the encryption type is @aws:kms@, you can use this value to
    -- specify the encryption context for the job results.
    kmsContext :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Encryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'encryption_encryptionType' - The server-side encryption algorithm used when storing job results in
-- Amazon S3, for example @AES256@ or @aws:kms@.
--
-- 'kmsKeyId', 'encryption_kmsKeyId' - The AWS KMS key ID to use for object encryption. All GET and PUT
-- requests for an object protected by AWS KMS fail if not made by using
-- Secure Sockets Layer (SSL) or Signature Version 4.
--
-- 'kmsContext', 'encryption_kmsContext' - Optional. If the encryption type is @aws:kms@, you can use this value to
-- specify the encryption context for the job results.
newEncryption ::
  Encryption
newEncryption =
  Encryption'
    { encryptionType = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      kmsContext = Prelude.Nothing
    }

-- | The server-side encryption algorithm used when storing job results in
-- Amazon S3, for example @AES256@ or @aws:kms@.
encryption_encryptionType :: Lens.Lens' Encryption (Prelude.Maybe EncryptionType)
encryption_encryptionType = Lens.lens (\Encryption' {encryptionType} -> encryptionType) (\s@Encryption' {} a -> s {encryptionType = a} :: Encryption)

-- | The AWS KMS key ID to use for object encryption. All GET and PUT
-- requests for an object protected by AWS KMS fail if not made by using
-- Secure Sockets Layer (SSL) or Signature Version 4.
encryption_kmsKeyId :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsKeyId = Lens.lens (\Encryption' {kmsKeyId} -> kmsKeyId) (\s@Encryption' {} a -> s {kmsKeyId = a} :: Encryption)

-- | Optional. If the encryption type is @aws:kms@, you can use this value to
-- specify the encryption context for the job results.
encryption_kmsContext :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsContext = Lens.lens (\Encryption' {kmsContext} -> kmsContext) (\s@Encryption' {} a -> s {kmsContext = a} :: Encryption)

instance Core.FromJSON Encryption where
  parseJSON =
    Core.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Prelude.<$> (x Core..:? "EncryptionType")
            Prelude.<*> (x Core..:? "KMSKeyId")
            Prelude.<*> (x Core..:? "KMSContext")
      )

instance Prelude.Hashable Encryption

instance Prelude.NFData Encryption

instance Core.ToJSON Encryption where
  toJSON Encryption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EncryptionType" Core..=)
              Prelude.<$> encryptionType,
            ("KMSKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("KMSContext" Core..=) Prelude.<$> kmsContext
          ]
      )
