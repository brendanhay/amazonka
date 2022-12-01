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
-- Module      : Amazonka.Glacier.Types.Encryption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.Encryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glacier.Types.EncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the encryption used to store the job results
-- in Amazon S3.
--
-- /See:/ 'newEncryption' smart constructor.
data Encryption = Encryption'
  { -- | Optional. If the encryption type is @aws:kms@, you can use this value to
    -- specify the encryption context for the job results.
    kmsContext :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption algorithm used when storing job results in
    -- Amazon S3, for example @AES256@ or @aws:kms@.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The AWS KMS key ID to use for object encryption. All GET and PUT
    -- requests for an object protected by AWS KMS fail if not made by using
    -- Secure Sockets Layer (SSL) or Signature Version 4.
    kmsKeyId :: Prelude.Maybe Prelude.Text
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
-- 'kmsContext', 'encryption_kmsContext' - Optional. If the encryption type is @aws:kms@, you can use this value to
-- specify the encryption context for the job results.
--
-- 'encryptionType', 'encryption_encryptionType' - The server-side encryption algorithm used when storing job results in
-- Amazon S3, for example @AES256@ or @aws:kms@.
--
-- 'kmsKeyId', 'encryption_kmsKeyId' - The AWS KMS key ID to use for object encryption. All GET and PUT
-- requests for an object protected by AWS KMS fail if not made by using
-- Secure Sockets Layer (SSL) or Signature Version 4.
newEncryption ::
  Encryption
newEncryption =
  Encryption'
    { kmsContext = Prelude.Nothing,
      encryptionType = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | Optional. If the encryption type is @aws:kms@, you can use this value to
-- specify the encryption context for the job results.
encryption_kmsContext :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsContext = Lens.lens (\Encryption' {kmsContext} -> kmsContext) (\s@Encryption' {} a -> s {kmsContext = a} :: Encryption)

-- | The server-side encryption algorithm used when storing job results in
-- Amazon S3, for example @AES256@ or @aws:kms@.
encryption_encryptionType :: Lens.Lens' Encryption (Prelude.Maybe EncryptionType)
encryption_encryptionType = Lens.lens (\Encryption' {encryptionType} -> encryptionType) (\s@Encryption' {} a -> s {encryptionType = a} :: Encryption)

-- | The AWS KMS key ID to use for object encryption. All GET and PUT
-- requests for an object protected by AWS KMS fail if not made by using
-- Secure Sockets Layer (SSL) or Signature Version 4.
encryption_kmsKeyId :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsKeyId = Lens.lens (\Encryption' {kmsKeyId} -> kmsKeyId) (\s@Encryption' {} a -> s {kmsKeyId = a} :: Encryption)

instance Core.FromJSON Encryption where
  parseJSON =
    Core.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Prelude.<$> (x Core..:? "KMSContext")
            Prelude.<*> (x Core..:? "EncryptionType")
            Prelude.<*> (x Core..:? "KMSKeyId")
      )

instance Prelude.Hashable Encryption where
  hashWithSalt _salt Encryption' {..} =
    _salt `Prelude.hashWithSalt` kmsContext
      `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData Encryption where
  rnf Encryption' {..} =
    Prelude.rnf kmsContext
      `Prelude.seq` Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Core.ToJSON Encryption where
  toJSON Encryption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KMSContext" Core..=) Prelude.<$> kmsContext,
            ("EncryptionType" Core..=)
              Prelude.<$> encryptionType,
            ("KMSKeyId" Core..=) Prelude.<$> kmsKeyId
          ]
      )
