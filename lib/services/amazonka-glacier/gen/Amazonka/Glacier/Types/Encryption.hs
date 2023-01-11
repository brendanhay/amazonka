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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.Encryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.EncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the encryption used to store the job results
-- in Amazon S3.
--
-- /See:/ 'newEncryption' smart constructor.
data Encryption = Encryption'
  { -- | The server-side encryption algorithm used when storing job results in
    -- Amazon S3, for example @AES256@ or @aws:kms@.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | Optional. If the encryption type is @aws:kms@, you can use this value to
    -- specify the encryption context for the job results.
    kmsContext :: Prelude.Maybe Prelude.Text,
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
-- 'encryptionType', 'encryption_encryptionType' - The server-side encryption algorithm used when storing job results in
-- Amazon S3, for example @AES256@ or @aws:kms@.
--
-- 'kmsContext', 'encryption_kmsContext' - Optional. If the encryption type is @aws:kms@, you can use this value to
-- specify the encryption context for the job results.
--
-- 'kmsKeyId', 'encryption_kmsKeyId' - The AWS KMS key ID to use for object encryption. All GET and PUT
-- requests for an object protected by AWS KMS fail if not made by using
-- Secure Sockets Layer (SSL) or Signature Version 4.
newEncryption ::
  Encryption
newEncryption =
  Encryption'
    { encryptionType = Prelude.Nothing,
      kmsContext = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | The server-side encryption algorithm used when storing job results in
-- Amazon S3, for example @AES256@ or @aws:kms@.
encryption_encryptionType :: Lens.Lens' Encryption (Prelude.Maybe EncryptionType)
encryption_encryptionType = Lens.lens (\Encryption' {encryptionType} -> encryptionType) (\s@Encryption' {} a -> s {encryptionType = a} :: Encryption)

-- | Optional. If the encryption type is @aws:kms@, you can use this value to
-- specify the encryption context for the job results.
encryption_kmsContext :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsContext = Lens.lens (\Encryption' {kmsContext} -> kmsContext) (\s@Encryption' {} a -> s {kmsContext = a} :: Encryption)

-- | The AWS KMS key ID to use for object encryption. All GET and PUT
-- requests for an object protected by AWS KMS fail if not made by using
-- Secure Sockets Layer (SSL) or Signature Version 4.
encryption_kmsKeyId :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsKeyId = Lens.lens (\Encryption' {kmsKeyId} -> kmsKeyId) (\s@Encryption' {} a -> s {kmsKeyId = a} :: Encryption)

instance Data.FromJSON Encryption where
  parseJSON =
    Data.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Prelude.<$> (x Data..:? "EncryptionType")
            Prelude.<*> (x Data..:? "KMSContext")
            Prelude.<*> (x Data..:? "KMSKeyId")
      )

instance Prelude.Hashable Encryption where
  hashWithSalt _salt Encryption' {..} =
    _salt `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` kmsContext
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData Encryption where
  rnf Encryption' {..} =
    Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf kmsContext
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToJSON Encryption where
  toJSON Encryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionType" Data..=)
              Prelude.<$> encryptionType,
            ("KMSContext" Data..=) Prelude.<$> kmsContext,
            ("KMSKeyId" Data..=) Prelude.<$> kmsKeyId
          ]
      )
