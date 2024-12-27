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
-- Module      : Amazonka.S3.Types.Encryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Encryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ServerSideEncryption

-- | Contains the type of server-side encryption used.
--
-- /See:/ 'newEncryption' smart constructor.
data Encryption = Encryption'
  { -- | If the encryption type is @aws:kms@, this optional value can be used to
    -- specify the encryption context for the restore results.
    kmsContext :: Prelude.Maybe Prelude.Text,
    -- | If the encryption type is @aws:kms@, this optional value specifies the
    -- ID of the symmetric customer managed key to use for encryption of job
    -- results. Amazon S3 only supports symmetric keys. For more information,
    -- see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
    -- in the /Amazon Web Services Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The server-side encryption algorithm used when storing job results in
    -- Amazon S3 (for example, AES256, aws:kms).
    encryptionType :: ServerSideEncryption
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Encryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsContext', 'encryption_kmsContext' - If the encryption type is @aws:kms@, this optional value can be used to
-- specify the encryption context for the restore results.
--
-- 'kmsKeyId', 'encryption_kmsKeyId' - If the encryption type is @aws:kms@, this optional value specifies the
-- ID of the symmetric customer managed key to use for encryption of job
-- results. Amazon S3 only supports symmetric keys. For more information,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
--
-- 'encryptionType', 'encryption_encryptionType' - The server-side encryption algorithm used when storing job results in
-- Amazon S3 (for example, AES256, aws:kms).
newEncryption ::
  -- | 'encryptionType'
  ServerSideEncryption ->
  Encryption
newEncryption pEncryptionType_ =
  Encryption'
    { kmsContext = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      encryptionType = pEncryptionType_
    }

-- | If the encryption type is @aws:kms@, this optional value can be used to
-- specify the encryption context for the restore results.
encryption_kmsContext :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsContext = Lens.lens (\Encryption' {kmsContext} -> kmsContext) (\s@Encryption' {} a -> s {kmsContext = a} :: Encryption)

-- | If the encryption type is @aws:kms@, this optional value specifies the
-- ID of the symmetric customer managed key to use for encryption of job
-- results. Amazon S3 only supports symmetric keys. For more information,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
encryption_kmsKeyId :: Lens.Lens' Encryption (Prelude.Maybe Prelude.Text)
encryption_kmsKeyId = Lens.lens (\Encryption' {kmsKeyId} -> kmsKeyId) (\s@Encryption' {} a -> s {kmsKeyId = a} :: Encryption) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption algorithm used when storing job results in
-- Amazon S3 (for example, AES256, aws:kms).
encryption_encryptionType :: Lens.Lens' Encryption ServerSideEncryption
encryption_encryptionType = Lens.lens (\Encryption' {encryptionType} -> encryptionType) (\s@Encryption' {} a -> s {encryptionType = a} :: Encryption)

instance Prelude.Hashable Encryption where
  hashWithSalt _salt Encryption' {..} =
    _salt
      `Prelude.hashWithSalt` kmsContext
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` encryptionType

instance Prelude.NFData Encryption where
  rnf Encryption' {..} =
    Prelude.rnf kmsContext `Prelude.seq`
      Prelude.rnf kmsKeyId `Prelude.seq`
        Prelude.rnf encryptionType

instance Data.ToXML Encryption where
  toXML Encryption' {..} =
    Prelude.mconcat
      [ "KMSContext" Data.@= kmsContext,
        "KMSKeyId" Data.@= kmsKeyId,
        "EncryptionType" Data.@= encryptionType
      ]
