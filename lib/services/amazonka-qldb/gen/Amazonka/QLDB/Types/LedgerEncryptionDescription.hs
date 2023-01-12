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
-- Module      : Amazonka.QLDB.Types.LedgerEncryptionDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.LedgerEncryptionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types.EncryptionStatus

-- | Information about the encryption of data at rest in an Amazon QLDB
-- ledger. This includes the current status, the key in Key Management
-- Service (KMS), and when the key became inaccessible (in the case of an
-- error).
--
-- For more information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/encryption-at-rest.html Encryption at rest>
-- in the /Amazon QLDB Developer Guide/.
--
-- /See:/ 'newLedgerEncryptionDescription' smart constructor.
data LedgerEncryptionDescription = LedgerEncryptionDescription'
  { -- | The date and time, in epoch time format, when the KMS key first became
    -- inaccessible, in the case of an error. (Epoch time format is the number
    -- of seconds that have elapsed since 12:00:00 AM January 1, 1970 UTC.)
    --
    -- This parameter is undefined if the KMS key is accessible.
    inaccessibleKmsKeyDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the customer managed KMS key that the
    -- ledger uses for encryption at rest. If this parameter is undefined, the
    -- ledger uses an Amazon Web Services owned KMS key for encryption.
    kmsKeyArn :: Prelude.Text,
    -- | The current state of encryption at rest for the ledger. This can be one
    -- of the following values:
    --
    -- -   @ENABLED@: Encryption is fully enabled using the specified key.
    --
    -- -   @UPDATING@: The ledger is actively processing the specified key
    --     change.
    --
    --     Key changes in QLDB are asynchronous. The ledger is fully accessible
    --     without any performance impact while the key change is being
    --     processed. The amount of time it takes to update a key varies
    --     depending on the ledger size.
    --
    -- -   @KMS_KEY_INACCESSIBLE@: The specified customer managed KMS key is
    --     not accessible, and the ledger is impaired. Either the key was
    --     disabled or deleted, or the grants on the key were revoked. When a
    --     ledger is impaired, it is not accessible and does not accept any
    --     read or write requests.
    --
    --     An impaired ledger automatically returns to an active state after
    --     you restore the grants on the key, or re-enable the key that was
    --     disabled. However, deleting a customer managed KMS key is
    --     irreversible. After a key is deleted, you can no longer access the
    --     ledgers that are protected with that key, and the data becomes
    --     unrecoverable permanently.
    encryptionStatus :: EncryptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LedgerEncryptionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inaccessibleKmsKeyDateTime', 'ledgerEncryptionDescription_inaccessibleKmsKeyDateTime' - The date and time, in epoch time format, when the KMS key first became
-- inaccessible, in the case of an error. (Epoch time format is the number
-- of seconds that have elapsed since 12:00:00 AM January 1, 1970 UTC.)
--
-- This parameter is undefined if the KMS key is accessible.
--
-- 'kmsKeyArn', 'ledgerEncryptionDescription_kmsKeyArn' - The Amazon Resource Name (ARN) of the customer managed KMS key that the
-- ledger uses for encryption at rest. If this parameter is undefined, the
-- ledger uses an Amazon Web Services owned KMS key for encryption.
--
-- 'encryptionStatus', 'ledgerEncryptionDescription_encryptionStatus' - The current state of encryption at rest for the ledger. This can be one
-- of the following values:
--
-- -   @ENABLED@: Encryption is fully enabled using the specified key.
--
-- -   @UPDATING@: The ledger is actively processing the specified key
--     change.
--
--     Key changes in QLDB are asynchronous. The ledger is fully accessible
--     without any performance impact while the key change is being
--     processed. The amount of time it takes to update a key varies
--     depending on the ledger size.
--
-- -   @KMS_KEY_INACCESSIBLE@: The specified customer managed KMS key is
--     not accessible, and the ledger is impaired. Either the key was
--     disabled or deleted, or the grants on the key were revoked. When a
--     ledger is impaired, it is not accessible and does not accept any
--     read or write requests.
--
--     An impaired ledger automatically returns to an active state after
--     you restore the grants on the key, or re-enable the key that was
--     disabled. However, deleting a customer managed KMS key is
--     irreversible. After a key is deleted, you can no longer access the
--     ledgers that are protected with that key, and the data becomes
--     unrecoverable permanently.
newLedgerEncryptionDescription ::
  -- | 'kmsKeyArn'
  Prelude.Text ->
  -- | 'encryptionStatus'
  EncryptionStatus ->
  LedgerEncryptionDescription
newLedgerEncryptionDescription
  pKmsKeyArn_
  pEncryptionStatus_ =
    LedgerEncryptionDescription'
      { inaccessibleKmsKeyDateTime =
          Prelude.Nothing,
        kmsKeyArn = pKmsKeyArn_,
        encryptionStatus = pEncryptionStatus_
      }

-- | The date and time, in epoch time format, when the KMS key first became
-- inaccessible, in the case of an error. (Epoch time format is the number
-- of seconds that have elapsed since 12:00:00 AM January 1, 1970 UTC.)
--
-- This parameter is undefined if the KMS key is accessible.
ledgerEncryptionDescription_inaccessibleKmsKeyDateTime :: Lens.Lens' LedgerEncryptionDescription (Prelude.Maybe Prelude.UTCTime)
ledgerEncryptionDescription_inaccessibleKmsKeyDateTime = Lens.lens (\LedgerEncryptionDescription' {inaccessibleKmsKeyDateTime} -> inaccessibleKmsKeyDateTime) (\s@LedgerEncryptionDescription' {} a -> s {inaccessibleKmsKeyDateTime = a} :: LedgerEncryptionDescription) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the customer managed KMS key that the
-- ledger uses for encryption at rest. If this parameter is undefined, the
-- ledger uses an Amazon Web Services owned KMS key for encryption.
ledgerEncryptionDescription_kmsKeyArn :: Lens.Lens' LedgerEncryptionDescription Prelude.Text
ledgerEncryptionDescription_kmsKeyArn = Lens.lens (\LedgerEncryptionDescription' {kmsKeyArn} -> kmsKeyArn) (\s@LedgerEncryptionDescription' {} a -> s {kmsKeyArn = a} :: LedgerEncryptionDescription)

-- | The current state of encryption at rest for the ledger. This can be one
-- of the following values:
--
-- -   @ENABLED@: Encryption is fully enabled using the specified key.
--
-- -   @UPDATING@: The ledger is actively processing the specified key
--     change.
--
--     Key changes in QLDB are asynchronous. The ledger is fully accessible
--     without any performance impact while the key change is being
--     processed. The amount of time it takes to update a key varies
--     depending on the ledger size.
--
-- -   @KMS_KEY_INACCESSIBLE@: The specified customer managed KMS key is
--     not accessible, and the ledger is impaired. Either the key was
--     disabled or deleted, or the grants on the key were revoked. When a
--     ledger is impaired, it is not accessible and does not accept any
--     read or write requests.
--
--     An impaired ledger automatically returns to an active state after
--     you restore the grants on the key, or re-enable the key that was
--     disabled. However, deleting a customer managed KMS key is
--     irreversible. After a key is deleted, you can no longer access the
--     ledgers that are protected with that key, and the data becomes
--     unrecoverable permanently.
ledgerEncryptionDescription_encryptionStatus :: Lens.Lens' LedgerEncryptionDescription EncryptionStatus
ledgerEncryptionDescription_encryptionStatus = Lens.lens (\LedgerEncryptionDescription' {encryptionStatus} -> encryptionStatus) (\s@LedgerEncryptionDescription' {} a -> s {encryptionStatus = a} :: LedgerEncryptionDescription)

instance Data.FromJSON LedgerEncryptionDescription where
  parseJSON =
    Data.withObject
      "LedgerEncryptionDescription"
      ( \x ->
          LedgerEncryptionDescription'
            Prelude.<$> (x Data..:? "InaccessibleKmsKeyDateTime")
            Prelude.<*> (x Data..: "KmsKeyArn")
            Prelude.<*> (x Data..: "EncryptionStatus")
      )

instance Prelude.Hashable LedgerEncryptionDescription where
  hashWithSalt _salt LedgerEncryptionDescription' {..} =
    _salt
      `Prelude.hashWithSalt` inaccessibleKmsKeyDateTime
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` encryptionStatus

instance Prelude.NFData LedgerEncryptionDescription where
  rnf LedgerEncryptionDescription' {..} =
    Prelude.rnf inaccessibleKmsKeyDateTime
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf encryptionStatus
