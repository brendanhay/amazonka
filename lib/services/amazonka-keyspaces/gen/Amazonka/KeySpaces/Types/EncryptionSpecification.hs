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
-- Module      : Amazonka.KeySpaces.Types.EncryptionSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.EncryptionSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.EncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Amazon Keyspaces encrypts and decrypts the table data at rest
-- transparently and integrates with Key Management Service for storing and
-- managing the encryption key. You can choose one of the following KMS
-- keys (KMS keys):
--
-- • Amazon Web Services owned key - This is the default encryption type.
-- The key is owned by Amazon Keyspaces (no additional charge).
--
-- • Customer managed key - This key is stored in your account and is
-- created, owned, and managed by you. You have full control over the
-- customer managed key (KMS charges apply).
--
-- For more information about encryption at rest in Amazon Keyspaces, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- For more information about KMS, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html KMS management service concepts>
-- in the /Key Management Service Developer Guide/.
--
-- /See:/ 'newEncryptionSpecification' smart constructor.
data EncryptionSpecification = EncryptionSpecification'
  { -- | The Amazon Resource Name (ARN) of the customer managed KMS key, for
    -- example @kms_key_identifier:ARN@.
    kmsKeyIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The encryption option specified for the table. You can choose one of the
    -- following KMS keys (KMS keys):
    --
    -- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
    --
    -- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
    -- and is created, owned, and managed by you. This option requires the
    -- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
    -- as input.
    --
    -- The default is @type:AWS_OWNED_KMS_KEY@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
    -- in the /Amazon Keyspaces Developer Guide/.
    type' :: EncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyIdentifier', 'encryptionSpecification_kmsKeyIdentifier' - The Amazon Resource Name (ARN) of the customer managed KMS key, for
-- example @kms_key_identifier:ARN@.
--
-- 'type'', 'encryptionSpecification_type' - The encryption option specified for the table. You can choose one of the
-- following KMS keys (KMS keys):
--
-- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
--
-- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
-- and is created, owned, and managed by you. This option requires the
-- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
-- as input.
--
-- The default is @type:AWS_OWNED_KMS_KEY@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
-- in the /Amazon Keyspaces Developer Guide/.
newEncryptionSpecification ::
  -- | 'type''
  EncryptionType ->
  EncryptionSpecification
newEncryptionSpecification pType_ =
  EncryptionSpecification'
    { kmsKeyIdentifier =
        Prelude.Nothing,
      type' = pType_
    }

-- | The Amazon Resource Name (ARN) of the customer managed KMS key, for
-- example @kms_key_identifier:ARN@.
encryptionSpecification_kmsKeyIdentifier :: Lens.Lens' EncryptionSpecification (Prelude.Maybe Prelude.Text)
encryptionSpecification_kmsKeyIdentifier = Lens.lens (\EncryptionSpecification' {kmsKeyIdentifier} -> kmsKeyIdentifier) (\s@EncryptionSpecification' {} a -> s {kmsKeyIdentifier = a} :: EncryptionSpecification)

-- | The encryption option specified for the table. You can choose one of the
-- following KMS keys (KMS keys):
--
-- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
--
-- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
-- and is created, owned, and managed by you. This option requires the
-- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
-- as input.
--
-- The default is @type:AWS_OWNED_KMS_KEY@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
-- in the /Amazon Keyspaces Developer Guide/.
encryptionSpecification_type :: Lens.Lens' EncryptionSpecification EncryptionType
encryptionSpecification_type = Lens.lens (\EncryptionSpecification' {type'} -> type') (\s@EncryptionSpecification' {} a -> s {type' = a} :: EncryptionSpecification)

instance Data.FromJSON EncryptionSpecification where
  parseJSON =
    Data.withObject
      "EncryptionSpecification"
      ( \x ->
          EncryptionSpecification'
            Prelude.<$> (x Data..:? "kmsKeyIdentifier")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable EncryptionSpecification where
  hashWithSalt _salt EncryptionSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyIdentifier
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EncryptionSpecification where
  rnf EncryptionSpecification' {..} =
    Prelude.rnf kmsKeyIdentifier
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON EncryptionSpecification where
  toJSON EncryptionSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyIdentifier" Data..=)
              Prelude.<$> kmsKeyIdentifier,
            Prelude.Just ("type" Data..= type')
          ]
      )
