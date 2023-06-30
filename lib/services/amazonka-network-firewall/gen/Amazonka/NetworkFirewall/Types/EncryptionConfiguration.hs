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
-- Module      : Amazonka.NetworkFirewall.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.EncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.EncryptionType
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains optional Amazon Web Services Key Management
-- Service (KMS) encryption settings for your Network Firewall resources.
-- Your data is encrypted by default with an Amazon Web Services owned key
-- that Amazon Web Services owns and manages for you. You can use either
-- the Amazon Web Services owned key, or provide your own customer managed
-- key. To learn more about KMS encryption of your Network Firewall
-- resources, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-encryption-at-rest.html Encryption at rest with Amazon Web Services Key Managment Service>
-- in the /Network Firewall Developer Guide/.
--
-- /See:/ 'newEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The ID of the Amazon Web Services Key Management Service (KMS) customer
    -- managed key. You can use any of the key identifiers that KMS supports,
    -- unless you\'re using a key that\'s managed by another account. If
    -- you\'re using a key managed by another account, then specify the key
    -- ARN. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key ID>
    -- in the /Amazon Web Services KMS Developer Guide/.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Web Services KMS key to use for encryption of your
    -- Network Firewall resources.
    type' :: EncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'encryptionConfiguration_keyId' - The ID of the Amazon Web Services Key Management Service (KMS) customer
-- managed key. You can use any of the key identifiers that KMS supports,
-- unless you\'re using a key that\'s managed by another account. If
-- you\'re using a key managed by another account, then specify the key
-- ARN. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key ID>
-- in the /Amazon Web Services KMS Developer Guide/.
--
-- 'type'', 'encryptionConfiguration_type' - The type of Amazon Web Services KMS key to use for encryption of your
-- Network Firewall resources.
newEncryptionConfiguration ::
  -- | 'type''
  EncryptionType ->
  EncryptionConfiguration
newEncryptionConfiguration pType_ =
  EncryptionConfiguration'
    { keyId = Prelude.Nothing,
      type' = pType_
    }

-- | The ID of the Amazon Web Services Key Management Service (KMS) customer
-- managed key. You can use any of the key identifiers that KMS supports,
-- unless you\'re using a key that\'s managed by another account. If
-- you\'re using a key managed by another account, then specify the key
-- ARN. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key ID>
-- in the /Amazon Web Services KMS Developer Guide/.
encryptionConfiguration_keyId :: Lens.Lens' EncryptionConfiguration (Prelude.Maybe Prelude.Text)
encryptionConfiguration_keyId = Lens.lens (\EncryptionConfiguration' {keyId} -> keyId) (\s@EncryptionConfiguration' {} a -> s {keyId = a} :: EncryptionConfiguration)

-- | The type of Amazon Web Services KMS key to use for encryption of your
-- Network Firewall resources.
encryptionConfiguration_type :: Lens.Lens' EncryptionConfiguration EncryptionType
encryptionConfiguration_type = Lens.lens (\EncryptionConfiguration' {type'} -> type') (\s@EncryptionConfiguration' {} a -> s {type' = a} :: EncryptionConfiguration)

instance Data.FromJSON EncryptionConfiguration where
  parseJSON =
    Data.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Prelude.<$> (x Data..:? "KeyId")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable EncryptionConfiguration where
  hashWithSalt _salt EncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EncryptionConfiguration where
  rnf EncryptionConfiguration' {..} =
    Prelude.rnf keyId `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyId" Data..=) Prelude.<$> keyId,
            Prelude.Just ("Type" Data..= type')
          ]
      )
