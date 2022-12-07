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
-- Module      : Amazonka.MQ.Types.EncryptionOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.EncryptionOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Does not apply to RabbitMQ brokers.
--
-- Encryption options for the broker.
--
-- /See:/ 'newEncryptionOptions' smart constructor.
data EncryptionOptions = EncryptionOptions'
  { -- | The customer master key (CMK) to use for the AWS Key Management Service
    -- (KMS). This key is used to encrypt your data at rest. If not provided,
    -- Amazon MQ will use a default CMK to encrypt your data.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Enables the use of an AWS owned CMK using AWS Key Management Service
    -- (KMS). Set to true by default, if no value is provided, for example, for
    -- RabbitMQ brokers.
    useAwsOwnedKey :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'encryptionOptions_kmsKeyId' - The customer master key (CMK) to use for the AWS Key Management Service
-- (KMS). This key is used to encrypt your data at rest. If not provided,
-- Amazon MQ will use a default CMK to encrypt your data.
--
-- 'useAwsOwnedKey', 'encryptionOptions_useAwsOwnedKey' - Enables the use of an AWS owned CMK using AWS Key Management Service
-- (KMS). Set to true by default, if no value is provided, for example, for
-- RabbitMQ brokers.
newEncryptionOptions ::
  -- | 'useAwsOwnedKey'
  Prelude.Bool ->
  EncryptionOptions
newEncryptionOptions pUseAwsOwnedKey_ =
  EncryptionOptions'
    { kmsKeyId = Prelude.Nothing,
      useAwsOwnedKey = pUseAwsOwnedKey_
    }

-- | The customer master key (CMK) to use for the AWS Key Management Service
-- (KMS). This key is used to encrypt your data at rest. If not provided,
-- Amazon MQ will use a default CMK to encrypt your data.
encryptionOptions_kmsKeyId :: Lens.Lens' EncryptionOptions (Prelude.Maybe Prelude.Text)
encryptionOptions_kmsKeyId = Lens.lens (\EncryptionOptions' {kmsKeyId} -> kmsKeyId) (\s@EncryptionOptions' {} a -> s {kmsKeyId = a} :: EncryptionOptions)

-- | Enables the use of an AWS owned CMK using AWS Key Management Service
-- (KMS). Set to true by default, if no value is provided, for example, for
-- RabbitMQ brokers.
encryptionOptions_useAwsOwnedKey :: Lens.Lens' EncryptionOptions Prelude.Bool
encryptionOptions_useAwsOwnedKey = Lens.lens (\EncryptionOptions' {useAwsOwnedKey} -> useAwsOwnedKey) (\s@EncryptionOptions' {} a -> s {useAwsOwnedKey = a} :: EncryptionOptions)

instance Data.FromJSON EncryptionOptions where
  parseJSON =
    Data.withObject
      "EncryptionOptions"
      ( \x ->
          EncryptionOptions'
            Prelude.<$> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..: "useAwsOwnedKey")
      )

instance Prelude.Hashable EncryptionOptions where
  hashWithSalt _salt EncryptionOptions' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` useAwsOwnedKey

instance Prelude.NFData EncryptionOptions where
  rnf EncryptionOptions' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf useAwsOwnedKey

instance Data.ToJSON EncryptionOptions where
  toJSON EncryptionOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("useAwsOwnedKey" Data..= useAwsOwnedKey)
          ]
      )
