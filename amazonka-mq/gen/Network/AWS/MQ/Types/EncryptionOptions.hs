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
-- Module      : Network.AWS.MQ.Types.EncryptionOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EncryptionOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Encryption options for the broker.
--
-- /See:/ 'newEncryptionOptions' smart constructor.
data EncryptionOptions = EncryptionOptions'
  { -- | The symmetric customer master key (CMK) to use for the AWS Key
    -- Management Service (KMS). This key is used to encrypt your data at rest.
    -- If not provided, Amazon MQ will use a default CMK to encrypt your data.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Enables the use of an AWS owned CMK using AWS Key Management Service
    -- (KMS).
    useAwsOwnedKey :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EncryptionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'encryptionOptions_kmsKeyId' - The symmetric customer master key (CMK) to use for the AWS Key
-- Management Service (KMS). This key is used to encrypt your data at rest.
-- If not provided, Amazon MQ will use a default CMK to encrypt your data.
--
-- 'useAwsOwnedKey', 'encryptionOptions_useAwsOwnedKey' - Enables the use of an AWS owned CMK using AWS Key Management Service
-- (KMS).
newEncryptionOptions ::
  -- | 'useAwsOwnedKey'
  Core.Bool ->
  EncryptionOptions
newEncryptionOptions pUseAwsOwnedKey_ =
  EncryptionOptions'
    { kmsKeyId = Core.Nothing,
      useAwsOwnedKey = pUseAwsOwnedKey_
    }

-- | The symmetric customer master key (CMK) to use for the AWS Key
-- Management Service (KMS). This key is used to encrypt your data at rest.
-- If not provided, Amazon MQ will use a default CMK to encrypt your data.
encryptionOptions_kmsKeyId :: Lens.Lens' EncryptionOptions (Core.Maybe Core.Text)
encryptionOptions_kmsKeyId = Lens.lens (\EncryptionOptions' {kmsKeyId} -> kmsKeyId) (\s@EncryptionOptions' {} a -> s {kmsKeyId = a} :: EncryptionOptions)

-- | Enables the use of an AWS owned CMK using AWS Key Management Service
-- (KMS).
encryptionOptions_useAwsOwnedKey :: Lens.Lens' EncryptionOptions Core.Bool
encryptionOptions_useAwsOwnedKey = Lens.lens (\EncryptionOptions' {useAwsOwnedKey} -> useAwsOwnedKey) (\s@EncryptionOptions' {} a -> s {useAwsOwnedKey = a} :: EncryptionOptions)

instance Core.FromJSON EncryptionOptions where
  parseJSON =
    Core.withObject
      "EncryptionOptions"
      ( \x ->
          EncryptionOptions'
            Core.<$> (x Core..:? "kmsKeyId")
            Core.<*> (x Core..: "useAwsOwnedKey")
      )

instance Core.Hashable EncryptionOptions

instance Core.NFData EncryptionOptions

instance Core.ToJSON EncryptionOptions where
  toJSON EncryptionOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
            Core.Just ("useAwsOwnedKey" Core..= useAwsOwnedKey)
          ]
      )
