{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.EncryptionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EncryptionOptions
  ( EncryptionOptions (..),

    -- * Smart constructor
    mkEncryptionOptions,

    -- * Lenses
    eoUseAwsOwnedKey,
    eoKmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encryption options for the broker.
--
-- /See:/ 'mkEncryptionOptions' smart constructor.
data EncryptionOptions = EncryptionOptions'
  { -- | Enables the use of an AWS owned CMK using AWS Key Management Service (KMS).
    useAwsOwnedKey :: Core.Bool,
    -- | The symmetric customer master key (CMK) to use for the AWS Key Management Service (KMS). This key is used to encrypt your data at rest. If not provided, Amazon MQ will use a default CMK to encrypt your data.
    kmsKeyId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionOptions' value with any optional fields omitted.
mkEncryptionOptions ::
  -- | 'useAwsOwnedKey'
  Core.Bool ->
  EncryptionOptions
mkEncryptionOptions useAwsOwnedKey =
  EncryptionOptions' {useAwsOwnedKey, kmsKeyId = Core.Nothing}

-- | Enables the use of an AWS owned CMK using AWS Key Management Service (KMS).
--
-- /Note:/ Consider using 'useAwsOwnedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoUseAwsOwnedKey :: Lens.Lens' EncryptionOptions Core.Bool
eoUseAwsOwnedKey = Lens.field @"useAwsOwnedKey"
{-# DEPRECATED eoUseAwsOwnedKey "Use generic-lens or generic-optics with 'useAwsOwnedKey' instead." #-}

-- | The symmetric customer master key (CMK) to use for the AWS Key Management Service (KMS). This key is used to encrypt your data at rest. If not provided, Amazon MQ will use a default CMK to encrypt your data.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoKmsKeyId :: Lens.Lens' EncryptionOptions (Core.Maybe Core.Text)
eoKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED eoKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON EncryptionOptions where
  toJSON EncryptionOptions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("useAwsOwnedKey" Core..= useAwsOwnedKey),
            ("kmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )

instance Core.FromJSON EncryptionOptions where
  parseJSON =
    Core.withObject "EncryptionOptions" Core.$
      \x ->
        EncryptionOptions'
          Core.<$> (x Core..: "useAwsOwnedKey") Core.<*> (x Core..:? "kmsKeyId")
