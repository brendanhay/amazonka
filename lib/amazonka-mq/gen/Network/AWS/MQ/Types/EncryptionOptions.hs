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
    eoUseAWSOwnedKey,
    eoKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encryption options for the broker.
--
-- /See:/ 'mkEncryptionOptions' smart constructor.
data EncryptionOptions = EncryptionOptions'
  { -- | Enables the use of an AWS owned CMK using AWS Key Management Service (KMS).
    useAWSOwnedKey :: Lude.Bool,
    -- | The symmetric customer master key (CMK) to use for the AWS Key Management Service (KMS). This key is used to encrypt your data at rest. If not provided, Amazon MQ will use a default CMK to encrypt your data.
    kmsKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionOptions' with the minimum fields required to make a request.
--
-- * 'useAWSOwnedKey' - Enables the use of an AWS owned CMK using AWS Key Management Service (KMS).
-- * 'kmsKeyId' - The symmetric customer master key (CMK) to use for the AWS Key Management Service (KMS). This key is used to encrypt your data at rest. If not provided, Amazon MQ will use a default CMK to encrypt your data.
mkEncryptionOptions ::
  -- | 'useAWSOwnedKey'
  Lude.Bool ->
  EncryptionOptions
mkEncryptionOptions pUseAWSOwnedKey_ =
  EncryptionOptions'
    { useAWSOwnedKey = pUseAWSOwnedKey_,
      kmsKeyId = Lude.Nothing
    }

-- | Enables the use of an AWS owned CMK using AWS Key Management Service (KMS).
--
-- /Note:/ Consider using 'useAWSOwnedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoUseAWSOwnedKey :: Lens.Lens' EncryptionOptions Lude.Bool
eoUseAWSOwnedKey = Lens.lens (useAWSOwnedKey :: EncryptionOptions -> Lude.Bool) (\s a -> s {useAWSOwnedKey = a} :: EncryptionOptions)
{-# DEPRECATED eoUseAWSOwnedKey "Use generic-lens or generic-optics with 'useAWSOwnedKey' instead." #-}

-- | The symmetric customer master key (CMK) to use for the AWS Key Management Service (KMS). This key is used to encrypt your data at rest. If not provided, Amazon MQ will use a default CMK to encrypt your data.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoKMSKeyId :: Lens.Lens' EncryptionOptions (Lude.Maybe Lude.Text)
eoKMSKeyId = Lens.lens (kmsKeyId :: EncryptionOptions -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: EncryptionOptions)
{-# DEPRECATED eoKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.FromJSON EncryptionOptions where
  parseJSON =
    Lude.withObject
      "EncryptionOptions"
      ( \x ->
          EncryptionOptions'
            Lude.<$> (x Lude..: "useAwsOwnedKey") Lude.<*> (x Lude..:? "kmsKeyId")
      )

instance Lude.ToJSON EncryptionOptions where
  toJSON EncryptionOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("useAwsOwnedKey" Lude..= useAWSOwnedKey),
            ("kmsKeyId" Lude..=) Lude.<$> kmsKeyId
          ]
      )
