{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MLUserDataEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MLUserDataEncryption
  ( MLUserDataEncryption (..),

    -- * Smart constructor
    mkMLUserDataEncryption,

    -- * Lenses
    mludeMlUserDataEncryptionMode,
    mludeKMSKeyId,
  )
where

import Network.AWS.Glue.Types.MLUserDataEncryptionModeString
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The encryption-at-rest settings of the transform that apply to accessing user data.
--
-- /See:/ 'mkMLUserDataEncryption' smart constructor.
data MLUserDataEncryption = MLUserDataEncryption'
  { -- | The encryption mode applied to user data. Valid values are:
    --
    --
    --     * DISABLED: encryption is disabled
    --
    --
    --     * SSEKMS: use of server-side encryption with AWS Key Management Service (SSE-KMS) for user data stored in Amazon S3.
    mlUserDataEncryptionMode :: MLUserDataEncryptionModeString,
    -- | The ID for the customer-provided KMS key.
    kmsKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MLUserDataEncryption' with the minimum fields required to make a request.
--
-- * 'mlUserDataEncryptionMode' - The encryption mode applied to user data. Valid values are:
--
--
--     * DISABLED: encryption is disabled
--
--
--     * SSEKMS: use of server-side encryption with AWS Key Management Service (SSE-KMS) for user data stored in Amazon S3.
--
--
-- * 'kmsKeyId' - The ID for the customer-provided KMS key.
mkMLUserDataEncryption ::
  -- | 'mlUserDataEncryptionMode'
  MLUserDataEncryptionModeString ->
  MLUserDataEncryption
mkMLUserDataEncryption pMlUserDataEncryptionMode_ =
  MLUserDataEncryption'
    { mlUserDataEncryptionMode =
        pMlUserDataEncryptionMode_,
      kmsKeyId = Lude.Nothing
    }

-- | The encryption mode applied to user data. Valid values are:
--
--
--     * DISABLED: encryption is disabled
--
--
--     * SSEKMS: use of server-side encryption with AWS Key Management Service (SSE-KMS) for user data stored in Amazon S3.
--
--
--
-- /Note:/ Consider using 'mlUserDataEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mludeMlUserDataEncryptionMode :: Lens.Lens' MLUserDataEncryption MLUserDataEncryptionModeString
mludeMlUserDataEncryptionMode = Lens.lens (mlUserDataEncryptionMode :: MLUserDataEncryption -> MLUserDataEncryptionModeString) (\s a -> s {mlUserDataEncryptionMode = a} :: MLUserDataEncryption)
{-# DEPRECATED mludeMlUserDataEncryptionMode "Use generic-lens or generic-optics with 'mlUserDataEncryptionMode' instead." #-}

-- | The ID for the customer-provided KMS key.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mludeKMSKeyId :: Lens.Lens' MLUserDataEncryption (Lude.Maybe Lude.Text)
mludeKMSKeyId = Lens.lens (kmsKeyId :: MLUserDataEncryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: MLUserDataEncryption)
{-# DEPRECATED mludeKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.FromJSON MLUserDataEncryption where
  parseJSON =
    Lude.withObject
      "MLUserDataEncryption"
      ( \x ->
          MLUserDataEncryption'
            Lude.<$> (x Lude..: "MlUserDataEncryptionMode")
            Lude.<*> (x Lude..:? "KmsKeyId")
      )

instance Lude.ToJSON MLUserDataEncryption where
  toJSON MLUserDataEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("MlUserDataEncryptionMode" Lude..= mlUserDataEncryptionMode),
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId
          ]
      )
