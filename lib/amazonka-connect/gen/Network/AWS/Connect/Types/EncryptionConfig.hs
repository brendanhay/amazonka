-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.EncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.EncryptionConfig
  ( EncryptionConfig (..),

    -- * Smart constructor
    mkEncryptionConfig,

    -- * Lenses
    ecEncryptionType,
    ecKeyId,
  )
where

import Network.AWS.Connect.Types.EncryptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The encryption configuration.
--
-- /See:/ 'mkEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { encryptionType ::
      EncryptionType,
    keyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionConfig' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The type of encryption.
-- * 'keyId' - The identifier of the encryption key.
mkEncryptionConfig ::
  -- | 'encryptionType'
  EncryptionType ->
  -- | 'keyId'
  Lude.Text ->
  EncryptionConfig
mkEncryptionConfig pEncryptionType_ pKeyId_ =
  EncryptionConfig'
    { encryptionType = pEncryptionType_,
      keyId = pKeyId_
    }

-- | The type of encryption.
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEncryptionType :: Lens.Lens' EncryptionConfig EncryptionType
ecEncryptionType = Lens.lens (encryptionType :: EncryptionConfig -> EncryptionType) (\s a -> s {encryptionType = a} :: EncryptionConfig)
{-# DEPRECATED ecEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The identifier of the encryption key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKeyId :: Lens.Lens' EncryptionConfig Lude.Text
ecKeyId = Lens.lens (keyId :: EncryptionConfig -> Lude.Text) (\s a -> s {keyId = a} :: EncryptionConfig)
{-# DEPRECATED ecKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.FromJSON EncryptionConfig where
  parseJSON =
    Lude.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Lude.<$> (x Lude..: "EncryptionType") Lude.<*> (x Lude..: "KeyId")
      )

instance Lude.ToJSON EncryptionConfig where
  toJSON EncryptionConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EncryptionType" Lude..= encryptionType),
            Lude.Just ("KeyId" Lude..= keyId)
          ]
      )
