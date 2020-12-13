{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
  ( EncryptionAtRestOptions (..),

    -- * Smart constructor
    mkEncryptionAtRestOptions,

    -- * Lenses
    earoEnabled,
    earoKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the Encryption At Rest Options.
--
-- /See:/ 'mkEncryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { -- | Specifies the option to enable Encryption At Rest.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Specifies the KMS Key ID for Encryption At Rest options.
    kmsKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionAtRestOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies the option to enable Encryption At Rest.
-- * 'kmsKeyId' - Specifies the KMS Key ID for Encryption At Rest options.
mkEncryptionAtRestOptions ::
  EncryptionAtRestOptions
mkEncryptionAtRestOptions =
  EncryptionAtRestOptions'
    { enabled = Lude.Nothing,
      kmsKeyId = Lude.Nothing
    }

-- | Specifies the option to enable Encryption At Rest.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earoEnabled :: Lens.Lens' EncryptionAtRestOptions (Lude.Maybe Lude.Bool)
earoEnabled = Lens.lens (enabled :: EncryptionAtRestOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EncryptionAtRestOptions)
{-# DEPRECATED earoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies the KMS Key ID for Encryption At Rest options.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earoKMSKeyId :: Lens.Lens' EncryptionAtRestOptions (Lude.Maybe Lude.Text)
earoKMSKeyId = Lens.lens (kmsKeyId :: EncryptionAtRestOptions -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: EncryptionAtRestOptions)
{-# DEPRECATED earoKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.FromJSON EncryptionAtRestOptions where
  parseJSON =
    Lude.withObject
      "EncryptionAtRestOptions"
      ( \x ->
          EncryptionAtRestOptions'
            Lude.<$> (x Lude..:? "Enabled") Lude.<*> (x Lude..:? "KmsKeyId")
      )

instance Lude.ToJSON EncryptionAtRestOptions where
  toJSON EncryptionAtRestOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId
          ]
      )
