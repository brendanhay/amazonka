{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.EncryptionConfiguration
  ( EncryptionConfiguration (..),

    -- * Smart constructor
    mkEncryptionConfiguration,

    -- * Lenses
    ecNoEncryptionConfig,
    ecKMSEncryptionConfig,
  )
where

import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.NoEncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the encryption for a destination in Amazon S3.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { noEncryptionConfig ::
      Lude.Maybe NoEncryptionConfig,
    kmsEncryptionConfig ::
      Lude.Maybe KMSEncryptionConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- * 'kmsEncryptionConfig' - The encryption key.
-- * 'noEncryptionConfig' - Specifically override existing encryption information to ensure that no encryption is used.
mkEncryptionConfiguration ::
  EncryptionConfiguration
mkEncryptionConfiguration =
  EncryptionConfiguration'
    { noEncryptionConfig = Lude.Nothing,
      kmsEncryptionConfig = Lude.Nothing
    }

-- | Specifically override existing encryption information to ensure that no encryption is used.
--
-- /Note:/ Consider using 'noEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecNoEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Lude.Maybe NoEncryptionConfig)
ecNoEncryptionConfig = Lens.lens (noEncryptionConfig :: EncryptionConfiguration -> Lude.Maybe NoEncryptionConfig) (\s a -> s {noEncryptionConfig = a} :: EncryptionConfiguration)
{-# DEPRECATED ecNoEncryptionConfig "Use generic-lens or generic-optics with 'noEncryptionConfig' instead." #-}

-- | The encryption key.
--
-- /Note:/ Consider using 'kmsEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKMSEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Lude.Maybe KMSEncryptionConfig)
ecKMSEncryptionConfig = Lens.lens (kmsEncryptionConfig :: EncryptionConfiguration -> Lude.Maybe KMSEncryptionConfig) (\s a -> s {kmsEncryptionConfig = a} :: EncryptionConfiguration)
{-# DEPRECATED ecKMSEncryptionConfig "Use generic-lens or generic-optics with 'kmsEncryptionConfig' instead." #-}

instance Lude.FromJSON EncryptionConfiguration where
  parseJSON =
    Lude.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Lude.<$> (x Lude..:? "NoEncryptionConfig")
            Lude.<*> (x Lude..:? "KMSEncryptionConfig")
      )

instance Lude.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NoEncryptionConfig" Lude..=) Lude.<$> noEncryptionConfig,
            ("KMSEncryptionConfig" Lude..=) Lude.<$> kmsEncryptionConfig
          ]
      )
