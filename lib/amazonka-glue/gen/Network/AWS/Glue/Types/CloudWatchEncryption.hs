-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CloudWatchEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CloudWatchEncryption
  ( CloudWatchEncryption (..),

    -- * Smart constructor
    mkCloudWatchEncryption,

    -- * Lenses
    cweCloudWatchEncryptionMode,
    cweKMSKeyARN,
  )
where

import Network.AWS.Glue.Types.CloudWatchEncryptionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies how Amazon CloudWatch data should be encrypted.
--
-- /See:/ 'mkCloudWatchEncryption' smart constructor.
data CloudWatchEncryption = CloudWatchEncryption'
  { cloudWatchEncryptionMode ::
      Lude.Maybe CloudWatchEncryptionMode,
    kmsKeyARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchEncryption' with the minimum fields required to make a request.
--
-- * 'cloudWatchEncryptionMode' - The encryption mode to use for CloudWatch data.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
mkCloudWatchEncryption ::
  CloudWatchEncryption
mkCloudWatchEncryption =
  CloudWatchEncryption'
    { cloudWatchEncryptionMode = Lude.Nothing,
      kmsKeyARN = Lude.Nothing
    }

-- | The encryption mode to use for CloudWatch data.
--
-- /Note:/ Consider using 'cloudWatchEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweCloudWatchEncryptionMode :: Lens.Lens' CloudWatchEncryption (Lude.Maybe CloudWatchEncryptionMode)
cweCloudWatchEncryptionMode = Lens.lens (cloudWatchEncryptionMode :: CloudWatchEncryption -> Lude.Maybe CloudWatchEncryptionMode) (\s a -> s {cloudWatchEncryptionMode = a} :: CloudWatchEncryption)
{-# DEPRECATED cweCloudWatchEncryptionMode "Use generic-lens or generic-optics with 'cloudWatchEncryptionMode' instead." #-}

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweKMSKeyARN :: Lens.Lens' CloudWatchEncryption (Lude.Maybe Lude.Text)
cweKMSKeyARN = Lens.lens (kmsKeyARN :: CloudWatchEncryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: CloudWatchEncryption)
{-# DEPRECATED cweKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

instance Lude.FromJSON CloudWatchEncryption where
  parseJSON =
    Lude.withObject
      "CloudWatchEncryption"
      ( \x ->
          CloudWatchEncryption'
            Lude.<$> (x Lude..:? "CloudWatchEncryptionMode")
            Lude.<*> (x Lude..:? "KmsKeyArn")
      )

instance Lude.ToJSON CloudWatchEncryption where
  toJSON CloudWatchEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CloudWatchEncryptionMode" Lude..=)
              Lude.<$> cloudWatchEncryptionMode,
            ("KmsKeyArn" Lude..=) Lude.<$> kmsKeyARN
          ]
      )
