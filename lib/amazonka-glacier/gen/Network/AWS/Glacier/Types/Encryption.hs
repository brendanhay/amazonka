{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Encryption
  ( Encryption (..),

    -- * Smart constructor
    mkEncryption,

    -- * Lenses
    eEncryptionType,
    eKMSKeyId,
    eKMSContext,
  )
where

import Network.AWS.Glacier.Types.EncryptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the encryption used to store the job results in Amazon S3.
--
-- /See:/ 'mkEncryption' smart constructor.
data Encryption = Encryption'
  { encryptionType ::
      Lude.Maybe EncryptionType,
    kmsKeyId :: Lude.Maybe Lude.Text,
    kmsContext :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
-- * 'kmsContext' - Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
-- * 'kmsKeyId' - The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
mkEncryption ::
  Encryption
mkEncryption =
  Encryption'
    { encryptionType = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      kmsContext = Lude.Nothing
    }

-- | The server-side encryption algorithm used when storing job results in Amazon S3, for example @AES256@ or @aws:kms@ .
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionType :: Lens.Lens' Encryption (Lude.Maybe EncryptionType)
eEncryptionType = Lens.lens (encryptionType :: Encryption -> Lude.Maybe EncryptionType) (\s a -> s {encryptionType = a} :: Encryption)
{-# DEPRECATED eEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS fail if not made by using Secure Sockets Layer (SSL) or Signature Version 4.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSKeyId :: Lens.Lens' Encryption (Lude.Maybe Lude.Text)
eKMSKeyId = Lens.lens (kmsKeyId :: Encryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Encryption)
{-# DEPRECATED eKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Optional. If the encryption type is @aws:kms@ , you can use this value to specify the encryption context for the job results.
--
-- /Note:/ Consider using 'kmsContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSContext :: Lens.Lens' Encryption (Lude.Maybe Lude.Text)
eKMSContext = Lens.lens (kmsContext :: Encryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsContext = a} :: Encryption)
{-# DEPRECATED eKMSContext "Use generic-lens or generic-optics with 'kmsContext' instead." #-}

instance Lude.FromJSON Encryption where
  parseJSON =
    Lude.withObject
      "Encryption"
      ( \x ->
          Encryption'
            Lude.<$> (x Lude..:? "EncryptionType")
            Lude.<*> (x Lude..:? "KMSKeyId")
            Lude.<*> (x Lude..:? "KMSContext")
      )

instance Lude.ToJSON Encryption where
  toJSON Encryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EncryptionType" Lude..=) Lude.<$> encryptionType,
            ("KMSKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("KMSContext" Lude..=) Lude.<$> kmsContext
          ]
      )
