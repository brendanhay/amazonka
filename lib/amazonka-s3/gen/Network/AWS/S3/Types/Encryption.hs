-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Encryption
  ( Encryption (..),

    -- * Smart constructor
    mkEncryption,

    -- * Lenses
    eKMSKeyId,
    eKMSContext,
    eEncryptionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryption

-- | Contains the type of server-side encryption used.
--
-- /See:/ 'mkEncryption' smart constructor.
data Encryption = Encryption'
  { kmsKeyId ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    kmsContext :: Lude.Maybe Lude.Text,
    encryptionType :: ServerSideEncryption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
-- * 'kmsContext' - If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
-- * 'kmsKeyId' - If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
mkEncryption ::
  -- | 'encryptionType'
  ServerSideEncryption ->
  Encryption
mkEncryption pEncryptionType_ =
  Encryption'
    { kmsKeyId = Lude.Nothing,
      kmsContext = Lude.Nothing,
      encryptionType = pEncryptionType_
    }

-- | If the encryption type is @aws:kms@ , this optional value specifies the ID of the symmetric customer managed AWS KMS CMK to use for encryption of job results. Amazon S3 only supports symmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSKeyId :: Lens.Lens' Encryption (Lude.Maybe (Lude.Sensitive Lude.Text))
eKMSKeyId = Lens.lens (kmsKeyId :: Encryption -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {kmsKeyId = a} :: Encryption)
{-# DEPRECATED eKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | If the encryption type is @aws:kms@ , this optional value can be used to specify the encryption context for the restore results.
--
-- /Note:/ Consider using 'kmsContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eKMSContext :: Lens.Lens' Encryption (Lude.Maybe Lude.Text)
eKMSContext = Lens.lens (kmsContext :: Encryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsContext = a} :: Encryption)
{-# DEPRECATED eKMSContext "Use generic-lens or generic-optics with 'kmsContext' instead." #-}

-- | The server-side encryption algorithm used when storing job results in Amazon S3 (for example, AES256, aws:kms).
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEncryptionType :: Lens.Lens' Encryption ServerSideEncryption
eEncryptionType = Lens.lens (encryptionType :: Encryption -> ServerSideEncryption) (\s a -> s {encryptionType = a} :: Encryption)
{-# DEPRECATED eEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

instance Lude.ToXML Encryption where
  toXML Encryption' {..} =
    Lude.mconcat
      [ "KMSKeyId" Lude.@= kmsKeyId,
        "KMSContext" Lude.@= kmsContext,
        "EncryptionType" Lude.@= encryptionType
      ]
