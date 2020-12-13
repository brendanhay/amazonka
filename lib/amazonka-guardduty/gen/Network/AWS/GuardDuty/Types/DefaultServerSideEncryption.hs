{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
  ( DefaultServerSideEncryption (..),

    -- * Smart constructor
    mkDefaultServerSideEncryption,

    -- * Lenses
    dsseEncryptionType,
    dsseKMSMasterKeyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the server side encryption method used in the S3 bucket. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html S3 Server-Side Encryption> for more information.
--
-- /See:/ 'mkDefaultServerSideEncryption' smart constructor.
data DefaultServerSideEncryption = DefaultServerSideEncryption'
  { -- | The type of encryption used for objects within the S3 bucket.
    encryptionType :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the KMS encryption key. Only available if the bucket @EncryptionType@ is @aws:kms@ .
    kmsMasterKeyARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefaultServerSideEncryption' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The type of encryption used for objects within the S3 bucket.
-- * 'kmsMasterKeyARN' - The Amazon Resource Name (ARN) of the KMS encryption key. Only available if the bucket @EncryptionType@ is @aws:kms@ .
mkDefaultServerSideEncryption ::
  DefaultServerSideEncryption
mkDefaultServerSideEncryption =
  DefaultServerSideEncryption'
    { encryptionType = Lude.Nothing,
      kmsMasterKeyARN = Lude.Nothing
    }

-- | The type of encryption used for objects within the S3 bucket.
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsseEncryptionType :: Lens.Lens' DefaultServerSideEncryption (Lude.Maybe Lude.Text)
dsseEncryptionType = Lens.lens (encryptionType :: DefaultServerSideEncryption -> Lude.Maybe Lude.Text) (\s a -> s {encryptionType = a} :: DefaultServerSideEncryption)
{-# DEPRECATED dsseEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | The Amazon Resource Name (ARN) of the KMS encryption key. Only available if the bucket @EncryptionType@ is @aws:kms@ .
--
-- /Note:/ Consider using 'kmsMasterKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsseKMSMasterKeyARN :: Lens.Lens' DefaultServerSideEncryption (Lude.Maybe Lude.Text)
dsseKMSMasterKeyARN = Lens.lens (kmsMasterKeyARN :: DefaultServerSideEncryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsMasterKeyARN = a} :: DefaultServerSideEncryption)
{-# DEPRECATED dsseKMSMasterKeyARN "Use generic-lens or generic-optics with 'kmsMasterKeyARN' instead." #-}

instance Lude.FromJSON DefaultServerSideEncryption where
  parseJSON =
    Lude.withObject
      "DefaultServerSideEncryption"
      ( \x ->
          DefaultServerSideEncryption'
            Lude.<$> (x Lude..:? "encryptionType")
            Lude.<*> (x Lude..:? "kmsMasterKeyArn")
      )
