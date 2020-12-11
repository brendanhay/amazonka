-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionPasswordEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionPasswordEncryption
  ( ConnectionPasswordEncryption (..),

    -- * Smart constructor
    mkConnectionPasswordEncryption,

    -- * Lenses
    cpeAWSKMSKeyId,
    cpeReturnConnectionPasswordEncrypted,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The data structure used by the Data Catalog to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
--
-- When a @CreationConnection@ request arrives containing a password, the Data Catalog first encrypts the password using your AWS KMS key. It then encrypts the whole connection object again if catalog encryption is also enabled.
-- This encryption requires that you set AWS KMS key permissions to enable or restrict access on the password key according to your security requirements. For example, you might want only administrators to have decrypt permission on the password key.
--
-- /See:/ 'mkConnectionPasswordEncryption' smart constructor.
data ConnectionPasswordEncryption = ConnectionPasswordEncryption'
  { awsKMSKeyId ::
      Lude.Maybe Lude.Text,
    returnConnectionPasswordEncrypted ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionPasswordEncryption' with the minimum fields required to make a request.
--
-- * 'awsKMSKeyId' - An AWS KMS key that is used to encrypt the connection password.
--
-- If connection password protection is enabled, the caller of @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@ permission on the specified AWS KMS key, to encrypt passwords before storing them in the Data Catalog.
-- You can set the decrypt permission to enable or restrict access on the password key according to your security requirements.
-- * 'returnConnectionPasswordEncrypted' - When the @ReturnConnectionPasswordEncrypted@ flag is set to "true", passwords remain encrypted in the responses of @GetConnection@ and @GetConnections@ . This encryption takes effect independently from catalog encryption.
mkConnectionPasswordEncryption ::
  -- | 'returnConnectionPasswordEncrypted'
  Lude.Bool ->
  ConnectionPasswordEncryption
mkConnectionPasswordEncryption pReturnConnectionPasswordEncrypted_ =
  ConnectionPasswordEncryption'
    { awsKMSKeyId = Lude.Nothing,
      returnConnectionPasswordEncrypted =
        pReturnConnectionPasswordEncrypted_
    }

-- | An AWS KMS key that is used to encrypt the connection password.
--
-- If connection password protection is enabled, the caller of @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@ permission on the specified AWS KMS key, to encrypt passwords before storing them in the Data Catalog.
-- You can set the decrypt permission to enable or restrict access on the password key according to your security requirements.
--
-- /Note:/ Consider using 'awsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeAWSKMSKeyId :: Lens.Lens' ConnectionPasswordEncryption (Lude.Maybe Lude.Text)
cpeAWSKMSKeyId = Lens.lens (awsKMSKeyId :: ConnectionPasswordEncryption -> Lude.Maybe Lude.Text) (\s a -> s {awsKMSKeyId = a} :: ConnectionPasswordEncryption)
{-# DEPRECATED cpeAWSKMSKeyId "Use generic-lens or generic-optics with 'awsKMSKeyId' instead." #-}

-- | When the @ReturnConnectionPasswordEncrypted@ flag is set to "true", passwords remain encrypted in the responses of @GetConnection@ and @GetConnections@ . This encryption takes effect independently from catalog encryption.
--
-- /Note:/ Consider using 'returnConnectionPasswordEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeReturnConnectionPasswordEncrypted :: Lens.Lens' ConnectionPasswordEncryption Lude.Bool
cpeReturnConnectionPasswordEncrypted = Lens.lens (returnConnectionPasswordEncrypted :: ConnectionPasswordEncryption -> Lude.Bool) (\s a -> s {returnConnectionPasswordEncrypted = a} :: ConnectionPasswordEncryption)
{-# DEPRECATED cpeReturnConnectionPasswordEncrypted "Use generic-lens or generic-optics with 'returnConnectionPasswordEncrypted' instead." #-}

instance Lude.FromJSON ConnectionPasswordEncryption where
  parseJSON =
    Lude.withObject
      "ConnectionPasswordEncryption"
      ( \x ->
          ConnectionPasswordEncryption'
            Lude.<$> (x Lude..:? "AwsKmsKeyId")
            Lude.<*> (x Lude..: "ReturnConnectionPasswordEncrypted")
      )

instance Lude.ToJSON ConnectionPasswordEncryption where
  toJSON ConnectionPasswordEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AwsKmsKeyId" Lude..=) Lude.<$> awsKMSKeyId,
            Lude.Just
              ( "ReturnConnectionPasswordEncrypted"
                  Lude..= returnConnectionPasswordEncrypted
              )
          ]
      )
