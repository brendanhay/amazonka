{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.EncryptionConfiguration
  ( EncryptionConfiguration (..),

    -- * Smart constructor
    mkEncryptionConfiguration,

    -- * Lenses
    ecEncryptionOption,
    ecKMSKey,
  )
where

import Network.AWS.Athena.Types.EncryptionOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used.
    --
    -- If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
    encryptionOption :: EncryptionOption,
    -- | For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
    kmsKey :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- * 'encryptionOption' - Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used.
--
-- If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
-- * 'kmsKey' - For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
mkEncryptionConfiguration ::
  -- | 'encryptionOption'
  EncryptionOption ->
  EncryptionConfiguration
mkEncryptionConfiguration pEncryptionOption_ =
  EncryptionConfiguration'
    { encryptionOption = pEncryptionOption_,
      kmsKey = Lude.Nothing
    }

-- | Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used.
--
-- If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
--
-- /Note:/ Consider using 'encryptionOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEncryptionOption :: Lens.Lens' EncryptionConfiguration EncryptionOption
ecEncryptionOption = Lens.lens (encryptionOption :: EncryptionConfiguration -> EncryptionOption) (\s a -> s {encryptionOption = a} :: EncryptionConfiguration)
{-# DEPRECATED ecEncryptionOption "Use generic-lens or generic-optics with 'encryptionOption' instead." #-}

-- | For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKMSKey :: Lens.Lens' EncryptionConfiguration (Lude.Maybe Lude.Text)
ecKMSKey = Lens.lens (kmsKey :: EncryptionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: EncryptionConfiguration)
{-# DEPRECATED ecKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

instance Lude.FromJSON EncryptionConfiguration where
  parseJSON =
    Lude.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Lude.<$> (x Lude..: "EncryptionOption") Lude.<*> (x Lude..:? "KmsKey")
      )

instance Lude.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EncryptionOption" Lude..= encryptionOption),
            ("KmsKey" Lude..=) Lude.<$> kmsKey
          ]
      )
