{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobBookmarksEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarksEncryption
  ( JobBookmarksEncryption (..),

    -- * Smart constructor
    mkJobBookmarksEncryption,

    -- * Lenses
    jbeJobBookmarksEncryptionMode,
    jbeKMSKeyARN,
  )
where

import Network.AWS.Glue.Types.JobBookmarksEncryptionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies how job bookmark data should be encrypted.
--
-- /See:/ 'mkJobBookmarksEncryption' smart constructor.
data JobBookmarksEncryption = JobBookmarksEncryption'
  { -- | The encryption mode to use for job bookmarks data.
    jobBookmarksEncryptionMode :: Lude.Maybe JobBookmarksEncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
    kmsKeyARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobBookmarksEncryption' with the minimum fields required to make a request.
--
-- * 'jobBookmarksEncryptionMode' - The encryption mode to use for job bookmarks data.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
mkJobBookmarksEncryption ::
  JobBookmarksEncryption
mkJobBookmarksEncryption =
  JobBookmarksEncryption'
    { jobBookmarksEncryptionMode =
        Lude.Nothing,
      kmsKeyARN = Lude.Nothing
    }

-- | The encryption mode to use for job bookmarks data.
--
-- /Note:/ Consider using 'jobBookmarksEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeJobBookmarksEncryptionMode :: Lens.Lens' JobBookmarksEncryption (Lude.Maybe JobBookmarksEncryptionMode)
jbeJobBookmarksEncryptionMode = Lens.lens (jobBookmarksEncryptionMode :: JobBookmarksEncryption -> Lude.Maybe JobBookmarksEncryptionMode) (\s a -> s {jobBookmarksEncryptionMode = a} :: JobBookmarksEncryption)
{-# DEPRECATED jbeJobBookmarksEncryptionMode "Use generic-lens or generic-optics with 'jobBookmarksEncryptionMode' instead." #-}

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeKMSKeyARN :: Lens.Lens' JobBookmarksEncryption (Lude.Maybe Lude.Text)
jbeKMSKeyARN = Lens.lens (kmsKeyARN :: JobBookmarksEncryption -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: JobBookmarksEncryption)
{-# DEPRECATED jbeKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

instance Lude.FromJSON JobBookmarksEncryption where
  parseJSON =
    Lude.withObject
      "JobBookmarksEncryption"
      ( \x ->
          JobBookmarksEncryption'
            Lude.<$> (x Lude..:? "JobBookmarksEncryptionMode")
            Lude.<*> (x Lude..:? "KmsKeyArn")
      )

instance Lude.ToJSON JobBookmarksEncryption where
  toJSON JobBookmarksEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobBookmarksEncryptionMode" Lude..=)
              Lude.<$> jobBookmarksEncryptionMode,
            ("KmsKeyArn" Lude..=) Lude.<$> kmsKeyARN
          ]
      )
