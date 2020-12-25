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
    jbeKmsKeyArn,
  )
where

import qualified Network.AWS.Glue.Types.JobBookmarksEncryptionMode as Types
import qualified Network.AWS.Glue.Types.KmsKeyArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies how job bookmark data should be encrypted.
--
-- /See:/ 'mkJobBookmarksEncryption' smart constructor.
data JobBookmarksEncryption = JobBookmarksEncryption'
  { -- | The encryption mode to use for job bookmarks data.
    jobBookmarksEncryptionMode :: Core.Maybe Types.JobBookmarksEncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
    kmsKeyArn :: Core.Maybe Types.KmsKeyArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobBookmarksEncryption' value with any optional fields omitted.
mkJobBookmarksEncryption ::
  JobBookmarksEncryption
mkJobBookmarksEncryption =
  JobBookmarksEncryption'
    { jobBookmarksEncryptionMode =
        Core.Nothing,
      kmsKeyArn = Core.Nothing
    }

-- | The encryption mode to use for job bookmarks data.
--
-- /Note:/ Consider using 'jobBookmarksEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeJobBookmarksEncryptionMode :: Lens.Lens' JobBookmarksEncryption (Core.Maybe Types.JobBookmarksEncryptionMode)
jbeJobBookmarksEncryptionMode = Lens.field @"jobBookmarksEncryptionMode"
{-# DEPRECATED jbeJobBookmarksEncryptionMode "Use generic-lens or generic-optics with 'jobBookmarksEncryptionMode' instead." #-}

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeKmsKeyArn :: Lens.Lens' JobBookmarksEncryption (Core.Maybe Types.KmsKeyArn)
jbeKmsKeyArn = Lens.field @"kmsKeyArn"
{-# DEPRECATED jbeKmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead." #-}

instance Core.FromJSON JobBookmarksEncryption where
  toJSON JobBookmarksEncryption {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobBookmarksEncryptionMode" Core..=)
              Core.<$> jobBookmarksEncryptionMode,
            ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn
          ]
      )

instance Core.FromJSON JobBookmarksEncryption where
  parseJSON =
    Core.withObject "JobBookmarksEncryption" Core.$
      \x ->
        JobBookmarksEncryption'
          Core.<$> (x Core..:? "JobBookmarksEncryptionMode")
          Core.<*> (x Core..:? "KmsKeyArn")
