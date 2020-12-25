{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SharingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SharingSettings
  ( SharingSettings (..),

    -- * Smart constructor
    mkSharingSettings,

    -- * Lenses
    ssNotebookOutputOption,
    ssS3KmsKeyId,
    ssS3OutputPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.NotebookOutputOption as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types

-- | Specifies options when sharing an Amazon SageMaker Studio notebook. These settings are specified as part of @DefaultUserSettings@ when the 'CreateDomain' API is called, and as part of @UserSettings@ when the 'CreateUserProfile' API is called.
--
-- /See:/ 'mkSharingSettings' smart constructor.
data SharingSettings = SharingSettings'
  { -- | Whether to include the notebook cell output when sharing the notebook. The default is @Disabled@ .
    notebookOutputOption :: Core.Maybe Types.NotebookOutputOption,
    -- | When @NotebookOutputOption@ is @Allowed@ , the AWS Key Management Service (KMS) encryption key ID used to encrypt the notebook cell output in the Amazon S3 bucket.
    s3KmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | When @NotebookOutputOption@ is @Allowed@ , the Amazon S3 bucket used to save the notebook cell output.
    s3OutputPath :: Core.Maybe Types.S3OutputPath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SharingSettings' value with any optional fields omitted.
mkSharingSettings ::
  SharingSettings
mkSharingSettings =
  SharingSettings'
    { notebookOutputOption = Core.Nothing,
      s3KmsKeyId = Core.Nothing,
      s3OutputPath = Core.Nothing
    }

-- | Whether to include the notebook cell output when sharing the notebook. The default is @Disabled@ .
--
-- /Note:/ Consider using 'notebookOutputOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNotebookOutputOption :: Lens.Lens' SharingSettings (Core.Maybe Types.NotebookOutputOption)
ssNotebookOutputOption = Lens.field @"notebookOutputOption"
{-# DEPRECATED ssNotebookOutputOption "Use generic-lens or generic-optics with 'notebookOutputOption' instead." #-}

-- | When @NotebookOutputOption@ is @Allowed@ , the AWS Key Management Service (KMS) encryption key ID used to encrypt the notebook cell output in the Amazon S3 bucket.
--
-- /Note:/ Consider using 's3KmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssS3KmsKeyId :: Lens.Lens' SharingSettings (Core.Maybe Types.KmsKeyId)
ssS3KmsKeyId = Lens.field @"s3KmsKeyId"
{-# DEPRECATED ssS3KmsKeyId "Use generic-lens or generic-optics with 's3KmsKeyId' instead." #-}

-- | When @NotebookOutputOption@ is @Allowed@ , the Amazon S3 bucket used to save the notebook cell output.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssS3OutputPath :: Lens.Lens' SharingSettings (Core.Maybe Types.S3OutputPath)
ssS3OutputPath = Lens.field @"s3OutputPath"
{-# DEPRECATED ssS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

instance Core.FromJSON SharingSettings where
  toJSON SharingSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotebookOutputOption" Core..=) Core.<$> notebookOutputOption,
            ("S3KmsKeyId" Core..=) Core.<$> s3KmsKeyId,
            ("S3OutputPath" Core..=) Core.<$> s3OutputPath
          ]
      )

instance Core.FromJSON SharingSettings where
  parseJSON =
    Core.withObject "SharingSettings" Core.$
      \x ->
        SharingSettings'
          Core.<$> (x Core..:? "NotebookOutputOption")
          Core.<*> (x Core..:? "S3KmsKeyId")
          Core.<*> (x Core..:? "S3OutputPath")
