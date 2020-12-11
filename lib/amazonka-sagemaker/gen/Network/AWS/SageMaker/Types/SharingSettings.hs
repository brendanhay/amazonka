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
    ssS3KMSKeyId,
    ssS3OutputPath,
    ssNotebookOutputOption,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.NotebookOutputOption

-- | Specifies options when sharing an Amazon SageMaker Studio notebook. These settings are specified as part of @DefaultUserSettings@ when the 'CreateDomain' API is called, and as part of @UserSettings@ when the 'CreateUserProfile' API is called.
--
-- /See:/ 'mkSharingSettings' smart constructor.
data SharingSettings = SharingSettings'
  { s3KMSKeyId ::
      Lude.Maybe Lude.Text,
    s3OutputPath :: Lude.Maybe Lude.Text,
    notebookOutputOption :: Lude.Maybe NotebookOutputOption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SharingSettings' with the minimum fields required to make a request.
--
-- * 'notebookOutputOption' - Whether to include the notebook cell output when sharing the notebook. The default is @Disabled@ .
-- * 's3KMSKeyId' - When @NotebookOutputOption@ is @Allowed@ , the AWS Key Management Service (KMS) encryption key ID used to encrypt the notebook cell output in the Amazon S3 bucket.
-- * 's3OutputPath' - When @NotebookOutputOption@ is @Allowed@ , the Amazon S3 bucket used to save the notebook cell output.
mkSharingSettings ::
  SharingSettings
mkSharingSettings =
  SharingSettings'
    { s3KMSKeyId = Lude.Nothing,
      s3OutputPath = Lude.Nothing,
      notebookOutputOption = Lude.Nothing
    }

-- | When @NotebookOutputOption@ is @Allowed@ , the AWS Key Management Service (KMS) encryption key ID used to encrypt the notebook cell output in the Amazon S3 bucket.
--
-- /Note:/ Consider using 's3KMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssS3KMSKeyId :: Lens.Lens' SharingSettings (Lude.Maybe Lude.Text)
ssS3KMSKeyId = Lens.lens (s3KMSKeyId :: SharingSettings -> Lude.Maybe Lude.Text) (\s a -> s {s3KMSKeyId = a} :: SharingSettings)
{-# DEPRECATED ssS3KMSKeyId "Use generic-lens or generic-optics with 's3KMSKeyId' instead." #-}

-- | When @NotebookOutputOption@ is @Allowed@ , the Amazon S3 bucket used to save the notebook cell output.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssS3OutputPath :: Lens.Lens' SharingSettings (Lude.Maybe Lude.Text)
ssS3OutputPath = Lens.lens (s3OutputPath :: SharingSettings -> Lude.Maybe Lude.Text) (\s a -> s {s3OutputPath = a} :: SharingSettings)
{-# DEPRECATED ssS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | Whether to include the notebook cell output when sharing the notebook. The default is @Disabled@ .
--
-- /Note:/ Consider using 'notebookOutputOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNotebookOutputOption :: Lens.Lens' SharingSettings (Lude.Maybe NotebookOutputOption)
ssNotebookOutputOption = Lens.lens (notebookOutputOption :: SharingSettings -> Lude.Maybe NotebookOutputOption) (\s a -> s {notebookOutputOption = a} :: SharingSettings)
{-# DEPRECATED ssNotebookOutputOption "Use generic-lens or generic-optics with 'notebookOutputOption' instead." #-}

instance Lude.FromJSON SharingSettings where
  parseJSON =
    Lude.withObject
      "SharingSettings"
      ( \x ->
          SharingSettings'
            Lude.<$> (x Lude..:? "S3KmsKeyId")
            Lude.<*> (x Lude..:? "S3OutputPath")
            Lude.<*> (x Lude..:? "NotebookOutputOption")
      )

instance Lude.ToJSON SharingSettings where
  toJSON SharingSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3KmsKeyId" Lude..=) Lude.<$> s3KMSKeyId,
            ("S3OutputPath" Lude..=) Lude.<$> s3OutputPath,
            ("NotebookOutputOption" Lude..=) Lude.<$> notebookOutputOption
          ]
      )
