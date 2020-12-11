-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.S3ReportExportConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.S3ReportExportConfig
  ( S3ReportExportConfig (..),

    -- * Smart constructor
    mkS3ReportExportConfig,

    -- * Lenses
    srecPackaging,
    srecPath,
    srecBucket,
    srecEncryptionDisabled,
    srecEncryptionKey,
  )
where

import Network.AWS.CodeBuild.Types.ReportPackagingType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the S3 bucket where the raw data of a report are exported.
--
-- /See:/ 'mkS3ReportExportConfig' smart constructor.
data S3ReportExportConfig = S3ReportExportConfig'
  { packaging ::
      Lude.Maybe ReportPackagingType,
    path :: Lude.Maybe Lude.Text,
    bucket :: Lude.Maybe Lude.Text,
    encryptionDisabled :: Lude.Maybe Lude.Bool,
    encryptionKey :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3ReportExportConfig' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the S3 bucket where the raw data of a report are exported.
-- * 'encryptionDisabled' - A boolean value that specifies if the results of a report are encrypted.
-- * 'encryptionKey' - The encryption key for the report's encrypted raw data.
-- * 'packaging' - The type of build output artifact to create. Valid values include:
--
--
--     * @NONE@ : AWS CodeBuild creates the raw data in the output bucket. This is the default if packaging is not specified.
--
--
--     * @ZIP@ : AWS CodeBuild creates a ZIP file with the raw data in the output bucket.
--
--
-- * 'path' - The path to the exported report's raw data results.
mkS3ReportExportConfig ::
  S3ReportExportConfig
mkS3ReportExportConfig =
  S3ReportExportConfig'
    { packaging = Lude.Nothing,
      path = Lude.Nothing,
      bucket = Lude.Nothing,
      encryptionDisabled = Lude.Nothing,
      encryptionKey = Lude.Nothing
    }

-- | The type of build output artifact to create. Valid values include:
--
--
--     * @NONE@ : AWS CodeBuild creates the raw data in the output bucket. This is the default if packaging is not specified.
--
--
--     * @ZIP@ : AWS CodeBuild creates a ZIP file with the raw data in the output bucket.
--
--
--
-- /Note:/ Consider using 'packaging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecPackaging :: Lens.Lens' S3ReportExportConfig (Lude.Maybe ReportPackagingType)
srecPackaging = Lens.lens (packaging :: S3ReportExportConfig -> Lude.Maybe ReportPackagingType) (\s a -> s {packaging = a} :: S3ReportExportConfig)
{-# DEPRECATED srecPackaging "Use generic-lens or generic-optics with 'packaging' instead." #-}

-- | The path to the exported report's raw data results.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecPath :: Lens.Lens' S3ReportExportConfig (Lude.Maybe Lude.Text)
srecPath = Lens.lens (path :: S3ReportExportConfig -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: S3ReportExportConfig)
{-# DEPRECATED srecPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the S3 bucket where the raw data of a report are exported.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecBucket :: Lens.Lens' S3ReportExportConfig (Lude.Maybe Lude.Text)
srecBucket = Lens.lens (bucket :: S3ReportExportConfig -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: S3ReportExportConfig)
{-# DEPRECATED srecBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | A boolean value that specifies if the results of a report are encrypted.
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecEncryptionDisabled :: Lens.Lens' S3ReportExportConfig (Lude.Maybe Lude.Bool)
srecEncryptionDisabled = Lens.lens (encryptionDisabled :: S3ReportExportConfig -> Lude.Maybe Lude.Bool) (\s a -> s {encryptionDisabled = a} :: S3ReportExportConfig)
{-# DEPRECATED srecEncryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead." #-}

-- | The encryption key for the report's encrypted raw data.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecEncryptionKey :: Lens.Lens' S3ReportExportConfig (Lude.Maybe Lude.Text)
srecEncryptionKey = Lens.lens (encryptionKey :: S3ReportExportConfig -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKey = a} :: S3ReportExportConfig)
{-# DEPRECATED srecEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

instance Lude.FromJSON S3ReportExportConfig where
  parseJSON =
    Lude.withObject
      "S3ReportExportConfig"
      ( \x ->
          S3ReportExportConfig'
            Lude.<$> (x Lude..:? "packaging")
            Lude.<*> (x Lude..:? "path")
            Lude.<*> (x Lude..:? "bucket")
            Lude.<*> (x Lude..:? "encryptionDisabled")
            Lude.<*> (x Lude..:? "encryptionKey")
      )

instance Lude.ToJSON S3ReportExportConfig where
  toJSON S3ReportExportConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("packaging" Lude..=) Lude.<$> packaging,
            ("path" Lude..=) Lude.<$> path,
            ("bucket" Lude..=) Lude.<$> bucket,
            ("encryptionDisabled" Lude..=) Lude.<$> encryptionDisabled,
            ("encryptionKey" Lude..=) Lude.<$> encryptionKey
          ]
      )
