{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slBundleType,
    slETag,
    slBucket,
    slKey,
    slVersion,
  )
where

import Network.AWS.CodeDeploy.Types.BundleType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the location of application artifacts stored in Amazon S3.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { bundleType :: Lude.Maybe BundleType,
    eTag :: Lude.Maybe Lude.Text,
    bucket :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket where the application revision is stored.
-- * 'bundleType' - The file type of the application revision. Must be one of the following:
--
--
--     * @tar@ : A tar archive file.
--
--
--     * @tgz@ : A compressed tar archive file.
--
--
--     * @zip@ : A zip archive file.
--
--
-- * 'eTag' - The ETag of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of the object is skipped.
-- * 'key' - The name of the Amazon S3 object that represents the bundled artifacts for the application revision.
-- * 'version' - A specific version of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version by default.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location'
    { bundleType = Lude.Nothing,
      eTag = Lude.Nothing,
      bucket = Lude.Nothing,
      key = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The file type of the application revision. Must be one of the following:
--
--
--     * @tar@ : A tar archive file.
--
--
--     * @tgz@ : A compressed tar archive file.
--
--
--     * @zip@ : A zip archive file.
--
--
--
-- /Note:/ Consider using 'bundleType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBundleType :: Lens.Lens' S3Location (Lude.Maybe BundleType)
slBundleType = Lens.lens (bundleType :: S3Location -> Lude.Maybe BundleType) (\s a -> s {bundleType = a} :: S3Location)
{-# DEPRECATED slBundleType "Use generic-lens or generic-optics with 'bundleType' instead." #-}

-- | The ETag of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of the object is skipped.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slETag :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slETag = Lens.lens (eTag :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: S3Location)
{-# DEPRECATED slETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The name of the Amazon S3 bucket where the application revision is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slBucket = Lens.lens (bucket :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: S3Location)
{-# DEPRECATED slBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The name of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slKey = Lens.lens (key :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: S3Location)
{-# DEPRECATED slKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | A specific version of the Amazon S3 object that represents the bundled artifacts for the application revision.
--
-- If the version is not specified, the system uses the most recent version by default.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slVersion :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slVersion = Lens.lens (version :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: S3Location)
{-# DEPRECATED slVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON S3Location where
  parseJSON =
    Lude.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Lude.<$> (x Lude..:? "bundleType")
            Lude.<*> (x Lude..:? "eTag")
            Lude.<*> (x Lude..:? "bucket")
            Lude.<*> (x Lude..:? "key")
            Lude.<*> (x Lude..:? "version")
      )

instance Lude.ToJSON S3Location where
  toJSON S3Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bundleType" Lude..=) Lude.<$> bundleType,
            ("eTag" Lude..=) Lude.<$> eTag,
            ("bucket" Lude..=) Lude.<$> bucket,
            ("key" Lude..=) Lude.<$> key,
            ("version" Lude..=) Lude.<$> version
          ]
      )
