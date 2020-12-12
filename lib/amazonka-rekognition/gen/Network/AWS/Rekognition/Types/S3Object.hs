{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.S3Object
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.S3Object
  ( S3Object (..),

    -- * Smart constructor
    mkS3Object,

    -- * Lenses
    soBucket,
    soName,
    soVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the S3 bucket name and object name.
--
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
-- For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see Resource-Based Policies in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkS3Object' smart constructor.
data S3Object = S3Object'
  { bucket :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'S3Object' with the minimum fields required to make a request.
--
-- * 'bucket' - Name of the S3 bucket.
-- * 'name' - S3 object key name.
-- * 'version' - If the bucket is versioning enabled, you can specify the object version.
mkS3Object ::
  S3Object
mkS3Object =
  S3Object'
    { bucket = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing
    }

-- | Name of the S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soBucket :: Lens.Lens' S3Object (Lude.Maybe Lude.Text)
soBucket = Lens.lens (bucket :: S3Object -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: S3Object)
{-# DEPRECATED soBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | S3 object key name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soName :: Lens.Lens' S3Object (Lude.Maybe Lude.Text)
soName = Lens.lens (name :: S3Object -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: S3Object)
{-# DEPRECATED soName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If the bucket is versioning enabled, you can specify the object version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soVersion :: Lens.Lens' S3Object (Lude.Maybe Lude.Text)
soVersion = Lens.lens (version :: S3Object -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: S3Object)
{-# DEPRECATED soVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON S3Object where
  parseJSON =
    Lude.withObject
      "S3Object"
      ( \x ->
          S3Object'
            Lude.<$> (x Lude..:? "Bucket")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
      )

instance Lude.ToJSON S3Object where
  toJSON S3Object' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Bucket" Lude..=) Lude.<$> bucket,
            ("Name" Lude..=) Lude.<$> name,
            ("Version" Lude..=) Lude.<$> version
          ]
      )
