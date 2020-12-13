{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionContentInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionContentInput
  ( LayerVersionContentInput (..),

    -- * Smart constructor
    mkLayerVersionContentInput,

    -- * Lenses
    lvciS3ObjectVersion,
    lvciS3Key,
    lvciZipFile,
    lvciS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A ZIP archive that contains the contents of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . You can specify either an Amazon S3 location, or upload a layer archive directly.
--
-- /See:/ 'mkLayerVersionContentInput' smart constructor.
data LayerVersionContentInput = LayerVersionContentInput'
  { -- | For versioned objects, the version of the layer archive object to use.
    s3ObjectVersion :: Lude.Maybe Lude.Text,
    -- | The Amazon S3 key of the layer archive.
    s3Key :: Lude.Maybe Lude.Text,
    -- | The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.
    zipFile :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    -- | The Amazon S3 bucket of the layer archive.
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LayerVersionContentInput' with the minimum fields required to make a request.
--
-- * 's3ObjectVersion' - For versioned objects, the version of the layer archive object to use.
-- * 's3Key' - The Amazon S3 key of the layer archive.
-- * 'zipFile' - The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.
-- * 's3Bucket' - The Amazon S3 bucket of the layer archive.
mkLayerVersionContentInput ::
  LayerVersionContentInput
mkLayerVersionContentInput =
  LayerVersionContentInput'
    { s3ObjectVersion = Lude.Nothing,
      s3Key = Lude.Nothing,
      zipFile = Lude.Nothing,
      s3Bucket = Lude.Nothing
    }

-- | For versioned objects, the version of the layer archive object to use.
--
-- /Note:/ Consider using 's3ObjectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciS3ObjectVersion :: Lens.Lens' LayerVersionContentInput (Lude.Maybe Lude.Text)
lvciS3ObjectVersion = Lens.lens (s3ObjectVersion :: LayerVersionContentInput -> Lude.Maybe Lude.Text) (\s a -> s {s3ObjectVersion = a} :: LayerVersionContentInput)
{-# DEPRECATED lvciS3ObjectVersion "Use generic-lens or generic-optics with 's3ObjectVersion' instead." #-}

-- | The Amazon S3 key of the layer archive.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciS3Key :: Lens.Lens' LayerVersionContentInput (Lude.Maybe Lude.Text)
lvciS3Key = Lens.lens (s3Key :: LayerVersionContentInput -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: LayerVersionContentInput)
{-# DEPRECATED lvciS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciZipFile :: Lens.Lens' LayerVersionContentInput (Lude.Maybe (Lude.Sensitive Lude.Base64))
lvciZipFile = Lens.lens (zipFile :: LayerVersionContentInput -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {zipFile = a} :: LayerVersionContentInput)
{-# DEPRECATED lvciZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

-- | The Amazon S3 bucket of the layer archive.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvciS3Bucket :: Lens.Lens' LayerVersionContentInput (Lude.Maybe Lude.Text)
lvciS3Bucket = Lens.lens (s3Bucket :: LayerVersionContentInput -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: LayerVersionContentInput)
{-# DEPRECATED lvciS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.ToJSON LayerVersionContentInput where
  toJSON LayerVersionContentInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3ObjectVersion" Lude..=) Lude.<$> s3ObjectVersion,
            ("S3Key" Lude..=) Lude.<$> s3Key,
            ("ZipFile" Lude..=) Lude.<$> zipFile,
            ("S3Bucket" Lude..=) Lude.<$> s3Bucket
          ]
      )
