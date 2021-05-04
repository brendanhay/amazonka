{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Image where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.S3Object

-- | Provides the input image either as bytes or an S3 object.
--
-- You pass image bytes to an Amazon Rekognition API operation by using the
-- @Bytes@ property. For example, you would use the @Bytes@ property to
-- pass an image loaded from a local file system. Image bytes passed by
-- using the @Bytes@ property must be base64-encoded. Your code may not
-- need to encode image bytes if you are using an AWS SDK to call Amazon
-- Rekognition API operations.
--
-- For more information, see Analyzing an Image Loaded from a Local File
-- System in the Amazon Rekognition Developer Guide.
--
-- You pass images stored in an S3 bucket to an Amazon Rekognition API
-- operation by using the @S3Object@ property. Images stored in an S3
-- bucket do not need to be base64-encoded.
--
-- The region for the S3 bucket containing the S3 object must match the
-- region you use for Amazon Rekognition operations.
--
-- If you use the AWS CLI to call Amazon Rekognition operations, passing
-- image bytes using the Bytes property is not supported. You must first
-- upload the image to an Amazon S3 bucket and then call the operation
-- using the S3Object property.
--
-- For Amazon Rekognition to process an S3 object, the user must have
-- permission to access the S3 object. For more information, see Resource
-- Based Policies in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | Blob of image bytes up to 5 MBs.
    bytes :: Prelude.Maybe Prelude.Base64,
    -- | Identifies an S3 object as the image source.
    s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytes', 'image_bytes' - Blob of image bytes up to 5 MBs.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 's3Object', 'image_s3Object' - Identifies an S3 object as the image source.
newImage ::
  Image
newImage =
  Image'
    { bytes = Prelude.Nothing,
      s3Object = Prelude.Nothing
    }

-- | Blob of image bytes up to 5 MBs.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
image_bytes :: Lens.Lens' Image (Prelude.Maybe Prelude.ByteString)
image_bytes = Lens.lens (\Image' {bytes} -> bytes) (\s@Image' {} a -> s {bytes = a} :: Image) Prelude.. Lens.mapping Prelude._Base64

-- | Identifies an S3 object as the image source.
image_s3Object :: Lens.Lens' Image (Prelude.Maybe S3Object)
image_s3Object = Lens.lens (\Image' {s3Object} -> s3Object) (\s@Image' {} a -> s {s3Object = a} :: Image)

instance Prelude.Hashable Image

instance Prelude.NFData Image

instance Prelude.ToJSON Image where
  toJSON Image' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Bytes" Prelude..=) Prelude.<$> bytes,
            ("S3Object" Prelude..=) Prelude.<$> s3Object
          ]
      )
