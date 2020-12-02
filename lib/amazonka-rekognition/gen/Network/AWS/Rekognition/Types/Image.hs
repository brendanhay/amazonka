{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Image where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.S3Object

-- | Provides the input image either as bytes or an S3 object.
--
--
-- You pass image bytes to an Amazon Rekognition API operation by using the @Bytes@ property. For example, you would use the @Bytes@ property to pass an image loaded from a local file system. Image bytes passed by using the @Bytes@ property must be base64-encoded. Your code may not need to encode image bytes if you are using an AWS SDK to call Amazon Rekognition API operations.
--
-- For more information, see Analyzing an Image Loaded from a Local File System in the Amazon Rekognition Developer Guide.
--
-- You pass images stored in an S3 bucket to an Amazon Rekognition API operation by using the @S3Object@ property. Images stored in an S3 bucket do not need to be base64-encoded.
--
-- The region for the S3 bucket containing the S3 object must match the region you use for Amazon Rekognition operations.
--
-- If you use the AWS CLI to call Amazon Rekognition operations, passing image bytes using the Bytes property is not supported. You must first upload the image to an Amazon S3 bucket and then call the operation using the S3Object property.
--
-- For Amazon Rekognition to process an S3 object, the user must have permission to access the S3 object. For more information, see Resource Based Policies in the Amazon Rekognition Developer Guide.
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iS3Object :: !(Maybe S3Object),
    _iBytes :: !(Maybe Base64)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iS3Object' - Identifies an S3 object as the image source.
--
-- * 'iBytes' - Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
image ::
  Image
image = Image' {_iS3Object = Nothing, _iBytes = Nothing}

-- | Identifies an S3 object as the image source.
iS3Object :: Lens' Image (Maybe S3Object)
iS3Object = lens _iS3Object (\s a -> s {_iS3Object = a})

-- | Blob of image bytes up to 5 MBs.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
iBytes :: Lens' Image (Maybe ByteString)
iBytes = lens _iBytes (\s a -> s {_iBytes = a}) . mapping _Base64

instance Hashable Image

instance NFData Image

instance ToJSON Image where
  toJSON Image' {..} =
    object
      ( catMaybes
          [("S3Object" .=) <$> _iS3Object, ("Bytes" .=) <$> _iBytes]
      )
