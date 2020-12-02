{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionContentInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionContentInput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A ZIP archive that contains the contents of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . You can specify either an Amazon S3 location, or upload a layer archive directly.
--
--
--
-- /See:/ 'layerVersionContentInput' smart constructor.
data LayerVersionContentInput = LayerVersionContentInput'
  { _lvciS3ObjectVersion ::
      !(Maybe Text),
    _lvciS3Key :: !(Maybe Text),
    _lvciZipFile ::
      !(Maybe (Sensitive Base64)),
    _lvciS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'LayerVersionContentInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvciS3ObjectVersion' - For versioned objects, the version of the layer archive object to use.
--
-- * 'lvciS3Key' - The Amazon S3 key of the layer archive.
--
-- * 'lvciZipFile' - The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'lvciS3Bucket' - The Amazon S3 bucket of the layer archive.
layerVersionContentInput ::
  LayerVersionContentInput
layerVersionContentInput =
  LayerVersionContentInput'
    { _lvciS3ObjectVersion = Nothing,
      _lvciS3Key = Nothing,
      _lvciZipFile = Nothing,
      _lvciS3Bucket = Nothing
    }

-- | For versioned objects, the version of the layer archive object to use.
lvciS3ObjectVersion :: Lens' LayerVersionContentInput (Maybe Text)
lvciS3ObjectVersion = lens _lvciS3ObjectVersion (\s a -> s {_lvciS3ObjectVersion = a})

-- | The Amazon S3 key of the layer archive.
lvciS3Key :: Lens' LayerVersionContentInput (Maybe Text)
lvciS3Key = lens _lvciS3Key (\s a -> s {_lvciS3Key = a})

-- | The base64-encoded contents of the layer archive. AWS SDK and AWS CLI clients handle the encoding for you.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
lvciZipFile :: Lens' LayerVersionContentInput (Maybe ByteString)
lvciZipFile = lens _lvciZipFile (\s a -> s {_lvciZipFile = a}) . mapping (_Sensitive . _Base64)

-- | The Amazon S3 bucket of the layer archive.
lvciS3Bucket :: Lens' LayerVersionContentInput (Maybe Text)
lvciS3Bucket = lens _lvciS3Bucket (\s a -> s {_lvciS3Bucket = a})

instance Hashable LayerVersionContentInput

instance NFData LayerVersionContentInput

instance ToJSON LayerVersionContentInput where
  toJSON LayerVersionContentInput' {..} =
    object
      ( catMaybes
          [ ("S3ObjectVersion" .=) <$> _lvciS3ObjectVersion,
            ("S3Key" .=) <$> _lvciS3Key,
            ("ZipFile" .=) <$> _lvciZipFile,
            ("S3Bucket" .=) <$> _lvciS3Bucket
          ]
      )
