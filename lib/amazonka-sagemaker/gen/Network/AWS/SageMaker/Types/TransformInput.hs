{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformInput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.SplitType
import Network.AWS.SageMaker.Types.TransformDataSource

-- | Describes the input source of a transform job and the way the transform job consumes it.
--
--
--
-- /See:/ 'transformInput' smart constructor.
data TransformInput = TransformInput'
  { _tiSplitType ::
      !(Maybe SplitType),
    _tiCompressionType :: !(Maybe CompressionType),
    _tiContentType :: !(Maybe Text),
    _tiDataSource :: !TransformDataSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiSplitType' - The method to use to split the transform job's data files into smaller batches. Splitting is necessary when the total size of each object is too large to fit in a single request. You can also use data splitting to improve performance by processing multiple concurrent mini-batches. The default value for @SplitType@ is @None@ , which indicates that input data files are not split, and request payloads contain the entire contents of an input object. Set the value of this parameter to @Line@ to split records on a newline character boundary. @SplitType@ also supports a number of record-oriented binary data formats. Currently, the supported record formats are:     * RecordIO     * TFRecord When splitting is enabled, the size of a mini-batch depends on the values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the value of @BatchStrategy@ is @MultiRecord@ , Amazon SageMaker sends the maximum number of records in each request, up to the @MaxPayloadInMB@ limit. If the value of @BatchStrategy@ is @SingleRecord@ , Amazon SageMaker sends individual records in each request.
--
-- * 'tiCompressionType' - If your transform data is compressed, specify the compression type. Amazon SageMaker automatically decompresses the data for the transform job accordingly. The default value is @None@ .
--
-- * 'tiContentType' - The multipurpose internet mail extension (MIME) type of the data. Amazon SageMaker uses the MIME type with each http call to transfer data to the transform job.
--
-- * 'tiDataSource' - Describes the location of the channel data, which is, the S3 location of the input data that the model can consume.
transformInput ::
  -- | 'tiDataSource'
  TransformDataSource ->
  TransformInput
transformInput pDataSource_ =
  TransformInput'
    { _tiSplitType = Nothing,
      _tiCompressionType = Nothing,
      _tiContentType = Nothing,
      _tiDataSource = pDataSource_
    }

-- | The method to use to split the transform job's data files into smaller batches. Splitting is necessary when the total size of each object is too large to fit in a single request. You can also use data splitting to improve performance by processing multiple concurrent mini-batches. The default value for @SplitType@ is @None@ , which indicates that input data files are not split, and request payloads contain the entire contents of an input object. Set the value of this parameter to @Line@ to split records on a newline character boundary. @SplitType@ also supports a number of record-oriented binary data formats. Currently, the supported record formats are:     * RecordIO     * TFRecord When splitting is enabled, the size of a mini-batch depends on the values of the @BatchStrategy@ and @MaxPayloadInMB@ parameters. When the value of @BatchStrategy@ is @MultiRecord@ , Amazon SageMaker sends the maximum number of records in each request, up to the @MaxPayloadInMB@ limit. If the value of @BatchStrategy@ is @SingleRecord@ , Amazon SageMaker sends individual records in each request.
tiSplitType :: Lens' TransformInput (Maybe SplitType)
tiSplitType = lens _tiSplitType (\s a -> s {_tiSplitType = a})

-- | If your transform data is compressed, specify the compression type. Amazon SageMaker automatically decompresses the data for the transform job accordingly. The default value is @None@ .
tiCompressionType :: Lens' TransformInput (Maybe CompressionType)
tiCompressionType = lens _tiCompressionType (\s a -> s {_tiCompressionType = a})

-- | The multipurpose internet mail extension (MIME) type of the data. Amazon SageMaker uses the MIME type with each http call to transfer data to the transform job.
tiContentType :: Lens' TransformInput (Maybe Text)
tiContentType = lens _tiContentType (\s a -> s {_tiContentType = a})

-- | Describes the location of the channel data, which is, the S3 location of the input data that the model can consume.
tiDataSource :: Lens' TransformInput TransformDataSource
tiDataSource = lens _tiDataSource (\s a -> s {_tiDataSource = a})

instance FromJSON TransformInput where
  parseJSON =
    withObject
      "TransformInput"
      ( \x ->
          TransformInput'
            <$> (x .:? "SplitType")
            <*> (x .:? "CompressionType")
            <*> (x .:? "ContentType")
            <*> (x .: "DataSource")
      )

instance Hashable TransformInput

instance NFData TransformInput

instance ToJSON TransformInput where
  toJSON TransformInput' {..} =
    object
      ( catMaybes
          [ ("SplitType" .=) <$> _tiSplitType,
            ("CompressionType" .=) <$> _tiCompressionType,
            ("ContentType" .=) <$> _tiContentType,
            Just ("DataSource" .= _tiDataSource)
          ]
      )
